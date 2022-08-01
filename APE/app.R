
library(shiny)
library(sf)
library(tidyverse)
library(ggspatial)
library(gt)
library(googlesheets4)
library(shinythemes)
library(leaflet)
library(leafpop)
library(leaflet.extras)
library(scales)

# setwd("C:/Users/jfdei/OneDrive/Desktop/Work/Hubbard Brook 2022/APE")
APE <- read_csv("https://raw.githubusercontent.com/Deitsch-John/Ash-Protection-Experiment/main/APE.csv")
trees <- read_csv("https://raw.githubusercontent.com/Deitsch-John/Ash-Protection-Experiment/main/treeLog.csv")
# setwd("C:/Users/jfdei/OneDrive/Desktop/qGIS/shapefile.library")

obs_to_coords <- function(df, coord_col, crs_add){
  coords.sfg <- df$coords
  coords.sfc <- st_sfc(coords.sfg, crs = crs_add)
  
  df %>%
    st_as_sf(geometry = coords.sfc)%>%
    st_transform(., crs = crs_add)
  
}

#load HB shape-files 
# using CRS EPSG:26919
url = "https://github.com/Deitsch-John/Ash-Protection-Experiment/raw/main/spatial.zip.zip"
download.file(url, "spatial.zip.zip")
unzip("spatial.zip.zip")

hbef.boundary = read_sf(layer = "hbef_boundary", dsn = ".")
hbef.roads = read_sf(layer = "hbef_roads", dsn = ".")
hbef.streams = read_sf(layer = "hbef_streams", dsn = ".")
hbef.elevation40 = read_sf(layer = "hbef_elev40m", dsn = ".")
plotgrid <- read_sf(layer = "virtual_grid_megamain", dsn = ".")

birdplot.outline <- st_union(plotgrid$geometry)
birdplot.outline <- st_transform(birdplot.outline, 26919)
bp <- st_as_sfc(st_bbox(birdplot.outline))

st_crs(hbef.streams)=26919

APE2 <- APE[1:68, ]
APE_cleaned <- APE2 %>%
  dplyr::select(Number, Name.Matt, Latitude, Longitude, `Within bird plots?`,
                `EAB signs in Nov '21?`, `EAB protection?`,
                `Number of trees to treat`)%>%
  mutate(Treatment = case_when(
    `EAB protection?`=="yes"~"Treated",
    `EAB protection?`=="no"~"Control",
    is.na(`EAB protection?`)=="TRUE"~"Control"
  ))%>%
  dplyr::select(-`EAB protection?`)%>%
  rename(Birdplot = `Within bird plots?`,
         Trees = `Number of trees to treat`,
         Infested_Nov2021 = `EAB signs in Nov '21?`)%>%
  rowwise()%>%
  mutate(coords = list(st_point(c(Longitude, Latitude))))

APE_sf <- obs_to_coords(APE_cleaned, coords, 4269)
APE_sf <- st_transform(APE_sf, 26919)

# rawdata <- read_sheet("https://docs.google.com/spreadsheets/d/13YHy2xUOoZYtnc05kCjSthoFdoqDUFWkB_ANJnfYwV0/edit#gid=0",
#                    col_types = "nnDnnnncin")
# 
# rawdata2 <- rawdata[2:304,]

TreeLog <- trees %>%
  dplyr::select(`Tree Number`, `Plot Number`, `DBH (inches)`, `Canopy class`, `Canopy Health`, `DBH (cm)`)%>%
  # filter(`Canopy Health` != "NULL")%>%
  mutate(Canopy_Healthx = unlist(`Canopy Health`))%>%
  mutate(Canopy_Health = case_when(
    Canopy_Healthx==1~"Healthy",
    Canopy_Healthx==2~"Some Thinning, No Dieback",
    Canopy_Healthx==3~"Canopy Dieback <50%",
    Canopy_Healthx==4~"Canopy Dieback >50%",
    Canopy_Healthx==5~"Dead Canopy"
  ))

TreeLog$`Canopy class` <- factor(TreeLog$`Canopy class`, levels = c("Supressed", "Intermediate", "Codominant", "Dominant"))  
TreeLog$Canopy_Health <- factor(TreeLog$Canopy_Health, levels = c("Healthy", "Some Thinning, No Dieback",
                                                                  "Canopy Dieback <50%", "Canopy Dieback >50%",
                                                                  "Dead Canopy"))

PlotsList <- TreeLog %>%
  select(`Plot Number`)%>%
  unique()%>%
  pull()

PlotStats <- trees %>%
  group_by(`Plot Number`)%>%
  summarise(Trees = n(),
            `Max DBH` = max(`DBH (inches)`),
            `Min DBH` = min(`DBH (inches)`),
            `Mean DBH` = mean(`DBH (inches)`),
            `Total DBH` = sum(`DBH (inches)`),
            Plugs = sum(`Drill Sites`),
            Liters = sum(`mL solution`)/1000)

plots.leaf <- st_transform(APE_sf, 4326)%>%
  rename('Plot Number' = Number)

TreeTable <- function(plot){
  TreeLog %>%
    filter(`Plot Number`==plot)%>%
    select(`Tree Number`, `DBH (inches)`, `Canopy class`, Canopy_Health)%>%
    arrange(`Tree Number`)%>%
    rename(`Canopy Class`=`Canopy class`,
           `Canopy Health`=Canopy_Health)%>%
    gt()
}
CanopyClass <- function(plot){
  
  lim <- TreeLog %>%
    filter(`Plot Number`==plot)%>%
    select(`Tree Number`, `Canopy class`)%>%
    rename(`Canopy Class`=`Canopy class`)%>%
    count(`Canopy Class`, .drop=F, name = "Trees")%>%
    pull(Trees)%>%
    max()
  
  TreeLog %>%
    filter(`Plot Number`==plot)%>%
    select(`Tree Number`, `Canopy class`)%>%
    rename(`Canopy Class`=`Canopy class`)%>%
    count(`Canopy Class`, .drop=F, name = "Trees")%>%
  ggplot(aes(x = `Canopy Class`, y = Trees))+
    geom_col(fill = "ForestGreen")+
    geom_text(aes(label=str_c(Trees, ifelse(Trees==1, "Tree", "Trees"), sep = " ")), hjust = -1)+
    scale_y_continuous(limits = c(0,lim+10), breaks = seq(0,lim+10, 10))+
    theme_classic()+
    theme(axis.text = element_text(size = 12),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          axis.ticks.x = element_blank())+
    coord_flip()
}
CanopyHealth <- function(plot){
  
  lim <- TreeLog %>%
    filter(`Plot Number`==plot)%>%
    select(`Tree Number`, Canopy_Health)%>%
    rename(`Canopy Health`=Canopy_Health)%>%
    count(`Canopy Health`, .drop=F, name = "Trees")%>%
    pull(Trees)%>%
    max()
  
  TreeLog %>%
    filter(`Plot Number`==plot)%>%
    select(`Tree Number`, Canopy_Health)%>%
    rename(`Canopy Health`=Canopy_Health)%>%
    count(`Canopy Health`, .drop=F, name = "Trees")%>%
    ggplot(aes(x = `Canopy Health`, y = Trees))+
    geom_col(fill = "ForestGreen")+
    geom_text(aes(label=str_c(Trees, ifelse(Trees==1, "Tree", "Trees"), sep = " ")), hjust = -1)+
    scale_y_continuous(limits = c(0,lim+10), breaks = seq(0,lim+10, 10))+
    theme_classic()+
    theme(axis.text = element_text(size = 12),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          axis.ticks.x = element_blank())+
    coord_flip()
}
DBH_by_Class <- function(df){
  ggplot(df, aes(`Canopy class`, `DBH (cm)`/2.54))+
    geom_boxplot(outlier.fill = NA, outlier.color = NA)+
    geom_jitter(alpha = 0.4, height = 0, 
                width = 0.1, color = "ForestGreen", size = 1.5)+
    theme_classic()+
    labs(y = "Diameter (inches)")+
    theme(axis.text = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = 14))
}
Large_map <- function(df){
  map <- ggplot()+
    geom_sf(data = hbef.boundary, 
            fill = "white")+
    geom_sf(data = hbef.elevation10,
            alpha = 0.1, size = 0.3)+
    geom_sf(data = birdplot.outline, 
            fill = NA)+
    geom_sf(data = hbef.streams, 
            size = 0.6, 
            color = "steelblue2",
            alpha = 0.6)+
    geom_sf(data = hbef.roads, 
            size = 0.6, 
            color = "black")+
    geom_sf(data = df,
            aes(color = Treatment, shape = Treatment))+
    theme(
      panel.background = element_rect(fill = "darkseagreen1"),
      panel.border = element_rect(fill = NA, size = 1),
      legend.position = "bottom",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank())+
    annotation_scale(location="br")+
    annotation_north_arrow(which_north = "true",
                           style = north_arrow_nautical,
                           location = "bl")
  
  return(map)
}
CanopyClass_all <- function(df){
  
  lim <- TreeLog %>%
    select(`Tree Number`, `Canopy class`)%>%
    rename(`Canopy Class`=`Canopy class`)%>%
    count(`Canopy Class`, .drop=F, name = "Trees")%>%
    pull(Trees)%>%
    max()
  
  df %>%
    select(`Tree Number`, `Canopy class`)%>%
    rename(`Canopy Class`=`Canopy class`)%>%
    count(`Canopy Class`, .drop=F, name = "Trees")%>%
    ggplot(aes(x = `Canopy Class`, y = Trees))+
    geom_col(fill = "ForestGreen")+
    geom_text(aes(label=str_c(Trees, ifelse(Trees==1, "Tree", "Trees"), sep = " ")), hjust = -0.8)+
    scale_y_continuous(limits = c(0,lim+10), breaks = seq(0,lim+10, 10))+
    theme_classic()+
    theme(axis.text = element_text(size = 12),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          axis.ticks.x = element_blank())+
    coord_flip()
}
CanopyHealth_all <- function(df){
  
  lim <- TreeLog %>%
    select(`Tree Number`, Canopy_Health)%>%
    rename(`Canopy Health`=Canopy_Health)%>%
    count(`Canopy Health`, .drop=F, name = "Trees")%>%
    pull(Trees)%>%
    max()
  
  df %>%
    select(`Tree Number`, Canopy_Health)%>%
    rename(`Canopy Health`=Canopy_Health)%>%
    count(`Canopy Health`, .drop=F, name = "Trees")%>%
    ggplot(aes(x = `Canopy Health`, y = Trees))+
    geom_col(fill = "ForestGreen")+
    geom_text(aes(label=str_c(Trees, ifelse(Trees==1, "Tree", "Trees"), sep = " ")), hjust = -0.8)+
    scale_y_continuous(limits = c(0,lim+10), breaks = seq(0,lim+10, 10))+
    theme_classic()+
    theme(axis.text = element_text(size = 12),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          axis.ticks.x = element_blank())+
    coord_flip()+
    scale_x_discrete(labels = wrap_format(10))
}
DBH_hist <- function(df){
  Mean <- TreeLog%>%
    filter(!is.na(`DBH (cm)`))%>%
    select(`DBH (cm)`)%>%
    mutate(inch = `DBH (cm)`/2.54)%>%
    pull()%>%
    mean()
  
  Mean.l <- toString(Mean)
  Mean.l2 <- strsplit(Mean.l, "")
  Mean.l3 <- Mean.l2[[1]][1:5]
  Mean.l4 <- str_c(Mean.l3, collapse = "")

  df%>%
    filter(!is.na(`DBH (cm)`))%>%
    ggplot(aes(`DBH (cm)`/2.54))+
    geom_histogram(binwidth = 1, color = "White", 
                   fill = "ForestGreen", aes(y = ..density..))+
    geom_density(color = "saddlebrown", size = 1.1)+
    geom_vline(aes(xintercept = mean(`DBH (cm)`/2.54)), 
               color = "Red", size = 1.1)+
    geom_text(aes(13, 0.11), 
               label = str_c("Mean DBH: ", Mean.l4, 
                             sep = ""))+
    theme_classic()+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14))+
    labs(y = "Trees", x = "DBH (inches)")+
    scale_x_continuous(limits = c(0,35), breaks = seq(0,35,5))
}
Large_map_lf <- function(df){
  
  boundary.leaf <- st_transform(hbef.boundary, 4326)
  elevation.leafx <- st_transform(hbef.elevation40, 4326)
  elevation.leaf <- elevation.leafx[boundary.leaf,]
  roads.leafx <- st_transform(hbef.roads, 4326)
  roads.leaf <- roads.leafx[boundary.leaf,]
  streams.leafx <- st_transform(hbef.streams, 4326)
  streams.leaf <- streams.leafx[boundary.leaf,]
  
  popT <- df %>%
    left_join(PlotStats, by = "Plot Number")%>%
    select(Trees.y, `Plot No`, Treatment, `Max DBH`, 
           `Min DBH`, `Mean DBH`)%>%
    rename(`Number of Trees` = Trees.y)%>%
    st_drop_geometry()%>%
    popupTable()
  
  leaflet()%>%
    addPolylines(data = elevation.leaf, color = "Green",
                 fillOpacity = 0.2, weight = 0.8)%>%
    addCircles(lng = df$Longitude,
               lat = df$Latitude,
               popup = popT,
               color = "Black",
               fillColor = "Red",
               radius = 20,
               fill = TRUE,
               fillOpacity = 1)%>%
    addPolylines(data = boundary.leaf, 
                 color = "Black",
                 weight = 1.7)%>%
    addPolylines(data = streams.leaf, 
                 color = "Blue", 
                 weight = 1.7)%>%
    addPolylines(data = roads.leaf, 
                 color = "Black",
                 weight = 1.7)%>%
    setMapWidgetStyle(list(background="white"))%>%
    addScaleBar(position = c("topright"))
  
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Ash Protection Experiment"),
  tabsetPanel(
    tabPanel("Map of Study Site",
             leafletOutput("Map")
    ),
    tabPanel("Explore by Plot",
             fluidRow(column(4,
                             selectInput(inputId = "Plot", label = "Choose Plot", choices = PlotsList),
                             gt_output("Tree_List"),
             ),
             column(8,
                    plotOutput("Class"),
                    plotOutput("Health")
             )
             )
    ),
    tabPanel("Tree Statistics",
             fluidRow(column(6,
                             plotOutput("Health_all"),
                             plotOutput("Class_all")
             ),
             column(6,
                    plotOutput("DBH_hist"),
                    plotOutput("DBH"),
             )
             )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$Map <- renderLeaflet(Large_map_lf(plots.leaf))
  output$Tree_List <- render_gt(TreeTable(input$Plot))
  output$Class <- renderPlot(CanopyClass(input$Plot))
  output$Health <- renderPlot(CanopyHealth(input$Plot))
  output$DBH <- renderPlot(DBH_by_Class(TreeLog))
  output$Health_all <- renderPlot(CanopyHealth_all(TreeLog))
  output$Class_all <- renderPlot(CanopyClass_all(TreeLog))
  output$DBH_hist <- renderPlot(DBH_hist(TreeLog))
}

# Run the application 
shinyApp(ui = ui, server = server)


