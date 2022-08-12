
library(shiny)
library(sf)
library(tidyverse)
library(ggspatial)
library(gt)
library(shinythemes)
library(leaflet)
library(leafpop)
library(leaflet.extras)
library(scales)
library(markdown)

APE <- read_csv("https://raw.githubusercontent.com/Deitsch-John/Ash-Protection-Experiment/main/APEupdated.csv", skip = 1)
trees <- read_csv("https://raw.githubusercontent.com/Deitsch-John/Ash-Protection-Experiment/main/treeLog.csv")

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
watersheds <- read_sf(layer = "hbef_wsheds", dsn = ".")

hbef.watersheds <- watersheds %>%
  dplyr::filter(WS %in% c("WS3", "WS2", "WS1", "WS4", "WS6", "WS5", "WS8", "WS7", "WS9"))%>%
  select(WS)%>%
  mutate(Watershed = case_when(
    WS == "WS1"~"1",
    WS == "WS2"~"2",
    WS == "WS3"~"3",
    WS == "WS4"~"4",
    WS == "WS5"~"5",
    WS == "WS6"~"6",
    WS == "WS7"~"7",
    WS == "WS8"~"8",
    WS == "WS9"~"9",
  ))

plotgrid.small <- plotgrid %>%
  mutate(testing = grepl("C", alpha, fixed = TRUE))%>%
  filter(testing==TRUE)%>%
  filter(as.numeric(numeric)<101 & as.numeric(numeric)>65)%>%
  mutate(testing2 = grepl('Cu|Cv|Cw|Cx|Cy|Cz', alpha))%>%
  mutate(testing3 = case_when(
    testing2==TRUE & as.numeric(numeric)<77~"remove",
    testing2==TRUE & as.numeric(numeric)>77~"no",
    testing2==FALSE~"no"
  ))%>%
  filter(testing3=="no")%>%
  dplyr::select(-testing, -testing2, -testing3)

birdplot.outline <- st_union(plotgrid.small$geometry)
birdplot.outline.p <- st_transform(birdplot.outline, 26919)

birdplot.outline.rough <- st_simplify(birdplot.outline.p, preserveTopology = TRUE, dTolerance = 100)

st_crs(hbef.streams)=26919


APE_cleaned <- APE %>%
  dplyr::select(`Plot Number`, Treatment, Latitude, Longitude,
                `Number of ash to be treated`)%>%
  rename(Trees = `Number of ash to be treated`)%>%
  rowwise()%>%
  mutate(coords = list(st_point(c(Longitude, Latitude))))

APE_sfx <- obs_to_coords(APE_cleaned, coords, 4269)
APE_sf <- st_transform(APE_sfx, 26919)

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

plots.leaf <- st_transform(APE_sf, 4326)

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
  df2 <- df %>%
    filter(!is.na(Canopy_Health))
  
  ggplot(df2, aes(`Canopy class`, `DBH (cm)`/2.54))+
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
    filter(!is.na(`Canopy class`)) %>%
    rename(`Canopy Class`=`Canopy class`)%>%
    count(`Canopy Class`, .drop=F, name = "Trees")%>%
    pull(Trees)%>%
    max()
  
  df %>%
    filter(!is.na(`Canopy class`)) %>%
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
    filter(!is.na(Canopy_Health)) %>%
    rename(`Canopy Health`=Canopy_Health)%>%
    count(`Canopy Health`, .drop=F, name = "Trees")%>%
    pull(Trees)%>%
    max()
  
  df %>%
    select(`Tree Number`, Canopy_Health)%>%
    filter(!is.na(Canopy_Health)) %>%
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
    filter(!is.na(`Canopy class`)) %>%
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
Large_map_lf <- function(df, water, bird, topo){
  
  boundary.leaf <- st_transform(hbef.boundary, 4326)
  elevation.leafx <- st_transform(hbef.elevation40, 4326)
  elevation.leaf <- elevation.leafx[boundary.leaf,]
  roads.leafx <- st_transform(hbef.roads, 4326)
  roads.leaf <- roads.leafx[boundary.leaf,]
  streams.leafx <- st_transform(hbef.streams, 4326)
  streams.leaf <- streams.leafx[boundary.leaf,]
  watersheds.leaf <- st_transform(hbef.watersheds, 4326)
  birdplot.outline.rough.leaf <- st_transform(birdplot.outline.rough, 4326)
  
  popT <- df %>%
    left_join(PlotStats, by = "Plot Number")%>%
    select(`Plot Number`, Treatment, Trees.y, `Max DBH`, 
           `Min DBH`, `Mean DBH`)%>%
    rename(`Number of Trees` = Trees.y)%>%
    st_drop_geometry()%>%
    popupTable(feature.id = FALSE,
               row.numbers = FALSE)
  
  pal = 
    colorFactor(palette = c("gray0", 
                            "firebrick2"), domain = df$Treatment)
  
  pal2 =
    colorFactor(palette = "Paired", domain = watersheds.leaf$Watershed)
  
  if (water == "Yes") {
    water.fill = 0.7
  } else if (water == "No") {
    water.fill = 0.0
  } else {
    print("Please provide Yes or No to water argument")
  }
  
  if (water == "Yes") {
    water.weight = 1.8
  } else if (water == "No") {
    water.weight = 0.0
  } else {
    print("Please provide Yes or No to water argument")
  }
  
  if (bird == "Yes") {
    bird.fill = 0.2
  } else if (bird == "No") {
    bird.fill = 0.0
  } else {
    print("Please provide Yes or No to bird argument")
  }
  
  if (bird == "Yes") {
    bird.weight = 1.8
  } else if (bird == "No") {
    bird.weight = 0.0
  } else {
    print("Please provide Yes or No to bird argument")
  }
  
  if (topo == "Yes"){
    topo.weight = 0.8
  } else if (topo == "No"){
    topo.weight = 0.0
  } else {
    print("Please provide Yes or No to  argutopoment")
  }
  
  leaflet()%>%
    addPolylines(data = elevation.leaf, color = "olivedrab",
                 fillOpacity = 0.2, weight = topo.weight)%>%
    addPolygons(data = watersheds.leaf,
                color = "Black",
                weight = water.weight,
                fillOpacity = water.fill, 
                fillColor = pal2(watersheds.leaf$Watershed),
                popup = paste("Watershed", watersheds.leaf$Watershed))%>%
    addPolygons(data = birdplot.outline.rough.leaf,
                color = "Black",
                weight = bird.weight,
                fillOpacity = bird.fill,
                fillColor = "Blue")%>%
    addPolylines(data = boundary.leaf, 
                 color = "Black",
                 weight = 2)%>%
    addPolylines(data = streams.leaf, 
                 color = "navy", 
                 weight = 1.9)%>%
    addPolylines(data = roads.leaf, 
                 color = "Black",
                 weight = 1.9)%>%
    addCircles(lng = df$Longitude,
               lat = df$Latitude,
               popup = popT,
               color = pal(df$Treatment),
               fillColor = pal(df$Treatment),
               radius = 20,
               fill = TRUE,
               fillOpacity = 1)%>%
    setMapWidgetStyle(list(background="white"))%>%
    addScaleBar(position = c("topright"))
  
  
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Ash Protection Experiment"),
  tabsetPanel(
    tabPanel("Map of Study Site",
             sidebarPanel(
               selectInput(inputId = "ws", label = "Overlay Watershed Boundaries?",
                           choices = c("Yes", "No")),
               selectInput(inputId = "birds", label = "Overlay Bird Plot Boundary?",
                           choices = c("Yes", "No")),
               selectInput(inputId = "topo", label = "Show 40 m contours?",
                           choices = c("Yes", "No")),
               width = 4
             ),
             mainPanel(
               leafletOutput("Map", height=900),
               width = 8
             )
    ),
    tabPanel("Explore by Plot",
             fluidRow(column(5,
                             selectInput(inputId = "Plot", label = "Choose Plot", choices = PlotsList),
                             gt_output("Tree_List"),
             ),
             column(7,
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
    ),
    tabPanel("Project Information",
             fluidRow(column(6,
                             includeMarkdown("for_APE_shiny.Rmd")
             ),
             column(6, imageOutput("Ash"))
             )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$Map <- renderLeaflet(Large_map_lf(plots.leaf, input$ws, input$birds, input$topo))
  output$Tree_List <- render_gt(TreeTable(input$Plot))
  output$Class <- renderPlot(CanopyClass(input$Plot))
  output$Health <- renderPlot(CanopyHealth(input$Plot))
  output$DBH <- renderPlot(DBH_by_Class(TreeLog))
  output$Health_all <- renderPlot(CanopyHealth_all(TreeLog))
  output$Class_all <- renderPlot(CanopyClass_all(TreeLog))
  output$DBH_hist <- renderPlot(DBH_hist(TreeLog))
  output$Ash <- renderImage({
    return(list(
      src="Ash.jpg",
      contentType="image/jpeg",
      width = 552.78, 
      height=743.84
    ))
  }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

# leaflet()%>%
#   addPolygons(data = watersheds.leaf,
#               color = "Black",
#               weight = 1.8,
#               fillOpacity = 0.7,
#               fillColor = pal2(watersheds.leaf$Watershed),
#               popup = paste("Watershed", watersheds.leaf$Watershed))%>%
#   addLabelOnlyMarkers(data = centers,
#                       label = ~WS,
#                       labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE))
# 
# centers <- st_centroid(watersheds.leaf)
# watersheds.leaf.nogeom <- st_drop_geometry(watersheds.leaf)
# watersheds.leaf2 <- bind_cols(watersheds.leaf.nogeom, centers)
# 
#   geom_sf(data = centers)


