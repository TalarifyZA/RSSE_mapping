#https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html
# LOAD PACKAGES ----

library(pacman)
pacman::p_load(shinythemes,readr, here, shiny, rgdal,
               leaflet, DT, dplyr, countrycode)


Site_Name <-sample(c('a','b','c'),replace=T,5)
Latitude <-runif(5,min=-26, max=-22)
Longitude<-runif(5,min=-54, max=-48)
Sites <-data.frame(Site_Name,Latitude,Longitude)



ui <- fluidPage(
  theme = shinytheme("cerulean"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("sites",
                     "Site Name",choices= Sites$Site_Name,
                     options= list(maxItems = 2)),
    ),
      mainPanel(
        tabsetPanel(
          tabPanel("Plots",leafletOutput("Station"))
        )
      )
  )
)



server <- function(input, output, session){
  
  df1 <- eventReactive(input$sites, {
    Sites %>% dplyr::filter(Site_Name %in% input$sites)
  })
  
  output$Station = renderLeaflet({
    leaflet(data = df1()) %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addMarkers(Sites$Longitude, Sites$Latitude, popup= input$sites,
                 icon = list(
                   iconUrl = 'https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png',
                   iconSize = c(13, 20)))
  })
}

shinyApp(ui = ui, server = server)