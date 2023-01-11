#https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html

library(shiny)
library(rgdal)
library(leaflet)
library(DT)
data <- read.csv("data_table.csv")
# ui object
ui <- fluidPage(
  titlePanel(p("RSE communities of practice in Africa", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "variableselected",
        label = "Select Country",
        choices = c("South Africa", "Botswana", "Tunisia", "Tanzania", "Kenya", "Egypt", "Eswatini",
                    "Zambia", "Cameroon", "Côte d’Ivoire", "Democratic Republic of Congo", "Mali", "Niger", "Nigeria",
                    "Ethiopia", "Lesotho", "Malawi", "Rwanda", "Somalia", "Senegal", "Ghana", "Gambia", "Algeria",
                    "Angola", "Mauritius", "Mozambique", "Namibia", "Seychelles", "Zimbabwe", "Tanzania")
      )
    ),
    mainPanel(leafletOutput(outputId = "map"),
              DTOutput(outputId = "table"))
  )
)

# server()
server <- function(input, output){
  
  output$table <- renderDT(data)
  
  output$map <- renderLeaflet({
    
    rse_data <- read.csv("mapping_rse.csv")
    head(rse_data)
    
    mymap1 <- leaflet(data = rse_data) %>%
      setView(lat = 0, lng= 25, zoom = 3) %>%  # set map view
      addTiles() %>%                                       # Add default OpenStreetMap map background tiles
      addMarkers(lng = ~Long,lat = ~Lat, popup = ~as.character(Institution), label = ~as.character(Institution))
    
    mymap1

  })
}

# shinyApp()
shinyApp(ui = ui, server = server)