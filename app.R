# LOAD PACKAGES ----

library(pacman)

pacman::p_load(shinythemes,readr, here, shiny, rgdal,
               leaflet, DT, dplyr, countrycode)

# FUNCTION TO DISPLAY RMD AS TAB PAGE ----
## function to render .Rmd files to html - does not embed image or add css
## From https://github.com/vnijs/shiny-site

encoding <- getOption("shiny.site.encoding", default = "UTF-8")


inclRmd <- function(path, r_env = parent.frame()) {
  paste(
    readLines(path, warn = FALSE, encoding = encoding),
    collapse = '\n'
  ) %>%
    knitr::knit2html(
      text = .,
      fragment.only = TRUE,
      envir = r_env,
      options = "",
      stylesheet = "",
      encoding = encoding
    ) %>%
    gsub("&lt;!--/html_preserve--&gt;","",.) %>%  ## knitr adds this
    gsub("&lt;!--html_preserve--&gt;","",.) %>%   ## knitr adds this
    HTML
}


# LOAD DATA ----

## The CSV of the mapping data is shared via Zenodo at https://doi.org/10.5281/zenodo.7594453
## Read the downloaded CSV file

mapping <- read_csv(here("data", "rsse_africa_mapping_2022.csv"))

# Augment data ----

# Add country iso3 code for geolocation

clean_mapping <- mapping %>%
  ## Make all initiatives that are available in multiple countries map to Zambia as an arbitrary position
  mutate(Mapping_location = case_when(Country == "Multiple countries" ~ "Zambia",
                                      TRUE ~ as.character(Country))) %>% 
  ## Use package {countrycode} to add a new column with iso3 code for each country
  mutate(country_iso = countrycode(Mapping_location, origin = "country.name", destination = "iso3c")) %>% 
  ## Move columns to make table neat
  relocate(country_iso, .after = "Mapping_location") %>% 
  ## Convert categorical data from character class to factor
  mutate(Type = as.factor(Type), Collector =as.factor(Collector), Region = as.factor(Region), Country = as.factor(Country), Data_origin = as.factor(Data_origin))


# Add country coordinates

mapping_w_coords <- clean_mapping %>% 
  left_join(coords, by = c("country_iso" = "Alpha-3 code")) %>% 
  select(-c(`Country.y`, `Alpha-2 code`, `Numeric code`)) %>% 
  rename(Country = Country.x, Latitude = `Latitude (average)`, Longitude = `Longitude (average)` )


## SAVE DATA FOR RE-USE IN SHINY ----

readr::write_csv(mapping_w_coords, here("data", "mapping_w_coords.csv"))




# SHINY APP UI ----


ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
  
  titlePanel("Research Software Initatives in Africa"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "initiative_type",
                  label = "Which types of initiatives would you like to view? (multiple selection possible)",
                  choices = c("All", as.character(mapping_w_coords$Type)),
                  multiple = TRUE,
                  selected = "All")
    ),
                     
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map",width = "100%", height = 800)),
        tabPanel("Table", DTOutput(outputId = "table")),
        tabPanel("About", uiOutput("about")
        )
        )
    )
  )
)  
      
  
# SHINY APP SERVER ----

server <- function(input, output, session){
  
  filter_map_data_react <- reactive({
    mapping_w_coords %>% 
      dplyr::filter(Type %in% input$initiative_type)    
  })
  
  table_data_react <- reactive({
    mapping %>% 
      dplyr::filter(Type %in% input$initiative_type)    
  })
  
  
  output$map <- renderLeaflet({
    
    filter_map_data <- filter_map_data_react()

    if ("All" %in% input$initiative_type){
      leaflet(mapping_w_coords) %>% 
        addTiles() %>%
        setView(20,1, zoom = 4) %>% 
        addAwesomeMarkers(lng=~Longitude, lat=~Latitude,
                          popup=~paste("Type:", Type),
                          label=~paste0(Name, "(", Country, ")"),
                          clusterOptions = markerClusterOptions())
    }else{
      leaflet(filter_map_data) %>% 
        setView(20,1, zoom = 4) %>% 
        addTiles() %>%
        addAwesomeMarkers(lng=~Longitude, lat=~Latitude,
                          popup=~paste("Type:", Type),
                          label=~paste0(Name, "(", Country, ")"),
                          clusterOptions = markerClusterOptions())
    }
  })
  
  
  output$table <- renderDT({
  
    table_data <- table_data_react()
    
    if ("All" %in% input$initiative_type){
      mapping
    } else{
      table_data
    }
  })
  
  output$about <- renderUI({
    inclRmd("./about.Rmd")
  })
}

# RUN SHINY APP ----
shinyApp(ui = ui, server = server)