#https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html
# LOAD PACKAGES ----

library(pacman)
pacman::p_load(readr, here, shiny, rgdal,
               leaflet, DT, dplyr, countrycode)


# LOAD DATA ----

# Read data from Google Sheets

mapping <- readr::read_csv(here("data", "rsse_africa_mapping_2022.csv"))
# Load country coordinates

coords <- readr::read_csv("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv")


# Add country iso2 code for geolocation

clean_mapping <- mapping %>%
  mutate(Mapping_location = case_when(Country == "Multiple countries" ~ "Zambia",
                                      TRUE ~ as.character(Country))) %>% 
  mutate(country_iso = countrycode(Mapping_location, origin = "country.name", destination = "iso3c")) %>% 
  relocate(country_iso, .after = "Mapping_location")


# Add country coordinates

mapping_w_coords <- clean_mapping %>% 
  left_join(coords, by = c("country_iso" = "Alpha-3 code")) %>% 
  select(-c(`Country.y`, `Alpha-2 code`, `Numeric code`)) %>% 
  rename(Country = Country.x, Latitude = `Latitude (average)`, Longitude = `Longitude (average)` )

## SAVE DATA FOR RE-USE IN SHINY

readr::write_csv(mapping_w_coords, here("data", "mapping_w_coords.csv"))


# SHINY APP UI ----

ui <- fluidPage(
  titlePanel(p("Research Software Initatives in Africa", style = "color:#3474A7")),
  leafletOutput(outputId = "map"),
              DTOutput(outputId = "table")
)

# SHINY APP SERVER ----

server <- function(input, output){
  
  output$table <- renderDT(mapping_w_coords)
  
  output$map <- renderLeaflet({
    
    rse_map <- leaflet(data = mapping_w_coords) %>%
      setView(lat = 0, lng= 25, zoom = 3) %>%  # set map view
      addTiles() %>%                                       # Add default OpenStreetMap map background tiles
      addAwesomeMarkers(lng=~Longitude, lat=~Latitude,
                        popup=~paste("Type:", Type),
                        label=~paste0(Name, "(", Country, ")"),
                        clusterOptions = markerClusterOptions())

    rse_map

  })
}

# RUN SHINY APP ----
shinyApp(ui = ui, server = server)