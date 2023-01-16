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
  
  
  tabsetPanel(type = "tabs",
              tabPanel("Map",  leafletOutput(outputId = "map")),
              tabPanel("Table", DTOutput(outputId = "table")),
              tabPanel("About", 
                   HTML(r"(
                        <p>This app was developed by Nomalungelo Maphanga and Anelda van der Walt from <a href=\"https://talarify.co.za\">Talarify</a> 
                        as part of a project to create more awareness about the breadth of interest and expertise in research software in Africa.
                       </p>
                       <p>Data included in this visualisation were sourced as follows:
                       <ul>
                        <li> The Research Software Alliance <a href=\"https://www.researchsoft.org/\">(ReSA)</a> recruited 
                        contracters to perform a mapping of research software initiatives in the Global South in 2021/2022;</li>
                        <li> Talarify built on the work by ReSA in 2022/2023 through web searches, outreach to their networks, 
                              and outreach at the World Science Forum to collect more data.</li>
                      </ul>
                     </p>
                     <p>The additional mapping and development of the Shiny app was run as part of Cohort 6 of the Open Life 
                     Science Mentoring and Training Programme <a href=\"https://openlifesci.org/\">(OLS)</a>.
                     </p>
                     
                     <p>For more information about the ReSA mapping, please <a href=\"https://www.researchsoft.org/blog/2022-10/\">read the 
                     blog post</a> by Michelle Barker.
                     </p>
                     
                     <p>
                     The additional mapping is a work in progress and is currently available in 
                     <a href=\"https://docs.google.com/spreadsheets/d/18FSidlJ4o1AOwz7lVoy2A8iWDxiADHMZ4sWMEIisYJA/edit#gid=0\">this Google Sheet</a>.
                     Please address any questions about this work to <a href=\"mailto:anelda@talarify.co.za\">Anelda van der Walt</a>. 
                     The project was funded by Talarify.
                     </p>
                     )"
                   )
                   )
              )
              
)

# SHINY APP SERVER ----

server <- function(input, output){
  
  output$table <- renderDT(mapping)
  
  
  output$map <- renderLeaflet({
    
    rse_map <- leaflet(data = mapping_w_coords) %>%
      setView(lat = 0, lng= 25, zoom = 3) %>%  # set map view
      addTiles() %>%                                       # Add default OpenStreetMap map background tiles
      addAwesomeMarkers(lng=~Longitude, lat=~Latitude,
                        popup=~paste("Type:", Type),
                        label=~paste0(Name, "(", Country, ")"),
                        clusterOptions = markerClusterOptions()
                        )
  
    rse_map

  })
}

# RUN SHINY APP ----
shinyApp(ui = ui, server = server)