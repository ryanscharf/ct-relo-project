
library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(DT)
library(leafgl)
library(tigris)
library(bslib)


# scorer <- function(
#                    mhvp = median_home_value_percentile,
#                    median_home_weight = 0,
#                    hiking_nodes_percentile,
#                    hiking_nodes_weight = 100,
#                    risk_score_percentile,
#                    risk_score_weight = 100,
#                    average_prime_utci_percentile,
#                    average_prime_utci_weight = 100,
#                    min_dist_airport_percentile,
#                    min_dist_airport_weight = 100,
#                    neighbor_dg_percentile,
#                    neighbor_dg_weight = 100
#                    ){
#   median_home_value_percentile * median_home_weight +
#     hiking_nodes_percentile * hiking_nodes_weight +
#     risk_score_percentile * risk_score_weight +
#     average_prime_utci_percentile * average_prime_utci_weight +
#     min_dist_airport_percentile * min_dist_airport_weight  +
#     neighbor_dg_percentile * neighbor_dg_weight
# }

cs <- readRDS(paste0(here::here(), '/data/county_stats.rds')) %>%
  janitor::clean_names() %>% st_sf() %>% 
  rename(median_home_value = estimate,
         median_home_value_percentile = estimate_percentile,
         hiking_nodes = neighbor_nodes,
         hiking_nodes_percentile = neighbor_nodes_percentile) %>%
  mutate(score = 0)

pres_res <- read_csv('https://raw.githubusercontent.com/fivethirtyeight/election-results/main/election_results_presidential.csv')
pres_res <- pres_res %>% 
  filter(cycle == '2020', stage == 'general') %>% 
  group_by(state_abbrev) %>% 
  filter(votes == max(votes)) %>% 
  select(state_abbrev, ballot_party)

st <- tigris::states(cb = T) %>% filter(!STATEFP  %in% c(60, 02, 66, 69, 78, 72, 15))
p_boundaries <- st %>% left_join(pres_res, by = c('STUSPS' = 'state_abbrev')) %>% 
  mutate(color = if_else(ballot_party == 'DEM', 'blue', 'red'))

cs <- cs %>% left_join(pres_res, by = c('stusps' = 'state_abbrev')) %>% 
  mutate(blue_state = if_else(ballot_party == 'DEM', 1, 0)) %>%
  select(-ballot_party)

# Define UI for application that draws a histogram
ui <- page_sidebar(

    # Application title
    # titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    #sidebarLayout(
        sidebar = sidebar(
          #sidebarPanel(
          
          sliderInput("home_price_filter",
                      "Home Prices:",
                      min = 1,
                      max = 1.5e6,
                      value = c(215000,800000)),
          
            sliderInput("median_home_weight",
                        "Median Home Weight:",
                        min = 1,
                        max = 100,
                        value = 1),
            sliderInput("average_prime_utci_weight",
                        "Weather Weight:",
                        min = 1,
                        max = 100,
                        value = 100),
            sliderInput("risk_score_weight",
                        "Natural Hazard Weight:",
                        min = 1,
                        max = 100,
                        value = 65),
            sliderInput("min_dist_airport_weight",
                        "Airport Proximity Weight:",
                        min = 1,
                        max = 100,
                        value = 100),
            sliderInput("hiking_nodes_weight",
                        "Hiking Nodes Weight:",
                        min = 1,
                        max = 100,
                        value = 80),
            sliderInput("neighbor_dg_weight",
                        "Disc Golf Weight:",
                        min = 1,
                        max = 100,
                        value = 60),
          checkboxInput('blue_state',
                        'Blue State',
                        T),
          checkboxInput('filter_reds',
                        'Filter Red States',
                        T)
        ),

        # Show a plot of the generated distribution
        #mainPanel(
           leafglOutput('map'),
           accordion(open = F,
                     # accordion_panel(
                     #   'Map',
                     #   leafglOutput('map'),
                     # ),
                     accordion_panel(
                     'Table',
                     dataTableOutput('tbl')
                     )
           )
        #)
    #)
)

# Define server logic required to draw a histogram
server <- function(input, output) {


  
  mapdf <- reactive({
    
    cs$score <-  (
     cs$median_home_value_percentile * input$median_home_weight +
     cs$hiking_nodes_percentile * input$hiking_nodes_weight +
     cs$risk_score_percentile * input$risk_score_weight +
     cs$average_prime_utci_percentile * input$average_prime_utci_weight +
     cs$min_dist_airport_percentile * input$min_dist_airport_weight  +
     cs$neighbor_dg_percentile * input$neighbor_dg_weight) *
      if(input$blue_state == T){
        cs$blue_state
      } else { 1 }
    
    
    if(input$filter_reds == T){
      cs %>% 
        filter(blue_state == 1) %>% 
        mutate(
        score = if_else(
          between(median_home_value, input$home_price_filter[1], input$home_price_filter[2]), score, 0),
        score_percentile = percent_rank(score)
      ) %>% st_sf()
      
    } else {
    
    cs %>% mutate(
      score = if_else(
        between(median_home_value, input$home_price_filter[1], input$home_price_filter[2]), score, 0),
      score_percentile = percent_rank(score)
      ) %>% st_sf()
    }
  })
  
  
    output$map <- renderLeaflet({

      #pal <- colorQuantile('magma', NULL, n = 5)
      pal <- colorBin("magma", bins = c(0,.5,.75,.90,.95,1), na.color = 'grey')
      
      m <- mapdf() %>% st_cast('POLYGON')
      popup = paste0( "County: "
                      , m$namelsad, ', ', m$stusps,
                      "<br>Score: ", round(m$score, 0 ), '(', round(m$score_percentile, 2), ')',
                      '<br>Median Home Value: ', round(m$median_home_value/1000, 0) , 'k (',round(m$median_home_value_percentile, 2), ')',
                      '<br>'
                      
                    )
      
      leaflet() %>%
        addProviderTiles("OpenStreetMap", layerId = "osm") %>%
        removeGlPolygons(layerId = 'polys') %>%
        addGlPolygons(
          data = m,
          fillColor = ~pal(score_percentile),
          fillOpacity = .9,
          stroke = F,
          popup = popup,
          layerID = 'polys'
        ) %>%
        # {if (input$blue_state == T) {
        # addPolylines(data = p_boundaries %>% st_cast('POLYGON') %>% st_cast('LINESTRING'),
        #               color = ~color,
        #               fillOpacity = 0,
        #               stroke = T,
        #               weight = .8)
        # } else  {.} }%>%
        addLegend(map = .,
                  position = 'bottomright', 
                  pal = pal, 
                  values = m$score, 
                  title = 'Score',
                  opacity = 1
        )
      
    })
    
    output$tbl <- renderDataTable({
      mapdf() %>% 
        st_drop_geometry() %>% 
        arrange(desc(score)) %>% 
        select(namelsad, stusps, score, everything(),
               -geoid, -state_name, -aland, -name,
               ) %>% 
        mutate(across(
          where(is.numeric), round, digits = 2)
          ) %>%
        datatable()
    })
    
    # observe({
    #   leaflet::leafletProxy("map") %>%
    #     clearShapes() %>%
    #     addPolygons(
    #       data = mapdf(),
    #       fillColor = ~score,
    #       fillOpacity = 0.7,
    #       color = "black",
    #       stroke = TRUE,
    #       layerId = "score",
    #       group = "layersToAdd"
    #     )
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
