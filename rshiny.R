# This script is used to deploy the RShiny app
# But I recommend using the GUI in RStudio to do this

# Loading package to deploy app

# Use credentials for deployment, run to authenticate (you should only need to do this once)
#rsconnect::setAccountInfo(name='3ezgmp-bornita0chowdhury', # Add shinyapps username
# token='13BB915FAD2F40A0BB8178BA37285F0', # Add shinyapps token
#secret='0t6Tp4IJ3iZ7/Abjf+nZ/jBUe4H3MLAYtlpc9EU') # Add shinyapps secret
# Warning: Do not publish these credentials publicly (e.g. on Github)
# ============================================================
# SMM: Social Media Misinformation Tracker
# Baseline Analysis + Cross-State Comparison
# Uses ONLY local Google Trends CSV exports
# ============================================================

# ======================
# LIBRARIES
# ======================
library(shiny)
library(tidyverse)
library(stringr)
library(sf)
library(leaflet)
library(maps)

# ======================
# US STATES SHAPEFILE
# ======================
states_sf <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
states_sf <- st_transform(states_sf, 4326)
states_sf$state_name <- tolower(states_sf$ID)

# ======================
# TOPICS (WITH EMOJIS)
# ======================
topics <- c(
  "🧪 COVID vaccine side effects" = "covid vaccine side effects",
  "🗳️ Election fraud 2016"        = "election fraud 2016",
  "🛰️ Flat Earth"                 = "flat earth",
  "🌫️ Chemtrails"                 = "chemtrails",
  "🔥 Climate change hoax"         = "climate change hoax",
  "🌀 QAnon"                      = "qanon",
  "💊 Big Pharma hiding cures"     = "big pharma cures",
  "🕵️‍♂️ 9/11 conspiracy"          = "9/11 conspiracy",
  "🍕 Pizzagate"                  = "pizzagate",
  "🥣 Mandela Effect: Froot Loops" = "mandela effect froot loops"
)

# ======================
# FILE MAPS
# ======================
time_files <- list(
  "covid vaccine side effects" = "covidvaccinesideeffects.csv",
  "election fraud 2016"        = "electionfraud2016.csv",
  "flat earth"                 = "flatearth.csv",
  "chemtrails"                 = "chemtrails.csv",
  "climate change hoax"        = "climatechangehoax.csv",
  "qanon"                      = "qanon.csv",
  "big pharma cures"           = "bigpharmacures.csv",
  "9/11 conspiracy"            = "911conspiracy.csv",
  "pizzagate"                  = "pizzagate.csv",
  "mandela effect froot loops" = "mandelaeffectfrootloops.csv"
)

map_files <- list(
  "covid vaccine side effects" = "geoMapCovid.csv",
  "election fraud 2016"        = "geoMapElection.csv",
  "flat earth"                 = "geoMapFlatEarth.csv",
  "chemtrails"                 = "geoMapChem.csv",
  "climate change hoax"        = "geoMapClimate.csv",
  "qanon"                      = "geoMapQanon.csv",
  "big pharma cures"           = "geoMapPharma.csv",
  "9/11 conspiracy"            = "geoMapNine.csv",
  "pizzagate"                  = "geoMapPizza.csv",
  "mandela effect froot loops" = "geoMapMandela.csv"
)

# ======================
# LOAD TIME DATA
# ======================
load_time_data <- function(keyword) {
  df <- read_csv(file.path("CSV", time_files[[keyword]]),
                 skip = 2, show_col_types = FALSE)
  df <- df[-1, ]
  
  dates <- as.Date(df[[1]], tryFormats = c("%m/%d/%y", "%Y-%m-%d"))
  
  values <- df[-1] %>%
    mutate(across(everything(),
                  ~ as.numeric(str_replace(., "<1", "0"))))
  
  tibble(
    date = dates,
    hits = rowMeans(values, na.rm = TRUE)
  )
}

# ======================
# LOAD MAP DATA
# ======================
load_map_data <- function(keyword) {
  df <- read_csv(file.path("Map CSV", map_files[[keyword]]),
                 skip = 2, show_col_types = FALSE)
  
  if (ncol(df) < 2) {
    return(tibble(state_name = states_sf$state_name, interest = 0))
  }
  
  tibble(
    state_name = tolower(df[[1]]),
    interest   = as.numeric(str_replace(df[[2]], "<1", "0"))
  )
}

# ======================
# UI
# ======================
ui <- fluidPage(
  titlePanel("SMM: Social Media Misinformation Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'trend'",
        selectInput("single_topic", "Single topic:", topics)
      ),
      conditionalPanel(
        condition = "input.tabs == 'singlemap'",
        selectInput("map_topic", "Single topic:", topics)
      ),
      conditionalPanel(
        condition = "input.tabs == 'compare'",
        selectInput("compare_a", "Compare topic A:", topics),
        selectInput("compare_b", "Compare topic B:", topics)
      ),
      actionButton("go", "Load Data")
    ),
    
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Trend Over Time",
                           value = "trend",
                           plotOutput("trendPlot"),
                           verbatimTextOutput("summary")),
                  
                  tabPanel("Single Topic Map",
                           value = "singlemap",
                           leafletOutput("singleMap", height = 520)),
                  
                  tabPanel("Comparison Map",
                           value = "compare",
                           leafletOutput("compareMap", height = 520)),
                  
                  tabPanel("README",
                           p("This app explores misinformation-related Google search interest."),
                           p("The comparison map highlights which topic dominates at the state level.")
                  )
      )
    )
  )
)

# ======================
# SERVER
# ======================
server <- function(input, output) {
  
  ts_data <- eventReactive(input$go,
                           load_time_data(input$single_topic))
  
  single_map <- eventReactive(input$go,
                              load_map_data(input$map_topic))
  
  compare_map <- eventReactive(input$go, {
    a <- load_map_data(input$compare_a)
    b <- load_map_data(input$compare_b)
    
    full_join(a, b, by = "state_name",
              suffix = c("_a", "_b")) %>%
      mutate(
        interest_a = replace_na(interest_a, 0),
        interest_b = replace_na(interest_b, 0),
        diff = interest_a - interest_b
      )
  })
  
  # ===== Trend =====
  output$trendPlot <- renderPlot({
    df <- ts_data()
    ggplot(df, aes(date, hits)) +
      geom_line(color = "steelblue", linewidth = 1.2) +
      theme_minimal() +
      labs(x = "Date", y = "Search Interest",
           title = paste("Search Interest Over Time:",
                         input$single_topic))
  })
  
  output$summary <- renderText({
    df <- ts_data()
    paste0(
      "Average: ", round(mean(df$hits), 2), "\n",
      "Peak: ", max(df$hits), "\n",
      "Minimum: ", min(df$hits)
    )
  })
  
  # ===== Single Map =====
  output$singleMap <- renderLeaflet({
    joined <- states_sf %>%
      left_join(single_map(), by = "state_name") %>%
      mutate(interest = replace_na(interest, 0))
    
    pal <- colorNumeric("viridis", domain = joined$interest)
    
    leaflet(joined) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(interest),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        popup = ~paste0(state_name,
                        "<br>Interest: ", interest)
      ) %>%
      addLegend("bottomright", pal = pal,
                values = joined$interest,
                title = "Relative Search Interest")
  })
  
  # ===== Comparison Map =====
  output$compareMap <- renderLeaflet({
    joined <- states_sf %>%
      left_join(compare_map(), by = "state_name")
    
    pal <- colorNumeric(c("red", "white", "blue"),
                        domain = joined$diff)
    
    leaflet(joined) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(diff),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        popup = ~paste0(
          state_name, "<br>",
          input$compare_a, ": ", interest_a, "<br>",
          input$compare_b, ": ", interest_b
        )
      ) %>%
      addLegend("bottomright", pal = pal,
                values = joined$diff,
                title = "Topic Dominance (A − B)")
  })
}

# ======================
# RUN APP
# ======================
shinyApp(ui, server)
