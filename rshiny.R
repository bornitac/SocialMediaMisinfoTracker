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
# Local Google Trends CSVs only
# ============================================================

library(shiny)
library(tidyverse)
library(stringr)
library(sf)
library(leaflet)
library(maps)

# ------------------------------------------------------------
# US STATES SHAPEFILE
# ------------------------------------------------------------
states_sf <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
states_sf <- st_transform(states_sf, 4326)
states_sf$state_name <- tolower(states_sf$ID)

# ------------------------------------------------------------
# TOPICS (EMOJIS)
# ------------------------------------------------------------
topics <- c(
  "🧪 COVID vaccine side effects" = "covid vaccine side effects",
  "🗳️ Election fraud 2016" = "election fraud 2016",
  "🛰️ Flat Earth" = "flat earth",
  "🌫️ Chemtrails" = "chemtrails",
  "🔥 Climate change hoax" = "climate change hoax",
  "🌀 QAnon" = "qanon",
  "💊 Big Pharma hiding cures" = "big pharma cures",
  "🕵️‍♂️ 9/11 conspiracy" = "9/11 conspiracy",
  "🍕 Pizzagate" = "pizzagate",
  "🥣 Mandela Effect: Froot Loops" = "mandela effect froot loops"
)

# ------------------------------------------------------------
# DATA LOADERS
# ------------------------------------------------------------
load_time_data <- function(keyword) {
  file_map <- list(
    "covid vaccine side effects" = "covidvaccinesideeffects.csv",
    "election fraud 2016" = "electionfraud2016.csv",
    "flat earth" = "flatearth.csv",
    "chemtrails" = "chemtrails.csv",
    "climate change hoax" = "climatechangehoax.csv",
    "qanon" = "qanon.csv",
    "big pharma cures" = "bigpharmacures.csv",
    "9/11 conspiracy" = "911conspiracy.csv",
    "pizzagate" = "pizzagate.csv",
    "mandela effect froot loops" = "mandelaeffectfrootloops.csv"
  )
  
  df <- read_csv(file.path("CSV", file_map[[keyword]]),
                 skip = 2, show_col_types = FALSE) |>
    slice(-1)
  
  dates <- as.Date(df[[1]], tryFormats = c("%Y-%m-%d", "%m/%d/%y"))
  
  vals <- df[-1] |> mutate(across(everything(),
                                  ~ as.numeric(str_replace(.x, "<1", "0"))))
  
  tibble(
    date = dates,
    hits = rowMeans(vals, na.rm = TRUE)
  )
}

load_map_data <- function(keyword) {
  file_map <- list(
    "covid vaccine side effects" = "geoMapCovid.csv",
    "election fraud 2016" = "geoMapElection.csv",
    "flat earth" = "geoMapFlatEarth.csv",
    "chemtrails" = "geoMapChem.csv",
    "climate change hoax" = "geoMapClimate.csv",
    "qanon" = "geoMapQanon.csv",
    "big pharma cures" = "geoMapPharma.csv",
    "9/11 conspiracy" = "geoMapNine.csv",
    "pizzagate" = "geoMapPizza.csv",
    "mandela effect froot loops" = "geoMapMandela.csv"
  )
  
  df <- read_csv(file.path("Map CSV", file_map[[keyword]]),
                 skip = 2, show_col_types = FALSE)
  
  tibble(
    state_name = tolower(df[[1]]),
    interest = as.numeric(str_replace(df[[2]], "<1", "0"))
  ) |> mutate(interest = replace_na(interest, 0))
}

# ------------------------------------------------------------
# UI (NO SIDEBAR)
# ------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel("SMM: Social Media Misinformation Tracker"),
  
  tabsetPanel(
    
    # ---------------- Trend Tab ----------------
    tabPanel("📈 Trend Over Time",
             br(),
             selectInput("trend_topic", "Choose a topic:", topics),
             actionButton("trend_go", "Show Trend"),
             plotOutput("trendPlot")
    ),
    
    # ---------------- Single Map ----------------
    tabPanel("🗺️ Single Topic Map",
             br(),
             selectInput("map_topic", "Choose a topic:", topics),
             actionButton("map_go", "Show Map"),
             leafletOutput("singleMap", height = 600)
    ),
    
    # ---------------- Comparison Map ----------------
    tabPanel("⚖️ Comparison Map",
             br(),
             selectInput("compare_a", "Topic A:", topics),
             selectInput("compare_b", "Topic B:", topics),
             actionButton("compare_go", "Compare"),
             leafletOutput("compareMap", height = 600)
    ),
    
    # ---------------- README ----------------
    tabPanel("📘 README",
             h4("About This Project"),
             p("This app examines how misinformation-related search interest varies over time and across U.S. states."),
             p("It supports geographic comparison between conspiracies, highlighting regional differences."),
             p("All data come from Google Trends CSV exports and are processed locally.")
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  output$trendPlot <- renderPlot({
    req(input$trend_go)
    
    df <- load_time_data(input$trend_topic)
    
    ggplot(df, aes(date, hits)) +
      geom_line(color = "#1f78b4", linewidth = 1.2) +
      theme_minimal() +
      labs(
        title = paste("Search Interest Over Time:", input$trend_topic),
        x = "Date",
        y = "Interest (0–100)"
      )
  })
  
  output$singleMap <- renderLeaflet({
    req(input$map_go)
    
    df <- load_map_data(input$map_topic)
    
    joined <- states_sf |>
      left_join(df, by = "state_name")
    
    pal <- colorNumeric("viridis", joined$interest)
    
    leaflet(joined) |>
      addTiles() |>
      addPolygons(
        fillColor = ~pal(interest),
        fillOpacity = 0.8,
        weight = 1,
        color = "white",
        popup = ~paste(state_name, ": ", interest)
      ) |>
      addLegend("bottomright",
                pal = pal,
                values = joined$interest,
                title = "Search Interest")
  })
  
  output$compareMap <- renderLeaflet({
    req(input$compare_go)
    
    a <- load_map_data(input$compare_a)
    b <- load_map_data(input$compare_b)
    
    diff <- a |>
      left_join(b, by = "state_name", suffix = c("_a", "_b")) |>
      mutate(diff = interest_a - interest_b)
    
    joined <- states_sf |> left_join(diff, by = "state_name")
    
    pal <- colorNumeric("RdBu", joined$diff)
    
    leaflet(joined) |>
      addTiles() |>
      addPolygons(
        fillColor = ~pal(diff),
        fillOpacity = 0.8,
        color = "white",
        popup = ~paste(state_name, ": ", diff)
      ) |>
      addLegend("bottomright",
                pal = pal,
                values = joined$diff,
                title = "Difference (A − B)")
  })
}

# ------------------------------------------------------------
# RUN
# ------------------------------------------------------------
shinyApp(ui, server)
