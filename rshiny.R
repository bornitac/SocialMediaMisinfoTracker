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
# Uses ONLY local CSV files (no Google API calls)
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
# TOPICS (EMOJIS KEPT)
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
# LOAD TIME-SERIES CSV
# ======================
load_time_data <- function(keyword) {
  
  file_map <- list(
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
  
  fname <- file.path("CSV", file_map[[keyword]])
  req(file.exists(fname))
  
  df <- read_csv(fname, skip = 2, show_col_types = FALSE)
  df <- df[-1, ]   # Remove "Week / keyword row"
  
  dates <- as.Date(df[[1]], tryFormats = c("%m/%d/%y", "%Y-%m-%d"))
  values <- df[-1]
  
  values_clean <- lapply(values, function(x) {
    as.numeric(str_replace(x, "<1", "0"))
  })
  
  tibble(
    date = dates,
    hits = rowMeans(as.data.frame(values_clean), na.rm = TRUE)
  )
}

# ======================
# LOAD MAP CSV
# ======================
load_map_data <- function(keyword) {
  
  file_map <- list(
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
  
  fname <- file.path("Map CSV", file_map[[keyword]])
  req(file.exists(fname))
  
  df <- read_csv(fname, skip = 2, show_col_types = FALSE)
  
  # Handle malformed or empty CSVs safely
  if (ncol(df) < 2) {
    return(tibble(
      state_name = states_sf$state_name,
      interest = 0
    ))
  }
  
  tibble(
    state_name = tolower(df[[1]]),
    interest = as.numeric(str_replace(df[[2]], "<1", "0"))
  )
}

# ======================
# UI
# ======================
ui <- fluidPage(
  titlePanel("SMM: Social Media Misinformation Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("keyword", "Choose a topic:", choices = topics),
      actionButton("go", "Load Data"),
      hr(),
      verbatimTextOutput("summary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Trend Over Time", plotOutput("trendPlot")),
        tabPanel("US Map", leafletOutput("usMap")),
        tabPanel(
          "README",
          h4("About"),
          p("This app explores misinformation-related topics using Google Trends CSV exports.")
        )
      )
    )
  )
)

# ======================
# SERVER
# ======================
server <- function(input, output, session) {
  
  ts_data <- eventReactive(input$go, {
    load_time_data(input$keyword)
  })
  
  map_data <- eventReactive(input$go, {
    load_map_data(input$keyword)
  })
  
  output$summary <- renderText({
    df <- ts_data()
    req(df)
    
    paste0(
      "Summary for '", input$keyword, "':\n",
      "Average interest: ", round(mean(df$hits, na.rm = TRUE), 2), "\n",
      "Peak interest: ", max(df$hits, na.rm = TRUE), " on ",
      df$date[which.max(df$hits)], "\n",
      "Minimum interest: ", min(df$hits, na.rm = TRUE)
    )
  })
  
  output$trendPlot <- renderPlot({
    df <- ts_data()
    req(df)
    
    ggplot(df, aes(date, hits)) +
      geom_line(color = "steelblue", linewidth = 1.1) +
      theme_minimal() +
      labs(
        title = paste("Search Interest Over Time:", input$keyword),
        x = "Date",
        y = "Search Interest (0–100)"
      )
  })
  
  output$usMap <- renderLeaflet({
    
    df_map <- map_data()
    req(df_map)
    
    states_joined <- states_sf %>%
      left_join(df_map, by = "state_name") %>%
      mutate(
        interest = replace_na(interest, 0),
        interest = pmax(0, round(as.numeric(interest)))   # ✅ FIXES “-0”
      )
    
    max_val <- max(states_joined$interest, na.rm = TRUE)
    if (!is.finite(max_val) || max_val <= 0) max_val <- 1
    
    pal <- colorNumeric("viridis", domain = c(0, max_val))
    
    leaflet(states_joined) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(interest),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        popup = ~paste0(
          "<b>", state_name, "</b><br>",
          "Relative interest: ", interest
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = states_joined$interest,
        title = "Relative Search Interest"
      )
  })
}

# ======================
# RUN APP
# ======================
shinyApp(ui, server)
