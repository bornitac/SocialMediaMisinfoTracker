# This script is used to deploy the RShiny app
# But I recommend using the GUI in RStudio to do this

# Loading package to deploy app

# Use credentials for deployment, run to authenticate (you should only need to do this once)
#rsconnect::setAccountInfo(name='3ezgmp-bornita0chowdhury', # Add shinyapps username
# token='13BB915FAD2F40A0BB8178BA37285F0', # Add shinyapps token
#secret='0t6Tp4IJ3iZ7/Abjf+nZ/jBUe4H3MLAYtlpc9EU') # Add shinyapps secret
# Warning: Do not publish these credentials publicly (e.g. on Github)
library(shiny)
library(tidyverse)
library(dplyr)
library(stringr)
library(sf)
library(leaflet)
library(maps)

states_sf <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
states_sf <- st_transform(states_sf, crs = 4326)
states_sf$state_name <- states_sf$ID

topics <- c(
  "COVID vaccine side effects"    = "covid vaccine side effects",
  "Election fraud 2016"           = "election fraud 2016",
  "Flat Earth"                    = "flat earth",
  "Chemtrails"                    = "chemtrails",
  "Climate change hoax"           = "climate change hoax",
  "QAnon"                         = "qanon",
  "Big Pharma hiding cures"       = "big pharma cures",
  "9/11 conspiracy"               = "9/11 conspiracy",
  "Pizzagate"                     = "pizzagate",
  "Mandela Effect Froot Loops"    = "mandela effect froot loops"
)

load_local_data <- function(keyword) {
  
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
  
  fname <- file.path("data", file_map[[keyword]])
  req(fname)
  
  
  df_raw <- read_csv(fname, skip = 2, show_col_types = FALSE)
  df_raw <- df_raw[-1, ]
  
  names(df_raw)[1] <- "date"
  df_raw$date <- as.Date(df_raw$date, format = "%m/%d/%y")
  
  hit_cols <- names(df_raw)[names(df_raw) != "date"]
  
  df_raw[hit_cols] <- lapply(df_raw[hit_cols], function(x) {
    as.numeric(str_replace(x, "<1", "0"))
  })
  
  df_clean <- df_raw %>%
    mutate(hits = rowMeans(across(all_of(hit_cols)), na.rm = TRUE)) %>%
    select(date, hits)
  
  return(df_clean)
}

load_map_data <- function(keyword) {
  
  map_file_map <- list(
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
  
  fname <- file.path("data", map_file_map[[keyword]])
  req(file.exists(fname))
  
  
  df <- read_csv(fname, show_col_types = FALSE)
  
  value_col <- names(df)[2]
  
  df_clean <- df %>%
    rename(
      state_name = 1,
      interest   = all_of(value_col)
    ) %>%
    mutate(
      state_name = tolower(state_name),
      interest = as.numeric(str_replace(interest, "<1", "0"))
    )
  
  return(df_clean)
}


# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  titlePanel("SMM: Social Media Misinformation Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "keyword",
        "Choose a topic:",
        choices = topics,
        selected = "covid vaccine side effects"
      ),
      actionButton("go", "Load Data"),
      br(), br(),
      helpText("Displays Google Trends time-series from your local CSV files."),
      hr(),
      verbatimTextOutput("summary_stats")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Trend Over Time", plotOutput("trendPlot")),
        tabPanel("US Map", leafletOutput("usMap")),
        tabPanel(
          "README",
          h3("About SMM: Social Media Misinformation Tracker"),
          p("This prototype app uses locally stored Google Trends CSV data to visualize interest in misinformation topics over time.")
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # Load data reactively when button is pressed
  data_ts <- eventReactive(input$go, {
    load_local_data(input$keyword)
  })
  
  data_map <- eventReactive(input$go, {
    load_map_data(input$keyword)
  })
  
  # SUMMARY STATISTICS
  output$summary_stats <- renderText({
    df <- data_ts()
    req(df)
    
    avg_hits <- mean(df$hits, na.rm = TRUE)
    max_hits <- max(df$hits, na.rm = TRUE)
    min_hits <- min(df$hits, na.rm = TRUE)
    peak_date <- df$date[df$hits == max_hits]
    
    paste0(
      "Summary for '", input$keyword, "':\n",
      "• Average interest: ", round(avg_hits, 2), "\n",
      "• Peak interest: ", round(max_hits), " on ", peak_date, "\n",
      "• Minimum interest: ", round(min_hits, 2)
    )
  })
  
  # TREND PLOT
  output$trendPlot <- renderPlot({
    df <- data_ts()
    req(df)
    
    ggplot(df, aes(date, hits)) +
      geom_line(color = "steelblue", linewidth = 1.2) +
      theme_minimal() +
      labs(
        title = paste("Google Search Interest Over Time for:", input$keyword),
        x = "Date",
        y = "Search Interest (0–100)"
      )
  })
  
  output$usMap <- renderLeaflet({
    req(data_map())
    
    df_map <- data_map()
    
    states_joined <- states_sf %>%
      left_join(df_map, by = "state_name")
    
    pal <- colorNumeric("viridis", domain = states_joined$interest)
    
    leaflet(states_joined) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(interest),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        popup = ~paste0("<b>", state_name, "</b><br>Interest: ", interest)
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = states_joined$interest,
        title = "Search Interest"
      )
  })
}




# ============================================================
# RUN THE APP
# ============================================================
shinyApp(ui, server)