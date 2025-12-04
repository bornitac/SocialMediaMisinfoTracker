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

topics <- c(
  "COVID vaccine side effects"    = "covid vaccine side effects",
  "Election fraud 2016"           = "election fraud 2016",
  "Flat Earth"                    = "flat earth",
  "Chemtrails"                    = "chemtrails",
  "Climate change hoax"           = "climate change hoax",
  "QAnon"                         = "qanon",
  "Big Pharma hiding cures"       = "big pharma cures",
  "9/11 conspiracy"            = "9/11 conspiracy",
  "Pizzagate"                     = "pizzagate",
  "Mandela Effect Froot Loops"    = "mandela effect froot loops"
)

load_local_data <- function(keyword) {
  
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
  
  fname <- file_map[[keyword]]
  
  # Step 1: read CSV, skip first 2 header rows
  df_raw <- read_csv(
    fname,
    skip = 2,
    show_col_types = FALSE
  )
  
  # Step 2: remove the first row ("Week", label text)
  df_raw <- df_raw[-1, ]
  
  # Step 3: rename first column to "date"
  names(df_raw)[1] <- "date"
  
  # Step 4: convert date strings to Date
  df_raw$date <- as.Date(df_raw$date, format = "%m/%d/%y")
  
  # Step 5: identify hit columns (all except date)
  hit_cols <- names(df_raw)[names(df_raw) != "date"]
  
  # Step 6: clean numeric columns
  df_raw[hit_cols] <- lapply(df_raw[hit_cols], function(x) {
    as.numeric(str_replace(x, "<1", "0"))
  })
  
  # Step 7: compute average hits (some files have 2 hit columns)
  df_clean <- df_raw %>%
    mutate(hits = rowMeans(across(all_of(hit_cols)), na.rm = TRUE)) %>%
    select(date, hits)
  
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
}

# ============================================================
# RUN THE APP
# ============================================================
shinyApp(ui, server)
