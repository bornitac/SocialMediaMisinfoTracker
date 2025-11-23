# This script is used to deploy the RShiny app
# But I recommend using the GUI in RStudio to do this

# Loading package to deploy app

# Use credentials for deployment, run to authenticate (you should only need to do this once)
#rsconnect::setAccountInfo(name='3ezgmp-bornita0chowdhury', # Add shinyapps username
                         # token='13BB915FAD2F40A0BB8178BA37285F0', # Add shinyapps token
                          #secret='0t6Tp4IJ3iZ7/Abjf+nZ/jBUe4H3MLAYtlpc9EU') # Add shinyapps secret
# Warning: Do not publish these credentials publicly (e.g. on Github)
library(sf)
library(dplyr)
library(shiny)
library(gtrendsR)
library(tidyverse)
library(leaflet)
library(maps)
library(sp)

# -------------------------------------
# Build a US states polygon object
# -------------------------------------

# Build US state polygons using modern sf syntax
states_sf <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
states_sf <- st_transform(states_sf, crs = 4326)
states_sf$state_name <- states_sf$ID   # creates a matching name column

# -------------------------------------
# UI
# -------------------------------------
ui <- fluidPage(
  titlePanel("SMM: Social Media Misinformation Tracker"),
  
  sidebarLayout(
  sidebarPanel(
    textInput("keyword", "Enter a topic:", "autism tylenol"),
    actionButton("go", "Search"),
    br(), br(),
    helpText("Shows Google Trends search interest in the past 5 years."),
    hr(),
    verbatimTextOutput("summary_stats")   # <-- NEW: summary of the trend
  ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Trend Over Time", plotOutput("trendPlot")),
        tabPanel("US Map", leafletOutput("usMap")),
        tabPanel(
          "README",
          h3("About SMM: Social Media Misinformation Tracker"),
          p("This prototype app uses Google Trends data to explore how interest in misinformation topics changes over time.")
        )
      )
    )
  )
)

# -------------------------------------
# SERVER
# -------------------------------------
server <- function(input, output, session) {
  
  trends <- eventReactive(input$go, {
    
    g <- gtrends(keyword = input$keyword,
                 geo = "US",
                 time = "today+5-y")
    
    # Time series
    time_df <- g$interest_over_time %>%
      mutate(hits = as.numeric(replace(hits, hits == "<1", 0)))
    
    # Region data
    geo_df <- g$interest_by_region %>%
      mutate(
        state_name = tolower(location),
        interest = as.numeric(replace(hits, hits == "<1", 0))
      ) %>%
      select(state_name, interest)
    
    # Merge region data into spatial polygons
    states_sf2 <- states_sf %>%
      left_join(geo_df, by = "state_name")
    
    list(time = time_df, map = states_sf2)
  })
  
  # Summary stats in sidebar
  output$summary_stats <- renderText({
    req(trends()$time)
    
    df <- trends()$time
    
    avg_hits <- mean(df$hits, na.rm = TRUE)
    max_hits <- max(df$hits, na.rm = TRUE)
    min_hits <- min(df$hits, na.rm = TRUE)
    
    paste0(
      "Summary for '", input$keyword, "':\n",
      "Average interest: ", round(avg_hits, 2), "\n",
      "Peak interest: ", round(max_hits, 2), "\n",
      "Minimum interest: ", round(min_hits, 2)
    )
  })
  
  # -----------------------
  # Trend Plot
  # -----------------------
  output$trendPlot <- renderPlot({
    req(trends()$time)
    
    ggplot(trends()$time, aes(date, hits)) +
      geom_line(color = "steelblue") +
      theme_minimal() +
      labs(title = paste("Google Trends:", input$keyword),
           x = "Date",
           y = "Search Interest (0–100)")
  })
  
  # -----------------------
  # US Map
  # -----------------------
  output$usMap <- renderLeaflet({
    req(trends()$map)
    
    pal <- colorNumeric("viridis", domain = trends()$map$interest)
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = trends()$map,
        fillColor = ~pal(interest),
        fillOpacity = 0.8,
        weight = 1,
        color = "white",
        popup = ~paste0("<b>", state_name, "</b><br>Interest: ", interest)
      ) %>%
      addLegend("bottomright",
                pal = pal,
                values = trends()$map$interest,
                title = "Search Interest")
  })
}

# -------------------------------------
# Run App
# -------------------------------------
shinyApp(ui, server)

# Run to deploy app to the web
#rsconnect::deployApp(appDir="/~/Computationalss/Computationalss/rshiny.R", # Replace with path to app folder
                    # appName = "SMM") # Replace with app name
