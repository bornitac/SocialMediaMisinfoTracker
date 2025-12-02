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

# Preset misinformation-related topics
topics <- c(
  "🧪 COVID vaccine side effects"    = "covid vaccine side effects",
  "🗳️ Election fraud 2016"           = "election fraud 2016",
  "🛰️ Flat Earth"                    = "flat earth",
  "🌫️ Chemtrails"                    = "chemtrails",
  "🔥 Climate change hoax"            = "climate change hoax",
  "🌀 QAnon"                          = "qanon",
  "💊 Big Pharma hiding cures"        = "big pharma cures",
  "🕵️‍♂️ 9/11 conspiracy"             = "9/11 conspiracy",
  "🍕 Pizzagate"                      = "pizzagate"
)

# Build US state polygons using modern sf syntax
states_sf <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
states_sf <- st_transform(states_sf, crs = 4326)
states_sf$state_name <- states_sf$ID   

# UI 
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
      actionButton("go", "Search"),
      br(), br(),
      helpText("Select one of the misinformation topics to see search interest over the past 5 years."),
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
          p("This prototype app uses Google Trends data to explore how interest in misinformation topics changes over time.")
        )
      )
    )
  )
)

# SERVER 
server <- function(input, output, session) {
  
  trends <- eventReactive(input$go, {
    
    g <- gtrends(keyword = input$keyword,
                 geo = "US",
                 time = "today+5-y")
    
    #Time series 
    time_df <- g$interest_over_time %>% 
      mutate(hits = as.numeric(replace(hits, hits == "<1", 0)))
    
    # Region data 
    geo_df <- g$interest_by_region %>% 
      mutate(
        state_name = tolower(location), 
        interest = as.numeric(replace(hits, hits == "<1", 0)) 
      ) %>% 
      select(state_name, interest) 
    
    #Merge region data into spatial polygons 
    states_sf2 <- states_sf %>% 
      left_join(geo_df, by = "state_name") 
    
    list(time = time_df, map = states_sf2) 
  }) 
  
  #Summary stats in sidebar 
  output$summary_stats <- renderText({
    req(trends()$time) 
    
    df <- trends()$time 
    avg_hits <- mean(df$hits, na.rm = TRUE) 
    max_hits <- max(df$hits, na.rm = TRUE) 
    min_hits <- min(df$hits, na.rm = TRUE) 
    peak_date <- df$date[df$hits == max_hits] 
    
    paste0( 
      "Summary for '", input$keyword, "':\n",
      "Average interest: ",round(avg_hits, 2), "\n", 
      "Peak interest: ", round(max_hits, 2), " on ", peak_date, "\n", 
      "Minimum interest: ", round(min_hits, 2) ) })
  
  #Trend Plot 
  output$trendPlot <- renderPlot({ 
    req(trends()$time) 
    ggplot(trends()$time, aes(date, hits)) + 
      geom_line(color = "steelblue") + 
      theme_minimal() + 
      labs(title = paste("Google Search interest for", input$keyword), 
           x = "Date", 
           y = "Search Interest (0–100)")
  }) 
  
  #US Map 
  output$usMap <- renderLeaflet({ 
    req(trends()$map) 
    df_map <- trends()$map 
    pal <- colorNumeric("viridis", domain = df_map$interest) 
    
    leaflet(df_map) %>% 
      addTiles() %>% 
      addPolygons( 
        fillColor = ~pal(interest), 
        fillOpacity = 0.8, 
        weight = 1, 
        color = "white", 
        popup = ~paste0("<b>", state_name, "</b><br>Interest: ", interest) 
      ) %>% 
      addLegend( "bottomright", 
                 pal = pal, 
                 values = df_map$interest, 
                 title = "Search Interest (0–100)" ) }) }

# Run App 
shinyApp(ui, server) 

