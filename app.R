#Load the libraries 
library(shiny)
library(tidyverse)
library(stringr)
library(sf)
library(leaflet)
library(maps)

# States SF

states_sf <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
states_sf <- st_transform(states_sf, 4326)
states_sf$state_name <- tolower(states_sf$ID)

#Making sure the titles are fine
to_title_case <- function(x) {
  stringr::str_to_title(x)
}

# Listing the topics
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

#Load the data for the map
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

#The UI
ui <- fluidPage(

  tags$style("
             body { background-color: #faf7f7; }
          
             
             #singleMap, #compareMap {
             border: 2px solid #990000;
             border-radius: 8px; }),
                 .nav-tabs a {
                   color: #cc0033;
                     font-weight: bold;
                 }
               
               .nav-tabs > .active > a {
                 background-color: #cc0033 !important;
                   color: white !important;
                 border-radius: 6px 6px 0 0 !important;
               }
               
               .nav-tabs a:hover {
                 text-decoration: underline;
                 text-decoration-color: #cc0033;
               }
               "),
  tags$head(tags$style("body { background-color: #f5f5f5; }")),
  
  titlePanel(div("SMM: Social Media Misinformation Tracker", style = "text-align:center; color:#cc0033; font-weight:bold;")),
  
  tabsetPanel(
    
    #Single Topic Line Graph 
    tabPanel("📈 Trend Over Time",
             br(),
             selectInput("trend_topic", "Choose a topic:", topics),
             actionButton("trend_go", "Show Trend", style = "background-color:#cc0033; color:white; font-weight:bold;"),
             uiOutput("trend_placeholder"),
             br(), br(),
             uiOutput("trend_placeholder"),
             plotOutput("trendPlot")
    ),
    
    #Single Topic Map
    tabPanel("🗺️ Single Topic Map",
             br(),
             selectInput("map_topic", "Choose a topic:", topics),
             actionButton("map_go", "Show Map", style = "background-color:#cc0033; color:white; font-weight:bold;"),
             uiOutput("map_placeholder"),
             leafletOutput("singleMap", height = 600)
    ),
    
    #Comparison Line Graph
    
    tabPanel("📊 Comparison Graph",
             br(),
             selectInput("comp_topic_a", "Topic A:", topics),
             selectInput("comp_topic_b", "Topic B:", topics),
             actionButton("comp_go", "Compare Trends", style = "background-color: #cc0033; color:white; font-weight:bold;"),
             uiOutput("comparison_placeholder"),
             br(),br(),
             plotOutput("comparePlot"),
             br(),
             uiOutput("ratio_text")
    ),
    
    #Most Popular Topic by State Map
    
    tabPanel("🗺 Most Popular Topic Map",
             br(),
             actionButton("pop_go", "Show Most Popular Searches by State", style = "background-color: #cc0033; color:white; font-weight:bold;"),
             uiOutput("popular_placeholder"),
             br(),br(),
             leafletOutput("popularMap", height = 600)
    ),
    
    #README
    tabPanel("📘 README",
             h4("About This Project"),
             p("This app examines how misinformation-related search interest varies over time and across U.S. states."),
             p("It supports geographic comparison between conspiracies, highlighting regional differences."),
             p("All data come from Google Trends CSV exports and are processed locally."),
             p("Users can select from well-known conspiracy themes — such as election fraud, chemtrails, or climate-related hoaxes — and instantly visualize how search activity for these topics has shifted over the past five years."),
             h4("What the App Shows"),
             tags$ul(
               tags$li("📈 A time-series graph showing how search interest rises and falls over time."),
               tags$li("🗺️ A state-level map that highlights where each topic is most or least popular."),
               tags$li("📊 A comparison graph that allows two topics to be viewed together."),
               tags$li("🧮 A ratio summary that explains how two topics differ in relative interest."),
               tags$li("🏆 A 'most popular topic by state' map that identifies the leading conspiracy in each state.")
             )
    )
  )
)

# The Server 
server <- function(input, output, session) {
  
  output$trend_placeholder <- renderUI({
    if (input$trend_go == 0) {
      div(style = "
          border: 2px solid #cc0000;
          background-color: #fff5f5;
          padding: 25px:
          margin-top:25px:
          width: 70%;
          text-align: center; 
          font-size: 16px !important;
          color: #660000;
          border-radius: 10px; 
          ",
          HTML("
        <b>Welcome to the Misinformation Mapper!</b><br><br>
        📊 Select a topic and click <b>Show Trend</b> to begin!<br>
      ")
      )
    } else {
      NULL
    }
  })
  
  output$map_placeholder <- renderUI({
    if (input$map_go == 0) {
      div(style = "
          border: 2px solid #cc0000;
          background-color: #fff5f5;
          padding: 25px:
          margin-top:25px:
          width: 70%;
          text-align: center; 
          font-size: 16px !important;
          color: #660000;
          border-radius: 10px; 
          ",
          HTML("
        📊 Select a topic and click <b>Show Map</b> to see how searches vary across states!<br>
      ")
      )
    } else {
      NULL
    }
  })
  
  output$comparison_placeholder <- renderUI({
    if (input$comp_go == 0) {
      div(style = "
          border: 2px solid #cc0000;
          background-color: #fff5f5;
          padding: 25px:
          margin-top:25px:
          width: 70%;
          text-align: center; 
          font-size: 16px !important;
          color: #660000;
          border-radius: 10px; 
          ",
          HTML("
        📊 Select <b> two topics <b> and click <b>Compare Trends</b> to see both trends side by side!<br>
      ")
      )
    } else {
      NULL
    }
  })
  
  output$popular_placeholder <- renderUI({
    if (input$pop_go == 0) {
      div(style = "
          border: 2px solid #cc0000;
          background-color: #fff5f5;
          padding: 25px:
          margin-top:25px:
          width: 70%;
          text-align: center; 
          font-size: 16px !important;
          color: #660000;
          border-radius: 10px; 
          ",
          HTML("📍
          Click the red button above to generate the map!<br>
          This will show which conspiracy topic<br>
          was <b> searched the most <b> in each state.")
      )
    } else {
      NULL
    }
  })
  
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
        popup = ~paste0(to_title_case(state_name), ": ", interest)
      ) |>
      addLegend("bottomright",
                pal = pal,
                values = joined$interest,
                title = "Search Interest")
  })
  
  output$comparePlot <- renderPlot({
    req(input$comp_go)
    
    df_a <- load_time_data(input$comp_topic_a)
    df_b <- load_time_data(input$comp_topic_b)
    
    df_a$topic <- input$comp_topic_a
    df_b$topic <- input$comp_topic_b
    
    df <- bind_rows(df_a, df_b)
    
    ggplot(df, aes(date, hits, color = topic)) +
      geom_line(linewidth = 1.3) +
      theme_minimal() +
      scale_color_manual(values = c("#cc0033", "#1f78b4")) +
      labs(
        title = paste("Comparison of Trends"),
        x = "Date",
        y = "Interest (0–100)",
        color = "Topic"
      )
  })
  
  output$ratio_text <- renderUI({
    req(input$comp_go)
    
    df_a <- load_time_data(input$comp_topic_a)
    df_b <- load_time_data(input$comp_topic_b)
    
    mean_a <- mean(df_a$hits, na.rm = TRUE)
    mean_b <- mean(df_b$hits, na.rm = TRUE)
    
    ratio <- mean_a/mean_b
    
    div(style = "
        padding: 15px;
        background-color:#fff5f5; 
        border:2px solid #cc0033; 
        border-radius:8px; 
        width:60%;
        font-size:16px;
        color:#660000;",
        HTML(paste0(
          "<b>Ratio of Average Search Interest (A ÷ B)</b><br><br>",
          "<b>", input$comp_topic_a, "</b> divided by <b>", input$comp_topic_b, "</b><br><br>",
          "<b>Ratio = ", round(ratio, 2), "</b><br><br>",
          
          "<i>How to interpret this:</i><br>",
          "A ratio of <b>1.0</b> means both topics had the same average interest.<br>",
          "A ratio <b>greater than 1.0</b> means <b>", input$comp_topic_a, "</b> was searched more overall.<br>",
          "A ratio <b>less than 1.0</b> means <b>", input$comp_topic_b, "</b> was searched more overall."
        ))
    )
  })
  
    
  output$popularMap <- renderLeaflet({
    req(input$pop_go)
    
    #get names 
    topic_keys <- unname(topics)
    
    #load each map csv
    all_maps <- lapply(topic_keys, load_map_data)
    names(all_maps) <- topic_keys
    
    #first dataset 
    final_df <- all_maps[[1]]
    colnames(final_df)[2] <- topic_keys[1]
    
    #merge in remaining datasets 
    for(i in 2: length(topic_keys)){
      df <- all_maps[[i]]
      colnames(df)[2] <- topic_keys[i]
      final_df <- left_join(final_df, df, by = "state_name")
    }
    
    #find top search
    final_df$top_topic <- apply(
      final_df[, topic_keys],
      1,
      function(row) topic_keys[which.max(row)]
    )
    
    final_df$top_topic_title <- stringr::str_to_title(final_df$top_topic)
    map_df <- left_join(states_sf, final_df, by = "state_name")
    
    #colors
    pal <- colorFactor("Set3", map_df$top_topic_title)
    
    #map 
    leaflet(map_df) |>
      addTiles() |>
      addPolygons(
        fillColor = ~pal(top_topic_title),
        fillOpacity = 0.85,
        weight = 1, 
        color = "white",
        popup = ~paste0("<b>", to_title_case(state_name), "</b><br>","🏆 Most Popular Topic:<br>",
                        top_topic_title)
      ) |>
      addLegend("bottomright", 
                pal = pal,
                values = map_df$top_topic_title,
                title = "Most Popular Topic")
  })
  
}

# RUN
shinyApp(ui, server)
