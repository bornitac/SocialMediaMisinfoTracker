# Social Media Misinformation Tracker

An R Shiny web application analyzing U.S. misinformation trends using Google Trends data.

 **Live app:** https://bornitachowdhury.shinyapps.io/SocialMediaMisinfoTracker/

## Built With
- R, R Shiny, gtrendsR, ggplot2, Leaflet, tidyverse, sf

## Features
- Time-series visualizations of search interest over 5 years
- State-level choropleth maps via Leaflet
- Side-by-side topic comparison with ratio summary
- Identifies the most-searched misinformation topic per state

## Topics Tracked
· COVID Vaccine Side Effects 
· Election Fraud 2016 
· Flat Earth 
· Chemtrails 
· Climate Change Hoax 
· QAnon 
· Big Pharma Hiding Cures 
· 9/11 Conspiracy 
· Pizzagate 
· Mandela Effect: Froot Loops

## Running Locally
1. Clone the repo
2. Install dependencies:
```r
   install.packages(c("shiny", "tidyverse", "stringr", "sf", "leaflet", "maps", "gtrendsR"))
```
3. Open `CSS-RShiny-App.Rproj` in RStudio and run:
```r
   shiny::runApp("app.R")
```

## Data Source
Google Trends via the `gtrendsR` package — https://trends.google.com
