# 🗺️ Social Media Misinformation Tracker

An R Shiny web application that analyzes U.S. misinformation trends using publicly available Google Trends data. Users can explore how interest in different conspiracy and misinformation topics has shifted across the United States over the past five years.

---

## 🔍 What It Does

- Analyzes U.S. misinformation trends using Google Trends and automated data collection
- Generates time-series visualizations showing how search interest rises and falls over time
- Produces state-level choropleth maps highlighting search interest by state
- Enables side-by-side topic comparisons with a ratio summary of relative search interest
- Identifies the most-searched misinformation topic per state using geospatial joins

---

## 📊 App Features

- **📈 Trend Over Time** – Time-series line graph showing search interest for a selected topic over 5 years
- **🗺️ Single Topic Map** – Interactive Leaflet map showing state-level interest for one topic
- **📊 Comparison Graph** – Overlaid line graph comparing two topics with a ratio summary
- **🗺 Most Popular Topic Map** – Identifies and displays the dominant misinformation topic in each U.S. state
- **📘 README Tab** – In-app project description and usage guide

---

## 🧪 Topics Tracked

- 🧪 COVID Vaccine Side Effects
- 🗳️ Election Fraud 2016
- 🛰️ Flat Earth
- 🌫️ Chemtrails
- 🔥 Climate Change Hoax
- 🌀 QAnon
- 💊 Big Pharma Hiding Cures
- 🕵️ 9/11 Conspiracy
- 🍕 Pizzagate
- 🥣 Mandela Effect: Froot Loops

---

## 🛠️ Built With

- **R** – Core scripting and data processing
- **R Shiny** – Interactive web application framework
- **Google Trends API (gtrendsR)** – Data source for misinformation search trends
- **ggplot2** – Time-series and comparison visualizations
- **Leaflet** – Interactive state-level choropleth maps
- **tidyverse / dplyr** – Data cleaning, aggregation, and transformation
- **sf / maps** – Geospatial joins and state boundary mapping

---

## 🚀 Getting Started

### Prerequisites
- R and RStudio installed
- Install required packages:
   ```r
   install.packages(c("shiny", "tidyverse", "stringr", "sf", "leaflet", "maps", "gtrendsR"))
   ```

### Installation
1. Clone the repository:
   ```bash
   git clone https://github.com/bornitac/SocialMediaMisinfoTracker
   ```
2. Open `CSS-RShiny-App.Rproj` in RStudio

3. Make sure the `CSV/` and `Map CSV/` folders are present with the Google Trends data files

4. Run the app:
   ```r
   shiny::runApp("app.R")
   ```

---

## 📂 Data Source

**Google Trends** — publicly available search interest data exported via the `gtrendsR` package  
🔗 https://trends.google.com

---

## 📈 Future Directions

- Add real-time Google Trends data fetching instead of pre-exported CSVs
- Include sentiment analysis on social media posts related to tracked topics
- Expand geographic scope to include international trend comparisons
- Add more misinformation categories and dynamic topic input
