library(gtrendsR)
library(dplyr)
library(readr)

#1st Topic: COVID Vaccine Side Effects
covidsideeffect <- "covid vaccine side effects"

g <- gtrends(
  keyword = covidsideeffect,
  geo = "US",
  time = "today+5-y"
)

# Results
str(g$interest_over_time)
str(g$interest_by_region)

# Time series
time_df <- g$interest_over_time %>%
  mutate(
    hits = as.numeric(hits),
    date = as.Date(date)
  )

write_csv(time_df, "data/covid_vaccine_side_effects_time.csv")

# Region data
region_df <- g$interest_by_region %>%
  mutate(
    state_name = tolower(location),
    interest   = as.numeric(hits)
  ) %>%
  select(state_name, interest)

write_csv(region_df, "data/covid_vaccine_side_effects_region.csv")

#2nd Topic: Election Fraud in 2016
election2016 <- "election fraud 2016"

e <- gtrends(
  keyword = election2016,
  geo = "US",
  time = "today+5-y"
)

# Results
str(e$interest_over_time)
str(e$interest_by_region)

# Time series
time_df <- e$interest_over_time %>%
  mutate(
    hits = as.numeric(hits),
    date = as.Date(date)
  )

write_csv(time_df, "data/election_fraud_2016_time.csv")

# Region data
region_df <- e$interest_by_region %>%
  mutate(
    state_name = tolower(location),
    interest   = as.numeric(hits)
  ) %>%
  select(state_name, interest)

write_csv(region_df, "data/election_fraud_2016_fraud_region.csv")

#3rd Topic: 9/11 Conspiracy
nineeleven <- "9/11 conspiracy"

n11 <- gtrends(
  keyword = nineeleven,
  geo = "US",
  time = "today+5-y"
)

# Results
str(n11$interest_over_time)
str(n11$interest_by_region)

# Time series
time_df <- n11$interest_over_time %>%
  mutate(
    hits = as.numeric(hits),
    date = as.Date(date)
  )

write_csv(time_df, "data/nine_eleven_conspiracy_time.csv")

# Region data
region_df <- n11$interest_by_region %>%
  mutate(
    state_name = tolower(location),
    interest   = as.numeric(hits)
  ) %>%
  select(state_name, interest)

write_csv(region_df, "data/nine_eleven_conspiracy_region.csv")

#4th Topic: Pizzagate
pizzagate <- "pizzagate"

pg <- gtrends(
  keyword = pizzagate,
  geo = "US",
  time = "today+5-y"
)

# Results
str(pg$interest_over_time)
str(pg$interest_by_region)

# Time series
time_df <- pg$interest_over_time %>%
  mutate(
    hits = as.numeric(hits),
    date = as.Date(date)
  )

write_csv(time_df, "data/pizzagate_time.csv")

# Region data
region_df <- pg$interest_by_region %>%
  mutate(
    state_name = tolower(location),
    interest   = as.numeric(hits)
  ) %>%
  select(state_name, interest)

write_csv(region_df, "data/pizzagate_region.csv")

#Anika Topics 

#1st topic: flat earth theory 
flatearth <-- "flat earth"

f <- gtrends(
  keyword = flatearth,
  geo = "US",
  time = "today+5-y"
)

#Results 
str(f$interest_over_time)
str(f$interest_by_region)

#Time Series 
time_df <- f$interest_over_time %>%
  mutate(
    hits = as.numeric(hits),
    date = as.Date(date)
  )

write_cvs(time_df, "data/flat_earth_time.csv")

#Region data
region_df <- f$interest_by_region %>%
  mutate(
    state_name = tolower(location),
    interest   = as.numeric(hits)
  ) %>%
  select(state_name, interest)
write_csv(region_df, "data/flat_earth_region.csv")

#2nd Topic: Chemtrails 

chemtrails <- "chemtrails"

c <- gtrends(
  keyword = chemtrails,
  geo = "US",
  time = "today+5-y"
)

#Results 
str(c$interest_over_time)
str(c$interest_by_region)

#Time Series 
time_df <- c$interest_over_time %>%
  mutate(
    hits = as.numeric(hits),
    date = as.Date(date)
  )

write_cvs(time_df, "data/chemtrails_time.csv")

#Region data
region_df <- c$interest_by_region %>%
  mutate(
    state_name = tolower(location),
    interest   = as.numeric(hits)
  ) %>%
  select(state_name, interest)
write_csv(region_df, "data/chemtrails_region.csv")

#3rd Topic: Climate change is a hoax 
climatehoax <- "climate change hoax"

ch <- gtrends(
  keyword = climatehoax,
  geo = "US",
  time = "today+5-y"
)

#Results 
str(ch$interest_over_time)
str(ch$interest_by_region)

#Time Series 
time_df <- ch$interest_over_time %>%
  mutate(
    hits = as.numeric(hits),
    date = as.Date(date)
  )

write_cvs(time_df, "data/climate_change_hoax_time.csv")

#Region data
region_df <- ch$interest_by_region %>%
  mutate(
    state_name = tolower(location),
    interest   = as.numeric(hits)
  ) %>%
  select(state_name, interest)
write_csv(region_df, "data/climate_change_hoax_region.csv")

#4th Topic: QAnon 
qanon <- "qanon"

q <- gtrends(
  keyword = qanon,
  geo = "US",
  time = "today+5-y"
)

#Results 
str(q$interest_over_time)
str(q$interest_by_region)

#Time Series 
time_df <- q$interest_over_time %>%
  mutate(
    hits = as.numeric(hits),
    date = as.Date(date)
  )

write_cvs(time_df, "data/qanon_time.csv")

#Region data
region_df <- q$interest_by_region %>%
  mutate(
    state_name = tolower(location),
    interest   = as.numeric(hits)
  ) %>%
  select(state_name, interest)
write_csv(region_df, "data/qanon_region.csv")

#5th Topic: Pharma Companies hide Cures for profit 
pharm -> "big pharma cures"

bp <- gtrends(
  keyword = pharma,
  geo = "US",
  time = "today+5-y"
)

# Results
str(bp$interest_over_time)
str(bp$interest_by_region)

# Time series
time_df <- bp$interest_over_time %>%
  mutate(
    hits = as.numeric(hits),
    date = as.Date(date)
  )

write_csv(time_df, "data/big_pharma_cures_time.csv")

# Region data
region_df <- bp$interest_by_region %>%
  mutate(
    state_name = tolower(location),
    interest   = as.numeric(hits)
  ) %>%
  select(state_name, interest)

write_csv(region_df, "data/big_pharma_cures_region.csv")








