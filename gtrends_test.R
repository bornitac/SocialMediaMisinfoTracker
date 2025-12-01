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
#4th Topic: Pizzagate



