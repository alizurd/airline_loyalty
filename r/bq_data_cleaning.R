# cleaning datasets for BQ

install.packages("janitor")
install.packages("lattice")
install.packages("pROC")
install.packages("party")

library(janitor)
library(moments)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)
library(pROC)
library(party)

setwd("/Users/alyssabueno/Desktop/airline_loyalty")
history <- read.csv("loyalty_history.csv", stringsAsFactors = FALSE)
flights <- read.csv("flight_activity.csv", stringsAsFactors = FALSE)

# change column names to lowercase and snakecase
history <- clean_names(history) 
flights <- clean_names(flights)

# apply more transformations
history[is.na(history)] <- 0 # change NA to 0 or NULL
flights[is.na(flights)] <- 0

history$postal_code <- gsub(" ", "", history$postal_code) # remove spaces from the postal codes

history$salary <- abs(history$salary)

# reformatting columns

history <- history %>%
  mutate(
    loyalty_number = as.numeric(loyalty_number),
    salary = as.numeric(salary),
    clv = as.numeric(clv),
    enrollment_year = as.numeric(enrollment_year),
    enrollment_month = as.numeric(enrollment_month),
    cancellation_month = as.numeric(cancellation_month),
    cancellation_year = as.numeric(cancellation_year),
    country = as.character(country),
    province = as.character(province),
    city = as.character(city),
    postal_code = as.character(postal_code),
    gender = as.character(gender),
    education = as.character(education),
    marital_status = as.character(marital_status),
    loyalty_card = as.character(loyalty_card),
    enrollment_type = as.character(enrollment_type)
  )

flights <- flights %>%
  mutate(
    year = as.numeric(year),
    month = as.numeric(month),
    total_flights = as.numeric(total_flights),
    distance = as.numeric(distance),
    points_accumulated = as.numeric(points_accumulated),
    points_redeemed = as.numeric(points_redeemed),
    dollar_cost_points_redeemed = as.numeric(dollar_cost_points_redeemed)
  )


write.csv(flights, "bq_flights.csv", na = "")
write.csv(history, "bq_history.csv", na = "")

