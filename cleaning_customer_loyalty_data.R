library(moments)
library(dplyr)
library(tidyverse)
library(ggplot2)

setwd("/Users/alyssabueno/Desktop/airline_loyalty")
history <- read.csv("customer_loyalty_history.csv", stringsAsFactors = FALSE)

View(history)

colnames(history) <- tolower(colnames(history))
history <- history %>% rename(loyalty_number = loyalty.number)
history <- history %>% rename(postal_code = postal.code)
history <- history %>% rename(marital_status = marital.status)
history <- history %>% rename(loyalty_card = loyalty.card)
history <- history %>% rename(enrollment_type = enrollment.type)
history <- history %>% rename(enrollment_year = enrollment.year)
history <- history %>% rename(enrollment_month = enrollment.month)
history <- history %>% rename(cancellation_year = cancellation.year)
history <- history %>% rename(cancellation_month = cancellation.month)

##To Do:
##change column names to lowercase and snakecase
##change NA to 0 or NULL
##remove spaces from the postal codes
##check for skew, but it doesn't look like we'll need to normalize