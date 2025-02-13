install.packages("janitor")
library(janitor)
library(moments)
library(dplyr)
library(tidyverse)
library(ggplot2)

setwd("/Users/alyssabueno/Desktop/airline_loyalty")
history <- read.csv("customer_loyalty_history.csv", stringsAsFactors = FALSE)

View(history)

# change column names to lowercase and snakecase
history <- clean_names(history)

# change NA to 0 or NULL
history[is.na(history)] <- 0

# remove spaces from the postal codes
history$postal_code <- gsub(" ", "", history$postal_code)                         
                
# at this point i want to download data and explore in bigquery
write.csv(history, "loyalty_history.csv", na = "")
    
# check for skew
check_skewness <- function(history) {
  sapply(history, function(col) if (is.numeric(col)) skewness(col, na.rm = TRUE) else NA)
}

# clv is positively skewed >3
# salary is positively skewed = 1.2
history$clv <- log1p(history$clv)
history$salary <- sqrt(log1p(history$salary))

# sqrt transformation resulted warning message: NaNs
# checking how many 0s are in column
summary(history$salary) # there are 20 0s


