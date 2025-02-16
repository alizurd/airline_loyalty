install.packages("janitor")
library(janitor)
library(moments)
library(dplyr)
library(tidyverse)
library(ggplot2)

setwd("/Users/alyssabueno/Desktop/airline_loyalty")
history <- read.csv("customer_loyalty_history.csv", stringsAsFactors = FALSE)

View(history)

history <- clean_names(history) # change column names to lowercase and snakecase

history[is.na(history)] <- 0 # change NA to 0 or NULL

history$postal_code <- gsub(" ", "", history$postal_code) # remove spaces from the postal codes

# making columns numeric
str(history)
history$loyalty_number <- as.numeric(history$loyalty_number)
history$salary <- as.numeric(history$salary)
history$clv <- as.numeric(history$clv)
history$enrollment_year <- as.numeric(history$enrollment_year)
history$enrollment_month <- as.numeric(history$enrollment_month)

history$salary <- abs(history$salary) # changing the negative salary values positive
                
# at this point i want to download data and explore in bigquery
write.csv(history, "loyalty_history.csv", na = "")
    
# check for skew
check_skewness <- function(history) {
  sapply(history, function(col) if (is.numeric(col)) skewness(col, na.rm = TRUE) else NA)
}
skew_values <- check_skewness(history)
print(skew_values)

# clv is positively skewed >3
# salary is positively skewed = 1.2
history$clv <- log1p(history$clv)
history$salary <- sqrt(log1p(history$salary))

zero_rows <- sum(history$salary == 0, na.rm = TRUE)
print(zero_rows)

history <- history %>% 
  filter(salary != 0) # filtering out rows with salary = 0



