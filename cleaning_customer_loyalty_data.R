library(moments)
library(dplyr)
library(tidyverse)
library(ggplot2)

setwd("/Users/alyssabueno/Desktop/airline_loyalty")
history <- read.csv("customer_loyalty_history.csv", stringsAsFactors = FALSE)

View(history)

##To Do:
##change column names to lowercase and snakecase
##change NA to 0 or NULL
##remove spaces from the postal codes
##check for skew, but it doesn't look like we'll need to normalize