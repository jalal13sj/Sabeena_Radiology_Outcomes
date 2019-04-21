install.packages("haven")
library(haven)

setwd("/Users/macbook/Documents/McGill School/Sabeena_Radiology_Outcomes")

radio_data <-read_dta("june9th.dta")

head(radio_data)

str(radio_data)
 library("Hmisc")

describe(radio_data)

nrow(radio_data)

install.packages("rio")
library(rio)

convert("june9th.dta", "june9th.csv")

