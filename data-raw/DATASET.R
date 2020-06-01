## code to prepare `macro` dataset goes here

# Load packages for data download and transformation
#install.packages("Quandl")
library(dplyr)
library(Quandl)
library(tidyr)

# Download data
macro <- Quandl("FRED/GDPC1", order = "asc",
               start_date = "1970-01-01", end_date = "2016-10-01")  %>%
  rename(date = Date,
         gdp = Value) %>%
  mutate(lgdp = log(gdp)) # Take logs


usethis::use_data(macro)
