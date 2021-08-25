library(tidyverse)
library(lubridate)
library(janitor)

#Load raw data
load("data-raw/SF21.Rdata")
load("data/SF.Rdata")
load("data/SS.Rdata")
load("data-raw/E.Rdata")

#Important dates
f20 <- as.Date("2020-07-20")
s21 <- as.Date("2021-01-01")
f21 <- as.Date("2021-08-14")
load("data/this_day.Rdata")
yest <- this_day-1
last <- as.Date("2021-12-31")



