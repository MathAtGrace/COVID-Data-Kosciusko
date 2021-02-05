library(tidyverse)
library(janitor)
library(lubridate)

T <- read.csv("M:/COVID_dashboard/COVID_dashboard_test_results.csv") %>%
  clean_names()%>%
  rename(id = i_id_num,
         at_hc = tested_at_health_center) %>%
  mutate(test_time = as_datetime(test_dt),
         employee = case_when(employee == "Y" ~ TRUE, TRUE ~ FALSE),
         test_result = case_when(
           id == 1618586 & test_time == as_datetime("2021-02-03 16:04:25") ~ FALSE,
           id == 1610566 & test_time == as_datetime("2021-01-25 12:59:04") ~ FALSE,
           TRUE ~ (test_result == "P")
         ))%>%
  select(-sent_to_state_dt)

save(T, file = "T.Rdata")


