library(tidyverse)
library(janitor)
library(lubridate)

T <- read.csv("M:/COVID_dashboard/COVID_dashboard_test_results.csv") %>%
  clean_names()%>%
  rename(id = i_id_num,
         at_hc = tested_at_health_center) %>%
  mutate(test_result = (test_result == "P"),
         test_time = as_datetime(test_dt),
         employee = case_when(employee == "Y" ~ TRUE, TRUE ~ FALSE))%>%
  select(-sent_to_state_dt)

save(T, file = "T.Rdata")


