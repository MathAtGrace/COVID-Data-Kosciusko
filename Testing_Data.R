library(tidyverse)
library(janitor)

T <- read.csv("M:/COVID_dashboard/COVID_dashboard_test_results.csv") %>%
  clean_names()%>%
  rename(id = i_id_num,
         test_date = test_dt,
         at_hc = tested_at_health_center) %>%
  mutate(test_result = (test_result == "P"),
         test_date = as.Date(test_date),
         employee = case_when(employee == "Y" ~ TRUE, TRUE ~ FALSE))%>%
  select(-sent_to_state_dt)

save(T, file = "T.Rdata")


