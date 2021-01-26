library(tidyverse)
library(janitor)

T <- read.csv("M:/COVID_dashboard/COVID_dashboard_test_results.csv",
              col.names = c("ID", "Test_Result", "Test_Date", "State", "At_Health_Center")) %>%
  mutate(Test_Result = factor(tolower(Test_Result), levels = c("p", "n", "i")),
         Test_Date = as.Date(Test_Date),
         At_Health_Center = case_when(
           At_Health_Center == "N" ~ FALSE,
           TRUE ~ TRUE
         )) %>%
  clean_names() %>%
  select(-state)

save(T, file = "T.Rdata")


