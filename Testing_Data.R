require(tidyverse)

T <- read.csv("M:/COVID_dashboard/COVID_dashboard_test_results.csv",
              col.names = c("ID", "Test_Result", "Test_Date", "State", "At_Health_Center")) %>%
  mutate(Test_Result = factor(tolower(Test_Result), levels = c("p", "n")),
         Test_Date = as.Date(Test_Date),
         State = case_when(
           is.na(State) ~ FALSE,
           TRUE ~ State
         ),
         At_Health_Center = case_when(
           At_Health_Center == "N" ~ FALSE,
           TRUE ~ TRUE
         )) %>%
  select(-State)

save(T, file = "T.Rdata")
