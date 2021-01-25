require(tidyverse, janitor)

load("T.RData")
T3 <- read.csv("M:/COVID_dashboard/COVID_dashboard_test_results.csv",
              col.names = c("ID", "Test_Result", "Test_Date", "State", "At_Health_Center")) %>%
  mutate(Test_Result = factor(tolower(Test_Result), levels = c("p", "n", "i")),
         Test_Date = as.Date(Test_Date),
         State = case_when(
           is.na(State) ~ FALSE,
           TRUE ~ State
         ),
         At_Health_Center = case_when(
           At_Health_Center == "N" ~ FALSE,
           TRUE ~ TRUE
         )) %>%
  clean_names() %>%
  select(-state)

T <- bind_rows(T, T3) %>%
  distinct(ID, .keep_all = TRUE)

save(T, file = "T.Rdata")


