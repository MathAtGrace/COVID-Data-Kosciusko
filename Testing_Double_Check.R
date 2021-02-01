
T_Pos %>%
  write.csv(file = "M:/COVID_dashboard/testing_numbers_by_day.csv")

T_hour <- read.csv("M:/COVID_dashboard/COVID_dashboard_test_results.csv",
              col.names = c("id", "test_result", "test_date", "state", "at_health_center")) %>%
  mutate(test_date = as_datetime(test_date),
         test_result = case_when(
           test_result == "N" ~ FALSE,
           TRUE ~ TRUE
         ),
         at_health_center = case_when(
           at_health_center == "N" ~ FALSE,
           TRUE ~ TRUE
         ),
         t_date = date(test_date),
         t_hour = factor(hour(test_date))) %>%
  select(-state) %>%
  filter(id != 1610566) #Fix data entry error

#T_hour %>%
#  group_by(t_date, t_hour)%>%
#  summarise(cases = sum(test_result)) %>%
#  pivot_wider(names_from = t_hour, values_from = cases,
#              values_fill = 0) %>%
#  select(t_date, as.character(seq(9,17))) %>%
#  write.csv(file = "M:/COVID_dashboard/positives_by_hour_including_2nd.csv")

T_hour %>%
  group_by(id, t_date) %>%
  summarise(cases = prod(test_result), test_date = min(test_date)) %>%
  mutate(t_hour = factor(hour(test_date))) %>%
  ungroup() %>%
  group_by(t_date, t_hour)%>%
  summarise(cases = sum(cases))%>%
  pivot_wider(names_from = t_hour, values_from = cases,
              values_fill = 0) %>%
  select(t_date, as.character(seq(9,17))) %>%
  write.csv(file = "M:/COVID_dashboard/positives_by_hour_unique.csv")


#T_hour %>%
#  group_by(t_date, t_hour)%>%
#  summarise(total = n()) %>%
#  pivot_wider(names_from = t_hour, values_from = total,
#              values_fill = 0) %>%
#  select(t_date, as.character(seq(9,17))) %>%
#  write.csv(file = "M:/COVID_dashboard/tests_by_hour_including_2nd.csv")

T_hour %>%
  group_by(id, t_date) %>%
  summarise(cases = prod(test_result), test_date = min(test_date)) %>%
  mutate(t_hour = factor(hour(test_date))) %>%
  ungroup() %>%
  group_by(t_date, t_hour)%>%
  summarise(total = n())%>%
  pivot_wider(names_from = t_hour, values_from = total,
              values_fill = 0) %>%
  select(t_date, as.character(seq(9,17))) %>%
  write.csv(file = "M:/COVID_dashboard/tests_by_hour_unique.csv")

TCS <- T_hour %>%
  group_by(id, t_date) %>%
  summarise(cases = prod(test_result), test_date = min(test_date)) %>%
  mutate(t_hour = factor(hour(test_date))) %>%
  ungroup() %>%
  filter(cases > 0) %>%
  select(id, test_date)

SS %>%
  filter(test_result == "p", location != "Before")%>%
  full_join(TCS, by = "id") %>%
  write.csv(file = "M:/COVID_dashboard/cases_students_combined.csv")

CE %>%
  filter(End_Date >= yest) %>%
  select(-End_Date) %>%
  write.csv(file = "M:/COVID_dashboard/cases_employees.csv")
