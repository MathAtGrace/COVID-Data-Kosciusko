
T_Pos %>%
  write.csv(file = "M:/COVID_dashboard/testing_numbers_by_day.csv")

load("Testing_Data.R")

T_hour <- read.csv("M:/COVID_dashboard/COVID_dashboard_test_results.csv") %>%
  clean_names() %>%
  rename(id = i_id_num,
         test_date = test_dt,
         at_hc = tested_at_health_center) %>%
  mutate(test_result = (test_result == "P"),
         test_date = as_datetime(test_date),
         employee = (employee == "Y"),
         t_date = date(test_date),
         t_hour = factor(hour(test_date)),
         test_result = case_when(
           id == 1610566 ~ FALSE,    #Fix data entry error
           TRUE ~ test_result
         ))%>%
  select(-sent_to_state_dt) %>%
  separate(last_first_mi, c("last_name", "first_name"), extra = "drop")

#T_hour %>%
#  group_by(t_date, t_hour)%>%
#  summarise(cases = sum(test_result)) %>%
#  pivot_wider(names_from = t_hour, values_from = cases,
#              values_fill = 0) %>%
#  select(t_date, as.character(seq(9,17))) %>%
#  write.csv(file = "M:/COVID_dashboard/positives_by_hour_including_2nd.csv")

T_hour %>%
  filter(!employee) %>%
  group_by(id, t_date) %>%
  summarise(cases = prod(test_result), test_date = min(test_date)) %>%
  mutate(t_hour = factor(hour(test_date))) %>%
  ungroup() %>%
  group_by(t_date, t_hour)%>%
  summarise(cases = sum(cases))%>%
  pivot_wider(names_from = t_hour, values_from = cases,
              values_fill = 0) %>%
  select(t_date, as.character(seq(9,17))) %>%
  write.csv(file = "M:/COVID_dashboard/positives_by_hour.csv")


#T_hour %>%
#  group_by(t_date, t_hour)%>%
#  summarise(total = n()) %>%
#  pivot_wider(names_from = t_hour, values_from = total,
#              values_fill = 0) %>%
#  select(t_date, as.character(seq(9,17))) %>%
#  write.csv(file = "M:/COVID_dashboard/tests_by_hour_including_2nd.csv")

T_hour %>%
  filter(!employee) %>%
  group_by(id, t_date) %>%
  summarise(cases = prod(test_result), test_date = min(test_date)) %>%
  mutate(t_hour = factor(hour(test_date))) %>%
  ungroup() %>%
  group_by(t_date, t_hour)%>%
  summarise(total = n())%>%
  pivot_wider(names_from = t_hour, values_from = total,
              values_fill = 0) %>%
  select(t_date, as.character(seq(9,17))) %>%
  write.csv(file = "M:/COVID_dashboard/tests_by_hour.csv")

TCS <- T_hour %>%
  filter(!employee) %>%
  group_by(id, t_date) %>%
  summarise(cases = prod(test_result), test_date = min(test_date)) %>%
  mutate(t_hour = factor(hour(test_date))) %>%
  ungroup() %>%
  filter(cases > 0) %>%
  select(id, test_date)

SS %>%
  filter(test_result == "p", location != "Before")%>%
  full_join(TCS, by = "id") %>%
  write.csv(file = "M:/COVID_dashboard/cases_students.csv")

CE %>%
  filter(End_Date >= yest) %>%
  select(-End_Date) %>%
  write.csv(file = "M:/COVID_dashboard/cases_employees.csv")
