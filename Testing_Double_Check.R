
T_Pos %>%
  write.csv(file = "M:/COVID_dashboard/testing_numbers_by_day.csv")

load("T.Rdata")

T_hour <- T %>%
  mutate(t_date = date(test_time),
         t_hour = factor(hour(test_time)))%>%
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
  summarise(cases = prod(test_result), test_time = min(test_time)) %>%
  mutate(t_hour = factor(hour(test_time))) %>%
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
  summarise(cases = prod(test_result), test_time = min(test_time)) %>%
  mutate(t_hour = factor(hour(test_time))) %>%
  ungroup() %>%
  group_by(t_date, t_hour)%>%
  summarise(total = n())%>%
  pivot_wider(names_from = t_hour, values_from = total,
              values_fill = 0) %>%
  select(t_date, as.character(seq(9,17))) %>%
  write.csv(file = "M:/COVID_dashboard/tests_by_hour.csv")

TCS <- T_hour %>%
  filter(!employee) %>%
  group_by(id, first_name, last_name, t_date) %>%
  summarise(cases = prod(test_result), test_time = min(test_time)) %>%
  mutate(t_hour = factor(hour(test_time))) %>%
  ungroup() %>%
  filter(cases > 0) %>%
  select(id, first_name, last_name, test_time)

SS %>%
  filter(test_result == "p",
         location != "Before",
         started > as_date("2021-01-23"))%>%
  full_join(TCS, by = c("id", "first_name", "last_name")) %>%
  write.csv(file = "M:/COVID_dashboard/cases_students.csv")

SS %>%
  filter(test_result == "p",
         location != "Before",
         started > as_date("2021-01-23"))%>%
  select(-end_date) %>%
  full_join(TCS, by = c("id", "first_name", "last_name")) %>%
  filter(is.na(test_time) | is.na(test_result)) %>%
  write.csv(file = "M:/COVID_dashboard/cases_students_problems.csv")

SS %>%
  filter(test_result == "p",
         location != "Before",
         started > as_date("2021-01-23"))%>%
  select(-end_date) %>%
  full_join(TCS, by = c("id", "first_name", "last_name")) %>%
  filter(is.na(test_time) | is.na(test_result)) %>%
  View()

CE %>%
  filter(End_Date >= yest) %>%
  select(-End_Date) %>%
  write.csv(file = "M:/COVID_dashboard/cases_employees.csv")
