library(tidyverse)
library(lubridate)
library(googlesheets4)

load("T_Pos.Rdata")
load("CS.Rdata")
load("CE.Rdata")
load("T.Rdata")
load("this_day.Rdata")

yest = this_day - 1

#T_Pos %>%
#  write.csv(file = "M:/COVID_dashboard/testing_numbers_by_day.csv")

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

#Positives by hour
#T_hour %>%
#  filter(!employee) %>%
#  group_by(id, t_date) %>%
#  summarise(cases = prod(test_result), test_time = min(test_time)) %>%
#  mutate(t_hour = factor(hour(test_time))) %>%
#  ungroup() %>%
#  group_by(t_date, t_hour)%>%
#  summarise(cases = sum(cases))%>%
#  pivot_wider(names_from = t_hour, values_from = cases,
#              values_fill = 0) %>%
#  select(t_date, as.character(seq(9,17))) %>%
#  write.csv(file = "M:/COVID_dashboard/positives_by_hour.csv")


#T_hour %>%
#  group_by(t_date, t_hour)%>%
#  summarise(total = n()) %>%
#  pivot_wider(names_from = t_hour, values_from = total,
#              values_fill = 0) %>%
#  select(t_date, as.character(seq(9,17))) %>%
#  write.csv(file = "M:/COVID_dashboard/tests_by_hour_including_2nd.csv")

ST_hour <- T_hour %>%
  filter(!employee) %>%
  group_by(id, t_date) %>%
  summarise(cases = prod(test_result), test_time = min(test_time)) %>%
  mutate(t_hour = factor(hour(test_time))) %>%
  ungroup() %>%
  group_by(t_date, t_hour)%>%
  summarise(total = n())%>%
  pivot_wider(names_from = t_hour, values_from = total,
              values_fill = 0) %>%
  select(t_date, as.character(seq(9,17)))

save(ST_hour, file = "ST_hour.Rdata")
#View(ST_hour)
#%>%   write.csv(file = "M:/COVID_dashboard/tests_by_hour.csv")

TCS <- T_hour %>%
  filter(!employee) %>%
  group_by(id, first_name, last_name, t_date) %>%
  summarise(cases = prod(test_result), test_time = min(test_time)) %>%
  mutate(t_hour = factor(hour(test_time))) %>%
  ungroup() %>%
  filter(cases > 0) %>%
  select(id, first_name, last_name, test_time)

#SS %>%
#  filter(test_result == "p",
#         location != "Before",
#         started > as_date("2021-01-23"),
#         end_date == Sys.Date() - 1)%>%
#  select(-end_date, -started) %>%
#  full_join(TCS, by = c("id", "first_name", "last_name")) %>%
#  write.csv(file = "M:/COVID_dashboard/cases_students.csv")

CS %>%
  full_join(TCS, by = c("id", "first_name", "last_name")) %>%
  filter(is.na(test_time) | is.na(test_result))%>%
  mutate(end_date = case_when(
    end_date >= yest ~ as.Date(NA),
    TRUE ~ end_date
  )) %>%
  sheet_write(ss = "https://docs.google.com/spreadsheets/d/1uFaRFBb7rykhLhOxwiWintvBILuO5wQMTdQ0oNaKkW8/edit#gid=0",
              sheet = 1)

CS %>%
  full_join(TCS, by = c("id", "first_name", "last_name")) %>%
  filter(is.na(test_time) | is.na(test_result))%>%
  mutate(end_date = case_when(
    end_date >= yest ~ as.Date(NA),
    TRUE ~ end_date
  )) %>%
  View()

CE %>%
  filter(End_Date >= yest) %>%
  select(-End_Date) %>%
  write.csv(file = "M:/COVID_dashboard/cases_employees.csv")
