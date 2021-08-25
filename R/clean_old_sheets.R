library(tidyverse)
library(lubridate)
library(janitor)

#Load raw data
load("data-raw/SF.Rdata")
load("data-raw/SS.Rdata")

#Important dates
f20 <- as.Date("2020-07-20")
f20_end <- as.Date("2020-12-15")
s21 <- as.Date("2021-01-01")
s21_end <- as.Date("2021-05-15")
f21 <- as.Date("2021-08-14")
load("data/this_day.Rdata")
yest <- this_day-1
last <- as.Date("2021-12-31")

SS <- S_spring21 %>%
  clean_names() %>%#In case of emergency: unnest_longer(problem_column,indices_include = FALSE)
  select(id:first_name, quarantine_isolation_location:test_result,
         actual_release_from_quarantine_isolation,
         complete, positive_before_coming) %>%
  #  filter(is.na(positive_at_home)) %>%
  rename(location = quarantine_isolation_location,
         tqs = travel_quarantine_start_date,
         exposure = last_exposure_date,
         symptoms = symptom_start_date,
         started = quarantine_isolation_started,
         end_date = actual_release_from_quarantine_isolation,
         completed = complete) %>%
  mutate(tqs = convert_to_date(tqs),
         exposure = convert_to_date(exposure),
         symptoms = convert_to_date(symptoms),
         test_date = convert_to_date(test_date),
         end_date = convert_to_date(end_date),
         started = pmin(tqs, test_date, convert_to_date(started), na.rm = TRUE)) %>%
  mutate(test_result = tolower(substring(test_result, 1, 1))) %>%
  mutate(type = factor(case_when(
    test_result == "p" ~ "Isolation",
    !is.na(symptoms) ~ "Isolation",
    TRUE ~ "Quarantine"))) %>%
  mutate(started = case_when(
    is.na(started) & !is.na(exposure) ~ exposure,
    is.na(started) & test_result == "p" & !is.na(end_date) ~ end_date - 10,
    started > end_date ~ end_date,
    TRUE ~ started
  )) %>%
  filter(!is.na(started) & started <= s21_end) %>%
  mutate(end_date = case_when(
    is.na(end_date) | is.na(completed) ~ pmin(started + 14, s21_end),
    end_date > s21_end ~ s21_end,
    TRUE ~ end_date
  )) %>%
  mutate(location = factor(case_when(
    !is.na(tqs) & type != "Isolation" ~ "Grace_travel",
    !is.na(positive_before_coming) ~ "Before",
    tolower(substring(location, 1, 4)) == "home" ~ "Home",
    tolower(substring(location, 1, 4)) == "off " ~ "Home",
    tolower(substring(location, 1, 4)) == "fami" ~ "Home",
    tolower(substring(location, 1, 4)) == "poss" ~ "Home",
    tolower(substring(location, 1, 4)) == "comm" ~ "Home",
    TRUE ~ "Grace"), levels = c("Before", "Home", "Grace_travel", "Grace"))) %>%
  select(id, first_name, last_name, location, type, test_result, test_date, started, end_date) %>%
  #Take out weird ones
  filter(!(id == 1590824 & started == as.Date("2021-02-08")))
save(SS, file = "data/SS.Rdata")

SF <- S_fall20 %>%
  clean_names() %>%
  select(id:first_name, location,
         travel_quarantine_start_date:nurse_approved_release_date_estimate_only_until_final,
         completed) %>%
  rename(tqs = travel_quarantine_start_date,
         exposure = last_exposure_date,
         q_started = quarantine_started,
         i_started = isolated_for_covid_19_symptoms_or_positive_test,
         symptoms = date_symptoms_started,
         end_date = nurse_approved_release_date_estimate_only_until_final,
         result_date = covid_19_positive_date)%>%
  mutate(tqs = convert_to_date(tqs),
         exposure = convert_to_date(exposure),
         q_started = convert_to_date(q_started),
         i_started = convert_to_date(i_started),
         symptoms = convert_to_date(symptoms),
         test_date = convert_to_date(test_date),
         result_date = convert_to_date(result_date),
         end_date = convert_to_date(end_date),
         test_result = factor(tolower(substring(test_result, 1, 1)))) %>%
  mutate(type = factor(case_when(
    test_result == "p" ~ "Isolation",
    !is.na(i_started) ~ "Isolation",
    TRUE ~ "Quarantine")),
    started = pmin(tqs, q_started, i_started, test_date, result_date, na.rm = TRUE)) %>%
  mutate(started = case_when(
    is.na(started) & !is.na(exposure) ~ exposure,
    TRUE ~ started
  )) %>%
  select(-q_started, -i_started) %>%
  mutate(started = case_when(
    is.na(started) & !is.na(end_date) & !is.na(exposure) & type == "Quarantine" ~ exposure,
    TRUE ~ started
  )) %>%
  filter(!is.na(started)) %>%
  mutate(end_date = case_when(
    is.na(end_date) | is.na(completed) ~ started + 14,
    TRUE ~ end_date
  )) %>%
  mutate(location = factor(case_when(
    !is.na(tqs) & type != "Isolation" ~ "Grace_travel",
    tolower(substring(location, 1, 4)) == "home" ~ "Home",
    tolower(substring(location, 1, 4)) == "off " ~ "Home",
    tolower(substring(location, 1, 4)) == "fami" ~ "Home",
    TRUE ~ "Grace"), levels = c("Home", "Grace_travel", "Grace"))) %>%
  select(id, first_name, last_name, location, type, test_result, test_date, started, end_date)

save(SF, file = "data/SF.Rdata")
