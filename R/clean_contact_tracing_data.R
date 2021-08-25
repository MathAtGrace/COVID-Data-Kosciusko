library(tidyverse)
library(lubridate)
library(janitor)

#Load raw data
load("data-raw/SF21.Rdata")
load("data/SF.Rdata")
load("data/SS.Rdata")
load("data-raw/E.Rdata")

#Important dates
f20 <- as.Date("2020-07-20")
s21 <- as.Date("2021-01-01")
f21 <- as.Date("2021-08-14")
load("data/this_day.Rdata")
yest <- this_day-1
last <- as.Date("2021-12-31")

SF21 <- S_fall21 %>%
  clean_names() %>%#In case of emergency: unnest_longer(problem_column,indices_include = FALSE)
  select(id:first_name, vax_proof,
         quarantine_isolation_location:test_result,
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
  mutate(exposure = convert_to_date(exposure),
         symptoms = convert_to_date(symptoms),
         test_date = convert_to_date(test_date),
         end_date = convert_to_date(end_date),
         started = convert_to_date(started),
         started = pmin(test_date, started, na.rm = TRUE)) %>%
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
  filter(!is.na(started) & started <= yest) %>%
  mutate(end_date = case_when(
    is.na(end_date) | is.na(completed) ~ pmin(started + 14, yest),
    end_date > yest ~ yest,
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
  select(id, first_name, last_name, location, vax_proof,
         type, test_result, test_date, started, end_date)
save(SF21, file = "data/SF21.Rdata")

S_days_fill <- SF21 %>%
  rowwise() %>%
  do(data.frame(.[1:7], date = seq(.$started, .$end_date, by = "1 day")))
  
save(S_days_fill, file = "data/S_days_fill.Rdata")

CS <- SF21 %>%
  filter(test_result == "p",
         location != "Before"
         )

save(CS, file = "data/CS.Rdata")

S_all <- bind_rows(SS, SF, SF21)
save(S_all, file = "data/S_all.Rdata")

E_all <- E %>%
  #Fix missing end dates for E if a return to work date exists
  mutate(End_Date = case_when(
    is.na(End_Date) & !is.na(Return_Date) ~ Return_Date,
    TRUE ~ End_Date
  )) %>%
  #Fix different ways of entering positives and negatives
  mutate(Test_Result = tolower(substring(E$Test_Result, 1, 1))) %>%
  #Don't count quarantines that are dated after isolation
  mutate(Quarantine = case_when(
    !is.na(Quarantine) & !is.na(Isolated) & (Quarantine > Isolated) ~ as.Date(NA),
    TRUE ~ Quarantine
  )) %>%
  mutate(End_Date = case_when(
    is.na(End_Date) | is.na(Complete) ~ min(max(Quarantine, Isolated, Result_Date,
                                                Test_Date, na.rm = TRUE) + 14, yest),
    End_Date > yest ~ yest,
    TRUE ~ End_Date
  )) %>%
  mutate(Isolated = case_when(
    Test_Result == "p" & is.na(Isolated) & !is.na(Quarantine) ~ Quarantine,
    Test_Result == "p" & is.na(Isolated) & !is.na(Test_Date) ~ Test_Date,
    Test_Result == "p" & is.na(Isolated) & !is.na(Result_Date) ~ Result_Date,
    TRUE ~ Isolated
  ))%>%
  filter(pmin(Quarantine, Isolated, Result_Date, na.rm = TRUE) <= yest)

E_f21 <- E_all %>%
  filter(pmax(Quarantine, Isolated, Test_Date, Result_Date, na.rm = TRUE) > f21)

##############################################################################
#Student Quarantine

#Note to self: the code paragraph below was for an atempt to create a spreadsheet detailing numbers.
#However, I see no need to pursue that any further. -Ryan 1/15/21
QS_by_day <- S_days_fill %>%
  filter(type == "Quarantine") %>%
  group_by(date, location, .drop = FALSE) %>%
  summarise(n = n()) %>%
  group_by(location) %>%
  complete(date = seq(f21, yest, by = "day"), fill = list(n = 0)) %>%
  pivot_wider(names_from = location, values_from = n)%>%
  mutate(s_total_quar = Grace + Home) %>%
  ungroup() %>%
  rename(S_Quar_Grace = Grace, S_Quar_Home = Home, S_Quar_Travel = Grace_travel)

QS_yest <- QS_by_day %>%
  filter(date == yest) %>%
  mutate(Type = str_glue("Active on ", format(yest, "%a %b %d"))) %>%
  rename(Grace = S_Quar_Grace, Home = S_Quar_Home, Grace_travel = S_Quar_Travel, Total = s_total_quar) %>%
  select(Type, Total, Home, Grace, Grace_travel)

QS_f21_total <- SF21 %>%
  filter(type == "Quarantine") %>%
  group_by(location, .drop = FALSE)%>%
  summarise(n = n()) %>%
  pivot_wider(names_from = location, values_from = n) %>%
  mutate(Total = Grace + Home) %>%
  ungroup() %>%
  mutate(Type = "Fall Semester Total") %>%
  select(Type, Total, Home, Grace, Grace_travel)

QS_all_total <- S_all %>%
  filter(type == "Quarantine") %>%
  #Don't count quarantines within 10 days of isolation, and change End Dates 
  group_by(location, .drop = FALSE)%>%
  summarise(n = n()) %>%
  pivot_wider(names_from = location, values_from = n) %>%
  mutate(Total = Grace + Home) %>%
  ungroup() %>%
  mutate(Type = "Total of Last Two Years") %>%
  select(Type, Total, Home, Grace, Grace_travel)

QS_all_unique <- S_all %>%
  filter(type == "Quarantine") %>%
  distinct(first_name, last_name, location, .keep_all = TRUE)%>%
  group_by(location, .drop = FALSE)%>%
  summarise(n = n()) %>%
  pivot_wider(names_from = location, values_from = n) %>%
  mutate(Total = Grace + Home) %>%
  ungroup() %>%
  mutate(Type = "Total Unique Students") %>%
  select(Type, Total, Home, Grace, Grace_travel)

QS_Tab <- bind_rows(QS_yest, QS_f21_total, QS_all_total, QS_all_unique)
save(QS_Tab, file = "data/QS_Tab.Rdata")

#Maximum count needed for graphs
mQS <- S_days_fill %>%
  filter(type == "Quarantine", location != "Grace_travel") %>%
  group_by(date, .drop = FALSE) %>%
  summarise(n = n()) %>%
  slice(which.max(n)) %>%
  pull(n)
save(mQS, file = "data/mQS.Rdata")

##############################################################################
#Student Isolation Numbers

IS_yest <- S_days_fill %>%
  filter(type == "Isolation" & date == yest) %>%
  group_by(date, location, .drop = FALSE)%>%
  summarise(n = n())%>%
  pivot_wider(names_from = location, values_from = n) %>%
  mutate(Total = Grace + Home) %>%
  ungroup() %>%
  mutate(Type = str_glue("Active on ", format(yest, "%a %b %d"))) %>%
  select(Type, Total, Grace, Home, Before)

IS_f21_total <- SF21 %>%
  filter(type == "Isolation") %>%
  group_by(location, .drop = FALSE)%>%
  summarise(n = n()) %>%
  pivot_wider(names_from = location, values_from = n) %>%
  mutate(Total = Grace + Home) %>%
  ungroup() %>%
  mutate(Type = "Fall Semester Total") %>%
  select(Type, Total, Grace, Home, Before)

IS_all_total <- S_all %>%
  filter(type == "Isolation") %>%
  group_by(location, .drop = FALSE)%>%
  summarise(n = n()) %>%
  pivot_wider(names_from = location, values_from = n) %>%
  mutate(Total = Grace + Home) %>%
  ungroup() %>%
  mutate(Type = "Total of Last Two Years") %>%
  select(Type, Total, Grace, Home, Before)

IS_all_unique <- S_all %>%
  filter(type == "Isolation") %>%
  distinct(first_name, last_name, .keep_all = TRUE)%>%
  group_by(location, .drop = FALSE)%>%
  summarise(n = n()) %>%
  pivot_wider(names_from = location, values_from = n) %>%
  mutate(Total = Grace + Home) %>%
  ungroup() %>%
  mutate(Type = "Total Unique Students") %>%
  select(Type, Total, Grace, Home, Before)

IS_Tab <- bind_rows(IS_yest, IS_f21_total, IS_all_total, IS_all_unique)
save(IS_Tab, file = "data/IS_Tab.Rdata")

#Maximum count needed for graphs
mIS <- S_days_fill %>%
  filter(type == "Isolation", location != "Before") %>%
  group_by(date, .drop = FALSE) %>%
  summarise(n = n()) %>%
  slice(which.max(n)) %>%
  pull(n)
save(mIS, file = "data/mIS.Rdata")

#########################################################################
#Student Cases

#Fill in dates between start of student quarantine and end date,
#Then total for each day

CS_yest <- S_days_fill %>%
  filter(test_result == "p", date == yest) %>%
  group_by(date, location, .drop = FALSE) %>%
  summarise(n = n())%>%
  pivot_wider(names_from = location, values_from = n) %>%
  mutate(Total = Grace + Home) %>%
  ungroup() %>%
  mutate(Type = str_glue("Active on ", format(yest, "%a %b %d"))) %>%
  select(Type, Total, Grace, Home, Before)

CS_f21_total <- SF21 %>%
  filter(test_result == "p") %>%
  group_by(location, .drop = FALSE)%>%
  summarise(n = n()) %>%
  pivot_wider(names_from = location, values_from = n) %>%
  mutate(Total = Grace + Home) %>%
  ungroup() %>%
  mutate(Type = "Fall Semester Total") %>%
  select(Type, Total, Grace, Home, Before)

CS_all_total <- S_all %>%
  filter(test_result == "p") %>%
  group_by(location, .drop = FALSE)%>%
  summarise(n = n()) %>%
  pivot_wider(names_from = location, values_from = n) %>%
  mutate(Total = Grace + Home) %>%
  ungroup() %>%
  mutate(Type = "20-21 Academic Year Total") %>%
  select(Type, Total, Grace, Home, Before)

CS_Tab <- bind_rows(CS_yest, CS_f21_total, CS_all_total)
save(CS_Tab, file = "data/CS_Tab.Rdata")

#Maximum count needed for graphs
mCS <- S_days_fill %>%
  filter(test_result == "p", location != "Before") %>%
  group_by(date, .drop = FALSE) %>%
  summarise(n = n()) %>%
  slice(which.max(n)) %>%
  pull(n)
save(mCS, file = "data/mCS")

################################################################################
#Employee Data

#Quarantine
#Fill in dates between start of student quarantine and end date
QE <- E_f21 %>%
  filter(!is.na(Quarantine)) %>%
  #Don't count quarantines within 10 days of isolation, and change End Dates 
  mutate(End_Date = case_when(
    !is.na(Isolated) & (Isolated >= Quarantine) ~ Isolated,
    TRUE ~ End_Date),
    Quarantine = case_when(
      !is.na(Isolated) & (abs(Quarantine - Isolated) < 10) & (Quarantine > Isolated) ~ as.Date(NA),
      TRUE ~ Quarantine)) %>%
  drop_na(Quarantine) %>%
  select(Last_Name:First_Name, Quarantine, End_Date)

#Then total for each day
QE_days_fill <- QE %>%
  rename(Date = Quarantine) %>%
  rowwise() %>%
  do(data.frame(.[1:2], Date = seq(.$Date, .$End_Date, by = "1 day"))) %>%
  tibble()

QE_yest <- QE_days_fill %>%
  #The line below is to create bogus data so that the summarization works
  add_row(Last_Name = NA, First_Name = NA, Date = yest) %>%
  filter(Date == yest) %>%
  group_by(Date, .drop = FALSE) %>%
  summarise(Quarantine = n()) %>%
  ungroup() %>%
  mutate(Quarantine = Quarantine-1) %>% #Correct for the bogus data
  mutate(Type = str_glue("Active on ", format(yest, "%a %b %d"))) %>%
  select(Type, Quarantine)

QE_f21_total <- QE %>%
  summarise(Quarantine = n()) %>%
  mutate(Type = "Fall Semester Total") %>%
  select(Type, Quarantine)

QE_all_total <- E_all %>%
  filter(!is.na(Quarantine)) %>%
  #Don't count quarantines within 10 days of isolation, and change End Dates 
  mutate(End_Date = case_when(
    !is.na(Isolated) & (Isolated > Quarantine) ~ Isolated,
    TRUE ~ End_Date),
    Quarantine = case_when(
      !is.na(Isolated) & (abs(Quarantine - Isolated) < 10) & (Quarantine > Isolated) ~ as.Date(NA),
      TRUE ~ Quarantine)) %>%
  drop_na(Quarantine) %>%
  select(Last_Name:First_Name, Quarantine, End_Date) %>%
  summarise(Quarantine = n()) %>%
  mutate(Type = "Total of Last Two Years") %>%
  select(Type, Quarantine)

QE_Tab <- bind_rows(QE_yest, QE_f21_total, QE_all_total)
save(QE_Tab, file = "data/QE_Tab.Rdata")


#Maximum count needed for graphs
mQE <- QE_days_fill %>%
  group_by(Date, .drop = FALSE) %>%
  summarise(n = n()) %>%
  slice(which.max(n)) %>%
  pull(n)
save(mQE, file = "data/mQE.Rdata")

############################################################################
#Employee Isolation Numbers

IE <- E_f21 %>%
  filter(!is.na(Isolated)) %>%
  #Don't count quarantines within 10 days of isolation, and change End Dates 
  select(Last_Name:First_Name, Isolated, End_Date)

IE_days_fill <- IE%>%
  rowwise() %>%
  do(data.frame(.[1:2], Date = seq(.$Isolated, .$End_Date, by = "1 day"))) %>%
  tibble()

IE_yest <- IE_days_fill %>%
  #The line below is to create bogus data so that the summarization works
  add_row(Last_Name = NA, First_Name = NA, Date = yest) %>%
  filter(Date == yest) %>%
  group_by(Date, .drop = FALSE) %>%
  summarise(Isolated = n()) %>%
  ungroup() %>%
  mutate(Isolated = Isolated-1) %>%
  mutate(Type = str_glue("Active on ", format(yest, "%a %b %d"))) %>%
  select(Type, Isolated)

IE_f21_total <- IE %>%
  summarise(Isolated = n()) %>%
  ungroup() %>%
  mutate(Type = "Fall Semester Total") %>%
  select(Type, Isolated)

IE_all_total <- E_all %>%
  filter(!is.na(Isolated)) %>%
  #Don't count quarantines within 10 days of isolation, and change End Dates 
  select(Last_Name:First_Name, Isolated, End_Date)%>%
  summarise(Isolated = n()) %>%
  mutate(Type = "Total of Last Two Years") %>%
  select(Type, Isolated)

IE_Tab <- bind_rows(IE_yest, IE_f21_total, IE_all_total)
save(IE_Tab, file = "data/IE_Tab.Rdata")

#Maximum count needed for graphs
mIE <- IE_days_fill %>%
  group_by(Date, .drop = FALSE) %>%
  summarise(n = n()) %>%
  slice(which.max(n)) %>%
  pull(n)
save(mIE, file = "data/mIE.Rdata")

#########################################################################
#Employee Cases

#Fill in dates between start of student quarantine and end date,
#Then total for each day
CE <- E_f21 %>%
  filter(Test_Result == "p") %>%
  #Fill in missing Result Dates 
  mutate(Test_Date = case_when(
    !is.na(Test_Date) ~ Test_Date,
    !is.na(Result_Date) ~ Result_Date,
    !is.na(Isolated) ~ Isolated,
    !is.na(Quarantine) ~ Quarantine,
    TRUE ~ yest - 1  #No data defaults to 2 days ago
  )) %>%
  filter(Test_Date <= yest) %>%
  select(Last_Name:First_Name, Test_Date, End_Date)

save(CE, file = "data/CE.Rdata")

CE_days_fill <- CE %>%
  rowwise() %>%
  do(data.frame(.[1:2], Test_Date = seq(.$Test_Date, .$End_Date, by = "1 day"))) %>%
  tibble()

CE_yest <- CE_days_fill %>%
  #The line below is to create bogus data so that the summarization works
  add_row(Last_Name = NA, First_Name = NA, Test_Date = yest) %>%
  filter(Test_Date == yest) %>%
  group_by(Test_Date, .drop = FALSE) %>%
  summarise(Cases = n()) %>%
  ungroup() %>%
  mutate(Cases = Cases-1) %>% #Correct for the bogus data
  mutate(Type = str_glue("Active on ", format(yest, "%a %b %d"))) %>%
  select(Type, Cases)

CE_f21_total <- CE %>%
  summarise(Cases = n()) %>%
  mutate(Type = "Fall Semester Total") %>%
  select(Type, Cases)

CE_all_total <- E_all %>%
  filter(Test_Result == "p") %>%
  #Fill in missing Test Dates 
  mutate(Test_Date = case_when(
    !is.na(Test_Date) ~ Test_Date,
    !is.na(Result_Date) ~ Result_Date,
    !is.na(Isolated) ~ Isolated,
    !is.na(Quarantine) ~ Quarantine,
    TRUE ~ yest - 1  #No data defaults to 2 days ago
  )) %>%
  select(Last_Name:First_Name, Test_Date, End_Date)%>%
  summarise(Cases = n()) %>%
  mutate(Type = "Total of Last Two Years") %>%
  select(Type, Cases)

CE_Tab <- bind_rows(CE_yest, CE_f21_total, CE_all_total)
save(CE_Tab, file = "data/CE_Tab.Rdata")

#Maximum count needed for graphs
mCE <- CE_days_fill %>%
  group_by(Test_Date, .drop = FALSE) %>%
  summarise(n = n()) %>%
  slice(which.max(n)) %>%
  pull()
save(mCE, file = "mCE.Rdata")

