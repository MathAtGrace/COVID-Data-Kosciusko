#########################################################################
#Testing Data

#Join with other testing data
#T2 <-SS %>%
#  select(id, test_result, test_date) %>%
#  drop_na() %>%
#  mutate(at_health_center = TRUE) %>% Currently the At_Health_Center data is ambiguous
#  full_join(x = T, y = ., by = c("id", "test_date", "test_result"))%>%
#  mutate(at_health_center = case_when(
#    is.na(at_health_center.x) ~ at_health_center.y,
#    TRUE ~ at_health_center.x
#  )) %>%
#  select(-at_health_center) %>%
#  filter(test_date <= yest) %>%
#  mutate(test_result = (test_result == "p")) %>%
#  select(-at_health_center.x, -at_health_center.y) %>%

#Positives are tested again, so we group by id and date, then use prod()
T2 <- T %>%
  mutate(test_date = as_date(test_time)) %>%
  group_by(id, test_date, employee) %>%
  summarise(test_result = prod(test_result))


#Weekly  
TW <- T2 %>%
  group_by(employee) %>%
  summarise(Cases = sum(test_result), Total = n()) %>%
  mutate(Type = c("Students: Spring Semester", "Employees: Spring Semester"),
         Percent = percent(case_when(
           Total == 0 ~ 0,
           TRUE ~ Cases/Total
         ), accuracy = 0.1)) %>%
  select(Type, Total, Cases, Percent)

#Past Week ALl Weekly
PTW <- T2 %>%
  filter(test_date > yest - 7) %>%
  group_by(employee) %>%
  summarise(Cases = sum(test_result), Total = n()) %>%
  mutate(Type = c("Students: Past Week", "Employees: Past Week"),
         Percent = percent(case_when(
           Total == 0 ~ 0,
           TRUE ~ Cases/Total
         ), accuracy = 0.1)) %>%
  select(Type, Total, Cases, Percent)

T_Tab <- bind_rows(PTW, TW)
save(T_Tab, file = "T_Tab.Rdata")

T_Pos <- T2 %>%
  group_by(test_date, employee) %>%
  summarise(Cases = sum(test_result), Total = n())
save(T_Pos, file = "T_Pos.Rdata")

T_Pos_W <- T_Pos%>%
  group_by(employee) %>%
  complete(test_date = seq.Date(as.Date("2021-01-15"), yest, by = "day"),
           fill = list(Cases = 0, Total = 0))%>%
  mutate(Cases = roll_sumr(Cases, 7, fill = NA),
         Total = roll_sumr(Total, 7, fill = NA)) %>%
  drop_na() %>%
  mutate(Percent = case_when(
    Total == 0 ~ 0,
    TRUE ~ Cases/Total
  ))
save(T_Pos_W, file = "T_Pos_W.Rdata")