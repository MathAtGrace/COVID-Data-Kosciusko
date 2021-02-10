#student cases
#employee case
#student testing
#isolated students
#quarantined students
library(xlsx)
library(tidyverse)
#write.xlsx(dataframe1, file="filename.xlsx", sheetName="sheet1", row.names=FALSE)
#write.xlsx(dataframe2, file="filename.xlsx", sheetName="sheet2", append=TRUE, row.names=FALSE)

load("T_Tab.Rdata")
load("CS.Rdata")
load("QS_Tab.Rdata")
load("IS_Tab.Rdata")
load("CS_Tab.Rdata")
load("ST_hour.Rdata")
load("T_Pos.Rdata")
load("CE_Tab.Rdata")
load("CE.Rdata")
load("T_Pos_W.Rdata")
load("this_day.Rdata")

yest = this_day - 1
t_sheet = str_glue("G:/Shared drives/Dashboard - COVID-19/Tables/tables_{this_day}.xlsx")

CS_Tab %>%
  data.frame() %>%
  column_to_rownames('Type') %>%
  write.xlsx2(file=t_sheet, sheetName="Student_Cases")
CE_Tab %>%
  data.frame() %>%
  column_to_rownames('Type') %>%
  write.xlsx2(file=t_sheet, sheetName="Employee_Cases", append=TRUE)
T_Tab %>%
  data.frame() %>%
  column_to_rownames('Type') %>%
  write.xlsx2(file=t_sheet, sheetName="Testing", append=TRUE)
IS_Tab %>%
  data.frame() %>%
  column_to_rownames('Type') %>%
  write.xlsx2(file=t_sheet, sheetName="Studet_Isolations", append=TRUE)
QS_Tab %>%
  data.frame() %>%
  column_to_rownames('Type') %>%
  write.xlsx2(file=t_sheet, sheetName="Studet_Quarantines", append=TRUE)

T_Pos %>%
  group_by(employee) %>%
  complete(test_date = seq.Date(as.Date("2021-01-15"), yest, by = "day"),
           fill = list(Cases = 0, Total = 0))%>%
  mutate(employee = case_when(
    employee ~ "Employee",
    TRUE ~ "Student"
  )) %>%
  rename(Positive = Cases, Tests = Total) %>%
  pivot_wider(names_from = employee,
              values_from = c(Positive, Tests),
              values_fill = 0) %>%
  select(test_date, Tests_Student, Positive_Student,
         Tests_Employee, Positive_Employee) %>%
  mutate(Total_Tests = Tests_Student + Tests_Employee,
         Total_Positive = Positive_Student + Positive_Employee) %>%
  column_to_rownames('test_date') %>%
  write.xlsx2(file=t_sheet, sheetName="Testing_by_day", append=TRUE)

T_Pos_W %>%
  filter(!employee) %>%
  ungroup() %>%
  select(-employee) %>%
  mutate(Percent = percent(Percent, accuracy = 0.01)) %>%
  column_to_rownames('test_date') %>%
  rename(Tests_Week = Total, Positives_Week = Cases) %>%
  write.xlsx2(file=t_sheet, sheetName="Weekly_Student_Percent", append=TRUE)
  
wb <- loadWorkbook(t_sheet)
sheets <- getSheets(wb)
# autosize column widths
autoSizeColumn(sheets[[1]], colIndex=1:5)
autoSizeColumn(sheets[[2]], colIndex=1:2)
autoSizeColumn(sheets[[3]], colIndex=1:4)
autoSizeColumn(sheets[[4]], colIndex=1:5)
autoSizeColumn(sheets[[5]], colIndex=1:5)
autoSizeColumn(sheets[[6]], colIndex=1:7)
autoSizeColumn(sheets[[7]], colIndex=1:4)
saveWorkbook(wb,t_sheet)
  
