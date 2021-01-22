#Try to only do this once or twice a day
library(googlesheets4)

#Student Data
S <- read_sheet("https://docs.google.com/spreadsheets/d/1Jwp6_vxG2l3JQU-hiZ6XBmI_74H6DcDVnF4g5DBn9ic/edit#gid=0",
                skip = 2,
                col_types = paste0("ncc",
                                   "cc",
                                   "ccc",
                                   "ccDD",
                                   "DDD",
                                   "DDc",
                                   "DDc",
                                   "c",
                                   "ccccc",
                                   "ccc"),
                col_names = c("ID", "Last_Name", "First_Name",
                              "Last_Care_Call_Date", "Email_Sent",
                              "Close_Contact", "Gender", "Phone",
                              "Grace_Space", "Location", "TQS", "Exposure_Date",
                              "Quarantine", "Isolated", "Symptons_Started",
                              "Test_Date", "Result_Date", "Test_Result",
                              "End_Date", "Reslease_Email_Sent", "Results_Recd",
                              "Connie_Email",
                              "RD_RA", "Athlete", "Meals", "Complete", "Notes",
                              "Extra", "Care_Call_Notes", "Ryan"))
#Save the Student Data
save(S, file = "S.RData")

#Employee Data
E <- read_sheet("https://docs.google.com/spreadsheets/d/1M1rKYVYg8bGqYlHjmh51XRNK4JLY5TjMPbrw9_OUaFE/edit#gid=0",
                col_types = paste0("cccc",
                                   "DDDD",
                                   "cDDD",
                                   "cnccc"),
                col_names = c("Last_Name", "First_Name","DoB", "Phone",
                              "Quarantine", "Isolated", "Test_Date", "Result_Date",
                              "Test_Result", "Earliest", "End_Date", "Return_Date",
                              "Work_Home", "Days_Off", "Complete", "Notes", "Notes2"), skip = 1)
#Save Employee Data
save(E, file = "E.Rdata")