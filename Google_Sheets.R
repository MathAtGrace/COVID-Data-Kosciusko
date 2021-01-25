#Try to only do this once or twice a day
library(googlesheets4)

#Student Data
S_fall <- read_sheet("https://docs.google.com/spreadsheets/d/1Jwp6_vxG2l3JQU-hiZ6XBmI_74H6DcDVnF4g5DBn9ic/edit#gid=0",
                skip = 1)
#,
#                col_types = paste0("ncc",
#                                   "cc",
#                                   "ccc",
#                                   "ccDD",
#                                   "DDD",
#                                   "DDc",
#                                   "DDc",
#                                   "c",
#                                   "ccccc",
#                                   "ccc"),
#                col_names = c("ID", "Last_Name", "First_Name",
#                              "Last_Care_Call_Date", "Email_Sent",
#                              "Close_Contact", "Gender", "Phone",
#                              "Grace_Space", "Location", "TQS", "Exposure_Date",
#                              "Quarantine", "Isolated", "Symptons_Started",
#                              "Test_Date", "Result_Date", "Test_Result",
#                              "End_Date", "Reslease_Email_Sent", "Results_Recd",
#                              "Connie_Email",
#                              "RD_RA", "Athlete", "Meals", "Complete", "Notes",
#                              "Extra", "Care_Call_Notes", "Ryan"))
#Save the Student Data

#Backup Plan - The Copycat spreadsheet
#S_fall <- read_sheet("https://docs.google.com/spreadsheets/d/1hYSSaiyIbV_pjBs3ljTwF9kf5YI0Ewj_bNEt7tNYMtA/edit#gid=0",
#                     skip = 1)

#Save the fall data
save(S_fall, file = "SF.RData")

S_spring <- read_sheet("https://docs.google.com/spreadsheets/d/107-RZMp8JifihWCQymwHVtJ7teUtVQL1cSC4O1H0fLo/edit?ts=600b0c97#gid=0",
                     skip = 1)

save(S_spring, file = "SS.Rdata")
#                     col_types = paste0("ncc", #ABC
#                                        "DD", #DE
#                                        "ccc", #FGH
#                                        "cccc", #IJKL
#                                        "DDD", #MNO
#                                        "cDD", #PQR
#                                        "cDD", #STU
#                                        "ccc", #VWX
#                                        "cccc", #Y Z AA AB
#                                        "c"), #AC
#                     col_names = c("ID", "Last_Name", "First_Name", #ABC
#                                   "Last_QI_Form_Sent", "Care_Call", #DE
#                                   "Email_Sent", "Q_Exp_Name", "Gender", #FGH
#                                   "Phone", "Grace_Space", "Needs_Meal", "Location", #IJKL
#                                   "TQS", "Exposure", "QI_Started", #MNO
#                                   "Asymptomatic", "Symptom_Start", "Test_Date", #PQR
#                                   "Test_Result", "Possible_End", "Actual_End", #STU
#                                   "Results_Received", "Release_Email_Sent", "Connie_Emailed", #VWX
#                                   "RD", "Athlete", "Complete", "Notes", #Y Z AA AB
#                                   "Health_Center")) #AC


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
