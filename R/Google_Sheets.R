#Try to only do this once or twice a day
library(googlesheets4)

S_fall21 <- read_sheet("https://docs.google.com/spreadsheets/d/1cAolnje3crsdF23m_EBU-a1KZjTXZLP776CbruXKDfY/edit#gid=0",
                   sheet = "Quarantine/Isolation", skip = 1)
save(S_fall21, file = "data-raw/SF21.Rdata")

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
save(E, file = "data-raw/E.Rdata")
