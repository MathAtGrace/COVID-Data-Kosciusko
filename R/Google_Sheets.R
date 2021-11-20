#Try to only do this once or twice a day
library(googlesheets4)

gs4_auth("johnsor@grace.edu")

S_fall21 <- read_sheet("https://docs.google.com/spreadsheets/d/1cAolnje3crsdF23m_EBU-a1KZjTXZLP776CbruXKDfY/edit#gid=0",
                   sheet = "Quar / Iso", skip = 1)
save(S_fall21, file = "data-raw/SF21.Rdata")

#Employee Data
E <- read_sheet("https://docs.google.com/spreadsheets/d/1KiUFoTYCCxHC6UPKpLxrNvLGEZmbxMLfRcDAmGq34A8/edit#gid=0")
save(E, file = "data-raw/E.Rdata")
