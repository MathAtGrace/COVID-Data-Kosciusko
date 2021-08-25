library(googlesheets4)

S_spring21 <- read_sheet("https://docs.google.com/spreadsheets/d/107-RZMp8JifihWCQymwHVtJ7teUtVQL1cSC4O1H0fLo/edit?ts=600b0c97#gid=0",
                     sheet = "Quarantine/Isolation", skip = 1)
save(S_spring21, file = "data-raw/SS.Rdata")


S_fall20 <- read_sheet("https://docs.google.com/spreadsheets/d/1Jwp6_vxG2l3JQU-hiZ6XBmI_74H6DcDVnF4g5DBn9ic/edit#gid=0",
                       sheet = "Quarantine/Isolation", skip = 1)
save(S_fall20, file = "data-raw/SF20.Rdata")
