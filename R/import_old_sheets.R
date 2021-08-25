library(googlesheets4)

SS <- read_sheet("https://docs.google.com/spreadsheets/d/107-RZMp8JifihWCQymwHVtJ7teUtVQL1cSC4O1H0fLo/edit?ts=600b0c97#gid=0",
                     sheet = "Quarantine/Isolation", skip = 1)
save(SS, file = "data-raw/SS.Rdata")


SF <- read_sheet("https://docs.google.com/spreadsheets/d/1Jwp6_vxG2l3JQU-hiZ6XBmI_74H6DcDVnF4g5DBn9ic/edit#gid=0",
                       sheet = "Quarantine/Isolation", skip = 1)
save(SF, file = "data-raw/SF.Rdata")