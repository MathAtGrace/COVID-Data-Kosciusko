library(httr)
library(readxl)

#Kosciusko Data
url = "https://hub.mph.in.gov/dataset/bd08cdd3-9ab1-4d70-b933-41f9ef7b809d/resource/afaa225d-ac4e-4e80-9190-f6800c366b58/download/covid_report_county_date.xlsx"
GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
IN_counties <- read_excel(tf)
IN_counties$DATE <- as.Date(IN_counties$DATE)
save(IN_counties, file = "IN.Rdata")

#Indiana Data
url2 = "https://hub.mph.in.gov/dataset/ab9d97ab-84e3-4c19-97f8-af045ee51882/resource/182b6742-edac-442d-8eeb-62f96b17773e/download/covid_report_date.xlsx"
GET(url2, write_disk(tf <- tempfile(fileext = ".xlsx")))
I <- read_excel(tf)
I$DATE <- as.Date(I$DATE)
save(I, file = "I.Rdata")