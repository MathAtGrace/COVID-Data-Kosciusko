library(httr)
library(readxl)
library(tidyverse)
library(tigris)

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

#Cases_by_Zip
url = "https://hub.mph.in.gov/dataset/14a59397-9ebc-4902-a7c7-fd7ca3c08101/resource/3ea01356-42e4-42aa-8935-493709313ca3/download/covid_count_per_zip_all.csv"
cases <- read.csv(url)

#Vaccinations by Zip
url = "https://hub.mph.in.gov/dataset/d0cb45af-c9de-40f5-9394-de367c17a7a1/resource/c496b384-f543-417e-912f-995caebf5fc0/download/vaccinations-by-zip-with-population.csv"
vacs <- read.csv(url)%>%
  mutate(zip_cd = as.character(zip_cd),
         first = case_when(
           first_dose_administered == "Suppressed" ~ 0,
           TRUE ~ parse_double(first_dose_administered)
         ),
         single = case_when(
           single_dose_administered == "Suppressed" ~ 0,
           TRUE ~ parse_double(single_dose_administered)
         ),
         partial = case_when(
           (first + single) > 0 ~ (first + single)/population_16_and_over,
           TRUE ~ as.numeric(NA)
         )
  )

In_zip <- zctas(cb = TRUE, state = "Indiana", class = "sf")

In_zip <- In_zip %>%
  inner_join(vacs, 
            by = c("ZCTA5CE10" = "zip_cd"))

ggplot(In_zip) +
  geom_sf(aes(fill = partial)) +
  scale_fill_gradient(
    low = "sky blue",
    high = "#000066",
    na.value = "grey50"
  )

Kos_zip <- zctas(cb = TRUE, starts_with = "465", class = "sf")

Kos_zip <- Kos_zip %>%
  inner_join(vacs, 
            by = c("ZCTA5CE10" = "zip_cd"))

ggplot(Kos_zip) +
  geom_sf(aes(fill = partial)) +
  scale_fill_gradient(
    low = "sky blue",
    high = "#000066",
    na.value = "grey50"
  )

#Hosp Census Data
url = "https://hub.mph.in.gov/dataset/d57037d4-6dab-4f09-9905-9349e9180ce9/resource/efa7b90c-b204-4618-9436-e33aac140e71/download/covid_report_puibed_date.xlsx"
GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
IN_hosps <- read_excel(tf)
#could graph?

#Case Data
url = "https://hub.mph.in.gov/dataset/6b57a4f2-b754-4f79-a46b-cff93e37d851/resource/46b310b9-2f29-4a51-90dc-3886d9cf4ac1/download/covid_report.xlsx"
GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
case_data <- read_excel(tf)

#Vax by County
url = "https://hub.mph.in.gov/dataset/145a43b2-28e5-4bf1-ad86-123d07fddb55/resource/82d99020-093f-41ac-95c7-d3c335b8c2ba/download/county-vaccination-demographics.xlsx"
GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
vax_by_county <- read_excel(tf)

#Vax by Date
url = "https://hub.mph.in.gov/dataset/c484a6a3-2f32-4af9-8e02-c98801c3454f/resource/a4d23ae8-34c2-4951-85e0-3ea9345ee6ea/download/county-vaccinations-by-date.csv"
vax_by_date <- read_csv(url)
