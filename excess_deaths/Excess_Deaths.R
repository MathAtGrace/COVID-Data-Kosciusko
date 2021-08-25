library(COVID19)
library(tidyverse)
library(lubridate)
library(gridExtra)

cut_off = lubridate::week(Sys.Date()) - 4

US <- COVID19::covid19(country = "US", level = 2)%>%
  mutate(week_of_year=lubridate::week(date),
         year = lubridate::year(date),
         new_deaths = c(0, diff(deaths, 1)),
         new_cases = c(0, diff(confirmed, 1)))%>%
  rename(state = administrative_area_level_2)%>%
  group_by(state, week_of_year, year) %>%
  summarise(covid_deaths = sum(new_deaths, na.rm = TRUE),
            cases = sum(new_cases, na.rm = TRUE),
            workplace_closing = mean(workplace_closing, na.rm = TRUE),
            gatherings_restrictions = mean(gatherings_restrictions, na.rm = TRUE),
            stringency_index = mean(stringency_index, na.rm = TRUE),
            pop = max(population, na.rm = TRUE))

cdc_excess_deaths <- function() {
  dat = readr::read_csv("https://data.cdc.gov/api/views/xkkf-xrst/rows.csv?accessType=DOWNLOAD&bom=true&format=true",
                        guess_max = 10000,
                        col_types=cols()) %>%
    dplyr::rename(date='Week Ending Date') %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::rename(
      state="State",
      type="Type",
      outcome="Outcome",
      suppress="Suppress",
      deaths="Observed Number"
    ) %>%
    mutate(week_of_year=lubridate::week(date))
  colnames(dat) = stringr::str_replace_all(colnames(dat),' ', '_') %>% tolower()
  dat = dplyr::select(dat, -c(starts_with('total'), starts_with('percent'),'year'))
  dat
}

D <- cdc_excess_deaths() %>%
  mutate(year = lubridate::year(date)) %>%
  left_join(US, by = c("state", "year", "week_of_year")) %>%
  mutate(state = factor(state)) %>%
  filter(!(state %in% c("United States", "Puerto Rico",
                        "District of Columbia", "New York City")),
         outcome == "All causes",
         year < 2021 | week_of_year < cut_off) %>%
  group_by(state) %>%
  fill(pop, .direction = "up") %>%
  mutate(deaths_per_mil = deaths/pop * 1000000,
         covid_deaths_per_mil = covid_deaths/pop * 1000000,
         average_expected_per_mil = average_expected_count/pop *1000000)
#View(D)


#p = D %>%
#  ggplot(aes(x = date, y = deaths_per_mil)) +
#  geom_line() +
#  geom_line(aes(y = average_expected_per_mil)) +
#  geom_line(aes(y = covid_deaths_per_mil)) 

p = D %>%
  ggplot(aes(x = date, y = deaths)) +
  geom_line() +
  geom_line(aes(y = average_expected_count)) +
  geom_line(aes(y = covid_deaths)) 

plots = D %>%
  group_by(state) %>%
  do(plots = p %+% . + facet_wrap(~state))

ggsave(filename = "ExcessDeaths.pdf",
       plot = marrangeGrob(plots[[2]], nrow = 1, ncol = 1),
       width = 15, height = 9
)

D %>%
  filter(date > lubridate::as_date("2020-03-15")) %>%
  group_by(state) %>%
  summarise(excess_lower_estimate = sum(excess_lower_estimate, na.rm = TRUE),
            excess_higher_estimate = sum(excess_higher_estimate, na.rm = TRUE),
            covid_deaths = sum(covid_deaths, na.rm = TRUE)) %>%
  write.csv(file = "ExcessDeaths.csv")
