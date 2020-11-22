library (zoo)
library (tidyverse)

denoms = readr::read_csv ("denominators.csv",
                          col_names = c("municipality", "population"),
                          col_types = "cd")


dat_all = readr::read_csv("covid_cases_20201108.csv",
                      col_names = c("date",
                                    "canton_cases",
                                    "plymouth_city_cases",
                                    "plymouth_township_cases"),
                      col_types = "cddd",
                      skip=1,
                      na = ".") %>%
  select (date, canton_cases, plymouth_city_cases, plymouth_township_cases) %>%
  mutate (date = as.Date(date, format = c("%m/%d")),
          combined_cases = canton_cases+ plymouth_city_cases + plymouth_township_cases,
          canton_incidence = (canton_cases/as.numeric(denoms[1,2]) * 1000000),
          plymouth_city_incidence = (plymouth_city_cases/as.numeric(denoms[2,2]) * 1000000),
          plymouth_township_incidence = (plymouth_township_cases/as.numeric(denoms[3,2]) * 1000000),
          combined_incidence = (combined_cases/as.numeric(denoms[4,2]) * 1000000),
          canton_7day = c(rep(NA, 6),rollmean (canton_incidence, k = 7, align = "right")),
          plymouth_city_7day =c(rep(NA, 6), rollmean (plymouth_city_incidence, k = 7, align = "right")),
          plymouth_township_7day = c(rep(NA, 6),rollmean (plymouth_township_incidence, k = 7, align = "right")),
          combined_7day = c(rep(NA, 6),rollmean (combined_incidence, k = 7, align = "right")))




dat_7dayincidences = dat_all %>%
  select (date, canton_7day, plymouth_city_7day, plymouth_township_7day, combined_7day) %>%
  rename ('Canton' = canton_7day,
          'Plymouth City' = plymouth_city_7day,
          'Plymouth Township' = plymouth_township_7day,
          'Combined (Canton and Plymouth)' = combined_7day) %>%
  pivot_longer (!date, names_to = "municipality", values_to = "count") %>% 
  mutate (municipality = factor (municipality, levels = c ("Canton", "Plymouth City", "Plymouth Township", "Combined (Canton and Plymouth)"))) 



dat_7dayincidences$color = as.character (recode (dat_7dayincidences$municipality,
                                   'Canton' = "#E90003",
                                   'Plymouth City' = "#1B9E77",
                                   'Plymouth Township' = "#7570B3",
                                   'Combined (Canton and Plymouth)' = "#AAAAAA"))


dat_7dayincidences$linetype = as.character (recode (dat_7dayincidences$municipality,
                                   'Canton' = 'dash',
                                   'Plymouth City' = 'dot',
                                   'Plymouth Township' = 'dotdash',
                                   'Combined (Canton and Plymouth)' = 'solid'))
