# library (zoo)
library (dplyr)
library (googlesheets4)
library (googledrive)
library (tidyr)

# options(gargle_oauth_cache = ".secrets")

drive_auth(cache = ".secrets", email = "shincd@umich.edu")
gs4_auth(token = drive_token())



recalculate_incidences <- function (dat) {
  
  dat = dat %>%
    mutate (date = as.Date(date, format = c("%Y-%m-%d")),
            combined_cases = canton_cases+ plymouthcity_cases + plymouthtownship_cases,
            canton_incidence = (canton_cases/as.numeric(denoms[1,2]) * 1000000),
            plymouthcity_incidence = (plymouthcity_cases/as.numeric(denoms[2,2]) * 1000000),
            plymouthtownship_incidence = (plymouthtownship_cases/as.numeric(denoms[3,2]) * 1000000),
            combined_incidence = (combined_cases/as.numeric(denoms[4,2]) * 1000000),
            canton_7day = c(rep(NA, 6),zoo::rollmean (canton_incidence, k = 7, align = "right")),
            plymouthcity_7day =c(rep(NA, 6), zoo::rollmean (plymouthcity_incidence, k = 7, align = "right")),
            plymouthtownship_7day = c(rep(NA, 6),zoo::rollmean (plymouthtownship_incidence, k = 7, align = "right")),
            combined_7day = c(rep(NA, 6),zoo::rollmean (combined_incidence, k = 7, align = "right")),
            canton_15day = c(rep(NA, 14),zoo::rollmean (canton_incidence, k = 15, align = "right")),
            plymouthcity_15day =c(rep(NA, 14), zoo::rollmean (plymouthcity_incidence, k = 15, align = "right")),
            plymouthtownship_15day = c(rep(NA, 14),zoo::rollmean (plymouthtownship_incidence, k = 15, align = "right")),
            combined_15day = c(rep(NA, 14),zoo::rollmean (combined_incidence, k = 15, align = "right")),
            canton_28day = c(rep(NA, 27),zoo::rollmean (canton_incidence, k = 28, align = "right")),
            plymouthcity_28day =c(rep(NA, 27), zoo::rollmean (plymouthcity_incidence, k = 28, align = "right")),
            plymouthtownship_28day = c(rep(NA, 27),zoo::rollmean (plymouthtownship_incidence, k = 28, align = "right")),
            combined_28day = c(rep(NA, 27),zoo::rollmean (combined_incidence, k = 28, align = "right"))
    ) %>%
    
    
    pivot_longer(cols = -date, 
                 names_to = c("municipality","measure"), 
                 names_sep = "_", 
                 values_to = "value") %>% 
    mutate (municipality = case_when (
      municipality ==  "canton" ~ "Canton",
      municipality =="plymouthcity" ~ "Plymouth City",
      municipality =="plymouthtownship" ~ "Plymouth Township",
      municipality =="combined" ~ "Combined (Canton and Plymouth)")) %>%
    mutate (municipality = factor (municipality, levels = c ("Canton", "Plymouth City", "Plymouth Township", "Combined (Canton and Plymouth)")),
            value = round (value, 1)) 
  
  
  
  dat$color = as.character (recode (dat$municipality,
                                    'Canton' = "#E90003",
                                    'Plymouth City' = "#1B9E77",
                                    'Plymouth Township' = "#7570B3",
                                    'Combined (Canton and Plymouth)' = "#AAAAAA"))
  
  
  dat$linetype = as.character (recode (dat$municipality,
                                       'Canton' = 'dash',
                                       'Plymouth City' = 'dot',
                                       'Plymouth Township' = 'dotdash',
                                       'Combined (Canton and Plymouth)' = 'solid'))
  
  dat = dat %>%
    select (date, municipality, measure, value) %>%
    filter (measure == "incidence") %>%
    group_by (municipality) %>%
    arrange (date) %>%
    mutate (percent_increase_in_incidence = ifelse (value != 0 & dplyr::lag (value) != 0, round (((value - dplyr::lag (value))/dplyr::lag (value)) * 100, 2), 0),
            is_3day_surge = ifelse (((percent_increase_in_incidence) >= 10) & 
                                      (dplyr::lag (percent_increase_in_incidence, 1) >= 10) & 
                                      (dplyr::lag (percent_increase_in_incidence, 2) >= 10) |
                                      ((percent_increase_in_incidence) >= 10) & 
                                      (dplyr::lag (percent_increase_in_incidence, 1) >= 10) & 
                                      (dplyr::lead (percent_increase_in_incidence, 1) >= 10) |
                                      ((percent_increase_in_incidence) >= 10) & 
                                      (dplyr::lead (percent_increase_in_incidence, 1) >= 10) & 
                                      (dplyr::lead (percent_increase_in_incidence, 2) >= 10), 1, 0),
            
            
            consecutive_percent_increase_in_incidence = paste0 (date, ": ", percent_increase_in_incidence, "%")
            
            
    )%>%
    select (date, municipality, percent_increase_in_incidence, is_3day_surge, consecutive_percent_increase_in_incidence) %>%
    dplyr::left_join (dat, by = c("date", "municipality"))
  
  dat = dat %>%
    select (date, municipality, measure, value) %>%
    filter (measure == "7day") %>%
    group_by (municipality) %>%
    arrange (date) %>%
    mutate (increase_in_7day = ifelse (lag(value, 1) < value, 1, 0),
            is_5day_surge = ifelse ((increase_in_7day == 1 &
                                       dplyr::lag (increase_in_7day) == 1 &
                                       dplyr::lag (increase_in_7day, 2)  == 1 &
                                       dplyr::lag (increase_in_7day, 3)  == 1 &
                                       dplyr::lag (increase_in_7day, 4)  == 1) |
                                      (increase_in_7day == 1 &
                                         dplyr::lag (increase_in_7day) == 1 &
                                         dplyr::lag (increase_in_7day, 2)  == 1 &
                                         dplyr::lag (increase_in_7day, 3)  == 1 &
                                         dplyr::lead (increase_in_7day, 1)  == 1) |
                                      (increase_in_7day == 1 &
                                         dplyr::lag (increase_in_7day) == 1 &
                                         dplyr::lag (increase_in_7day, 2)  == 1 &
                                         dplyr::lead (increase_in_7day, 1)  == 1 &
                                         dplyr::lead (increase_in_7day, 2)  == 1) |
                                      (increase_in_7day == 1 &
                                         dplyr::lag (increase_in_7day) == 1 &
                                         dplyr::lead (increase_in_7day, 1)  == 1 &
                                         dplyr::lead (increase_in_7day, 2)  == 1 &
                                         dplyr::lead (increase_in_7day, 3)  == 1) |
                                      (increase_in_7day == 1 &
                                         dplyr::lead (increase_in_7day) == 1 &
                                         dplyr::lead (increase_in_7day, 2)  == 1 &
                                         dplyr::lead (increase_in_7day, 3)  == 1 &
                                         dplyr::lead (increase_in_7day, 4)  == 1) , 1, 0),
            
            
            consecutive_increase_in_7day = paste0 (date, ": ", value)
            ) %>%
    select (date, municipality, increase_in_7day, is_5day_surge, consecutive_increase_in_7day) %>%
    dplyr::left_join (dat, by = c("date", "municipality"))
  
  
  
  return (as.data.frame(dat))
}

denoms = readr::read_csv ("denominators.csv",
                          col_names = c("municipality", "population"),
                          col_types = "cd")


dat_cases_only <- read_sheet ("https://docs.google.com/spreadsheets/d/1_BWCAtqFdap8giAtqvZLqVcmPj_MkXUt3Dnge4NGgyk/edit#gid=0",
                              range = "Sheet1!A:D")

dat_all = recalculate_incidences (dat_cases_only)

dat_selected <- dat_all %>%
  filter (date <= max(dat_all$date) & date >= max(dat_all$date) - 28,
          measure == "7day")

dat_3day_surge <- dat_all %>%
  filter (date <= max(dat_all$date) & date >= max(dat_all$date) - 28,
          is_3day_surge == 1,
          measure == "7day") 

dat_5day_surge <- dat_all %>%
  filter (date <= max(dat_all$date) & date >= max(dat_all$date) - 28,
          is_5day_surge == 1,
          measure == "7day") 


