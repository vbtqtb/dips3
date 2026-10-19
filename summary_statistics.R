library(RMariaDB)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(readr)
library(forcats)
library(ggplot2)
library(psych)
library(scales)
library(purrr)
library(jsonlite)
library(lme4)
library(nlme)

# REMEMBER NOT TO PUSH LOGIN CREDENTIALS TO GIT!
source("scripts/connect_MariaDB.R")
#source("scripts/connect_MariaDB2.R") # for keyboard / typing information

# load functions 
source("preprocessing/helper_functions.R") # loads functions
source("preprocessing/dd_episodes.R") # code DD episode information
source("preprocessing/get_calls.R") # Yannick's code to clean calls data from MS & ESM
source("preprocessing/build_daily_diary.R") # to build the DD dataset (initial merging, reshaping, cleaning)
source("preprocessing/fix_timestamp_DIPS3.R") # to correct timestamps for timezone offset in ESM data
# load Ramona's functions for app usage extraction
source("preprocessing/timestamp_correction.R") # for timestamp correction of the sensing data
source("preprocessing/helper_variables.R") 
source("preprocessing/App_features.R")
source("preprocessing/App_preprocessing.R") 
source("preprocessing/aggregation_labels.R") # assign usage sessions to ESM episodes
source("preprocessing/Screen_preprocessing_dips3.R") # labelling of screen events - also needed for app usage - now independent on Android OS version
source("Vanessa/get_texts_d3.R")

# CHECK if hourly and daily episodes match -------------------------------------

## APP USAGE

load("Vanessa/dips3 hourly/df_ep_hourly_pre5.rda")
load("Vanessa/dips3/df_dips3_pre6.rda")

users <- df_ep_hourly %>% pull(username) %>% unique()

ids <- sample(1:386, 10, replace = F)

ids <- c(1,2,3,5,6,7)

summarystat <- tibble(username = character(), ep_int = interval(), comm_app_freq= numeric(), socmed_app_freq= numeric(), comb_app_freq= numeric(), audio_app_freq= numeric(), career_app_freq= numeric(),
                      create_app_freq= numeric(), dating_app_freq= numeric(), finance_app_freq= numeric(), food_app_freq= numeric(), game_app_freq= numeric(), health_app_freq= numeric(), internet_app_freq= numeric(), know_app_freq= numeric(),
                      news_app_freq= numeric(), orient_app_freq= numeric(), photo_app_freq= numeric(), read_app_freq= numeric(), security_app_freq= numeric(), settings_app_freq= numeric(), shop_app_freq= numeric(), spirit_app_freq= numeric(),
                      system_app_freq= numeric(), time_app_freq= numeric(), tools_app_freq= numeric(), transport_app_freq= numeric(), visual_app_freq= numeric(), weather_app_freq= numeric(), comm_app_time = numeric(), socmed_app_time = numeric(),
                      comb_app_time = numeric(), audio_app_time = numeric(), career_app_time = numeric(), create_app_time = numeric(), dating_app_time = numeric(), finance_app_time = numeric(), food_app_time = numeric(), game_app_time = numeric(),
                      health_app_time = numeric(), internet_app_time = numeric(), know_app_time = numeric(), news_app_time = numeric(), orient_app_time = numeric(), photo_app_time = numeric(), read_app_time = numeric(), security_app_time = numeric(),
                      settings_app_time = numeric(), shop_app_time = numeric(), spirit_app_time = numeric(), system_app_time = numeric(), time_app_time = numeric(), tools_app_time = numeric(), transport_app_time = numeric(), visual_app_time = numeric(),
                      weather_app_time = numeric())

stat <-  tibble(username = character(), ep_int = interval(), comm_app_freq= numeric(), socmed_app_freq= numeric(), comb_app_freq= numeric(), audio_app_freq= numeric(), career_app_freq= numeric(),
                create_app_freq= numeric(), dating_app_freq= numeric(), finance_app_freq= numeric(), food_app_freq= numeric(), game_app_freq= numeric(), health_app_freq= numeric(), internet_app_freq= numeric(), know_app_freq= numeric(),
                news_app_freq= numeric(), orient_app_freq= numeric(), photo_app_freq= numeric(), read_app_freq= numeric(), security_app_freq= numeric(), settings_app_freq= numeric(), shop_app_freq= numeric(), spirit_app_freq= numeric(),
                system_app_freq= numeric(), time_app_freq= numeric(), tools_app_freq= numeric(), transport_app_freq= numeric(), visual_app_freq= numeric(), weather_app_freq= numeric(), comm_app_time = numeric(), socmed_app_time = numeric(),
                comb_app_time = numeric(), audio_app_time = numeric(), career_app_time = numeric(), create_app_time = numeric(), dating_app_time = numeric(), finance_app_time = numeric(), food_app_time = numeric(), game_app_time = numeric(),
                health_app_time = numeric(), internet_app_time = numeric(), know_app_time = numeric(), news_app_time = numeric(), orient_app_time = numeric(), photo_app_time = numeric(), read_app_time = numeric(), security_app_time = numeric(),
                settings_app_time = numeric(), shop_app_time = numeric(), spirit_app_time = numeric(), system_app_time = numeric(), time_app_time = numeric(), tools_app_time = numeric(), transport_app_time = numeric(), visual_app_time = numeric(),
                weather_app_time = numeric())

for (i in length(ids)){
  
  id = ids[6]
  user = users[id]
  
  hourly <- df_ep_hourly %>%
    filter(username == user) %>%
    select(username, ep_int, ends_with(c("_freq","_time")))
          
  daily <- df_dips3 %>%
    filter(username == user) %>%
    select(username, ep_int, ends_with(c("_freq","_time")))
  
  episodes <- daily %>% pull(ep_int)
  
  for (j in length(episodes)){
    
  episode = episodes[14]
  stat <- hourly %>%
    filter(ep_int %within% episode) 
  
  stat <- stat %>%
    group_by(username) %>%
    summarise(across(where(is.numeric), sum, na.rm = T)) %>%
    mutate(ep_int = episode,
           username = paste(username, "hr"))
  
  summary <- daily %>%
    filter(ep_int == episode) %>%
    rbind(stat)
  
  summarystat <- summarystat %>%
    rbind(summary)
  
  }
  

}


compare_hr_d <- summarystat

save(compare_hr_d, file = "Vanessa/dips3 hourly/compare_hr_d.rda")

## AWARE CALLS TEXTS

summarystat <- tibble(username = character(), ep_int = interval(), n_aware = numeric(),
                      n_voice = numeric(), prop_voice = numeric(), n_ms_calls = numeric(),
                      dur_ms_calls = numeric(), ms_outgoing = numeric(),
                      ms_len_outgoing = numeric(), unique_numbers = numeric())

stat <-  tibble(username = character(), ep_int = interval(), n_aware = numeric(),
                n_voice = numeric(), prop_voice = numeric(), n_ms_calls = numeric(),
                dur_ms_calls = numeric(), ms_outgoing = numeric(),
                ms_len_outgoing = numeric(), unique_numbers = numeric())


for (i in length(ids)){
  
  id = ids[2]
  user = users[id]
  
  hourly <- df_ep_hourly %>%
    filter(username == user) %>%
    select(username, ep_int, n_aware, n_voice, prop_voice, n_ms_calls,
           dur_ms_calls, ms_outgoing, ms_len_outgoing, unique_numbers)
  
  daily <- df_dips3 %>%
    filter(username == user) %>%
    select(username, ep_int, n_aware, n_voice, prop_voice, n_ms_calls,
           dur_ms_calls, ms_outgoing, ms_len_outgoing, unique_numbers)
  
  episodes <- daily %>% pull(ep_int)
  
  for (j in length(episodes)){
    
    episode = episodes[2]
    stat <- hourly %>%
      filter(ep_int %within% episode) 
    
    stat <- stat %>%
      group_by(username) %>%
      summarise(across(where(is.numeric), sum, na.rm = T)) %>%
      mutate(ep_int = episode,
             username = paste(username, "hr"))
    
    summary <- daily %>%
      filter(ep_int == episode) %>%
      rbind(stat)
    
    summarystat <- summarystat %>%
      rbind(summary)
    
  }
  
  
}


# SUMMARY Statistics -----------------------------------------------------------

summary(df_ep_hourly$ep_dur)

ggplot(df_ep_hourly, aes(x = ep_dur)) + geom_histogram() # a few outliers, possibly due to time zone diffs!


ggplot(df_ep_hourly, aes(x=n_aware)) + geom_histogram(bins=100)
ggplot(df_ep_hourly %>% filter(prop_voice!=0), aes(x=prop_voice)) + geom_histogram(bins=100)


# check for unrealistic cases:
df_ep_hourly %>% filter(comm_app_time > 50 | socmed_app_time > 50) %>% select(user_id, e_s_questionnaire_id, contains("app")) %>% print(n=Inf)



summary(df_ep_hourly)

summary(df_dips3)

# check cases with unrealistic or missing data

View(filter(df_ep_hourly, is.na(comm_app_time)))

head(filter(df_ep_hourly, ms_len_outgoing == 49830)) %>%
  print(width = Inf)

head(filter(df_ep_hourly, settings_app_time == 60)) %>%
  print(width = Inf)

head(filter(df_ep_hourly, settings_app_freq == 379)) %>%
  print(width = Inf)

head(filter(df_ep_hourly, weather_app_time >= 42)) %>%
  print(width = Inf)

























