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
library(haven)

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
source("preprocessing/screen/Screen_preprocessing.R")

#### TESTING #####
load(file = "dips3/data/df_dips3_pre6.rda")

communication <-  DBI::dbReadTable(con, "ps_communication") # needed for preprocessing_screen function
colnames(communication)[which(colnames(communication) == "id")] <- "communication_id"

user_ids <- df_dips3 %>% pull(user_id) %>% unique()

# read sensing data and apply timestamp correction

df_screen_act = dplyr::tbl(con, "ps_activity") %>% 
  dplyr::filter((activityName == "BOOT" | activityName == "SCREEN" | activityName == "PHONE" | event == "SCREEN_INTERACTIVE") & timestamp>='2022-06-21 00:00:00') %>% 
  data.frame()
df_screen_act$user_id = as.character(df_screen_act$user_id) #recode user id variable as character to avoid confusion
df_screen_act = ps_activity_preproc_timestamps(df_screen_act) # correct timestamps

# filter all users with low screen usage

screens_low = tibble()
screens_low_user_list = c()

count = 0

for (i in 1:length(user_ids)){
  user = user_ids[i]
  df_screen <- df_screen_act %>%
    filter(user_id == user) 
  
  if (nrow(df_screen) <= 10){
    screens_low <- rbind(screens_low, df_screen)
    screens_low_user_list <- append(screens_low_user_list, user)
    count = count + 1
  }
}

screens_low <- screens_low %>%
  select(user_id, event, activityName, timestamp.corrected, everything())

screens_low_user = screens_low %>% pull(user_id) %>% unique() %>% as.integer()

scr_low_app_usg <- df_dips3 %>% 
  filter(user_id %in% screens_low_user_list) %>%
  select(user_id, ep_int, ends_with(c("_time","_freq")))

# check ps_activity for SCREEN_INTERACTIVE 

screen_events = dplyr::tbl(con, "ps_activity") %>% 
  dplyr::filter(event == "SCREEN_INTERACTIVE" & timestamp>='2022-06-21 00:00:00') %>% 
  data.frame()
screen_events$user_id = as.character(screen_events$user_id) #recode user id variable as character to avoid confusion
screen_events = ps_activity_preproc_timestamps(screen_events) # correct timestamps

screen_events <- screen_events %>% select(user_id, activityName, event, weekday, timestamp.corrected, everything())

screen_events$event %>% unique()

# check for SCREEN_INTERACTIVE

screens_low = tibble()

count = 0

for (i in 1:length(user_ids)){
  user = user_ids[i]
  df_screen <- screen2 %>%
    filter(user_id == user) 
  
  if (nrow(df_screen) <= 2){
    screens_low <- rbind(screens_low, df_screen)
    count = count + 1
  }
}

screen2$user_id %>% unique() # data of 4 users only

# check for activityName = APPS low screen usage in the afternoon with dips3 hourly dataset

df_apps_act = dplyr::tbl(con, "ps_activity") %>% 
  dplyr::filter((activityName == "APPS") & timestamp>='2022-06-21 00:00:00') %>% 
  data.frame()

df_apps_act$event %>% unique()

df = dplyr::tbl(con, "ps_activity") %>% 
  dplyr::filter((activityName == "APP") & timestamp>='2022-06-21 00:00:00') %>% 
  data.frame() # nothing
