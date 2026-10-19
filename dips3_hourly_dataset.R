#### LOAD REQUIRED RESSOURCES ####
# load packages 
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
source("Vanessa/get_texts_d3.R")

#### HOURLY DATA ---------------------------------------------------------------

load(file = "dips3/data/df_dips3_pre3.rda")

df_ep_hourly <- df_dips3 %>%
  select(username) %>%
  distinct() 

# merge user_id to data
df_ep_hourly <- dplyr::left_join(df_ep_hourly, (DBI::dbReadTable(con, "ps_user") %>% dplyr::select(user_id = id, username)))

# create hourly episodes by study start and study end time
studymanagement =  DBI::dbReadTable(con3, "ps_participant")
### filter for study period (ids before 21.06.2022 are test users)
studymanagement = studymanagement %>% dplyr::filter(study_start_time > "2022-06-21 00:00:00 UTC")

study_assignment = studymanagement %>% select(user_id, study_start_time, study_ended)

df_ep_hourly <- df_ep_hourly %>%
  left_join(study_assignment, by = "user_id") %>%
  distinct() #not needed?

df_ep_hourly <- df_ep_hourly %>%
  mutate(study_start_time = floor_date(df_ep_hourly$study_start_time, unit = "hour"))

df_ep_hourly <- df_ep_hourly %>%
  mutate(ep_start = map2(study_start_time, study_ended, seq, by = "1 hour")) %>%
  unnest(ep_start)

df_ep_hourly <- df_ep_hourly %>%
  mutate(ep_end = ep_start + lubridate::hours(1),
         ep_dur = lubridate::as.duration(ep_end - ep_start),
         ep_int = lubridate::interval(ep_start, ep_end)) %>%
  select(-study_start_time,-study_ended)

# match n_diary and n_days from df_dips3 to data

## n_diary
## NA outside of esm period
df_ep_hourly <- df_ep_hourly %>%
  mutate(ep_start_hr = floor_date(ep_start, unit = "days"))

df_ep_hr <- df_dips3 %>%
  select(username, n_diary, ep_start) %>%
  mutate(ep_start = floor_date(ep_start, unit = "days")) %>%
  rename("ep_start_hr" = "ep_start")

df_ep_hourly <- df_ep_hourly %>%
  left_join(df_ep_hr, by = c('username', 'ep_start_hr')) %>%
  select(-ep_start_hr)

## n_days

#use ep_start from dips3 to get start_date of daily diary
start_dates <- df_dips3 %>%
  filter(n_days == 1) %>%
  select(username, user_id, start_date = ep_start) %>%
  mutate(start_date = floor_date(start_date, "day"))

df_ep_hourly <- df_ep_hourly %>%
  left_join(start_dates) %>%
  mutate(n_days = as.numeric(difftime(floor_date(ep_start, unit = "day"), 
                                      start_date, units = "days")) + 1) %>%
  select(-start_date)

# match e_s_questionnaire_id from df_dips3

df_es_quest_bind <- df_dips3[,c("username", "e_s_questionnaire_id", "n_diary", "n_days")] %>%
  unique()

df_ep_hourly <- df_ep_hourly %>%
  left_join(df_es_quest_bind, by = c('username', 'n_diary', 'n_days'))

save(df_ep_hourly, file = "Vanessa/dips3 hourly/df_ep_hourly_raw.rda")


#### AWARE ---------------------------------------------------------------------

load(file = "Vanessa/dips3 hourly/df_ep_hourly_raw.rda")
load(file = "Vanessa/dips3 hourly/df_aware_raw.rda")

options(dplyr.summarise.inform = FALSE)

users <- df_ep_hourly %>% pull(username) %>% unique()
aware_summaries <- tibble(username=character(), n_aware=integer(), n_voice=double(), ep_int = interval())

df_aware <- df_aware %>%
  cbind(ep_int = NA)

pos = 0

for (i in 1:length(users)){
  user = users[i]
  print(paste0("Run ", i, ", User ", user))
  
  # define intervals
  intv = df_ep_hourly %>% filter(username==user) %>% select(ep_int) %>% pull() %>% unique()
  
  # descriptives per interval 
    for (l in 1:length(intv)){
      
      int = intv[l]
      
      aware_summary <- df_aware %>%
        filter(username==user & timestamp %within% int) %>%
        group_by(username, ep_int = int ) %>% 
        summarise(n_aware=n(), n_voice=sum(voice, na.rm = T)) %>% # sum of overall samplings and of 'voice'
        ungroup()  %>%
        relocate(username, n_aware, n_voice, ep_int)
      
      if (nrow(aware_summary) != 0){
        pos = pos + 1
        aware_summaries[pos, ] <- aware_summary[1, ]
      }
    }
}      

aware_summaries <- aware_summaries %>% filter(!is.na(username))

df_ep_hourly <- df_ep_hourly %>% 
  left_join(aware_summaries, by = c('username', 'ep_int'))

df_ep_hourly <- df_ep_hourly %>% 
  mutate(n_aware = replace_na(n_aware, 0),
         n_voice = replace_na(n_voice, 0)) 

df_ep_hourly <- df_ep_hourly %>% mutate(prop_voice = ifelse(n_aware==0 & n_voice==0, 0, n_voice/n_aware))

save(df_ep_hourly, file = "Vanessa/dips3 hourly/df_ep_hourly_pre1.rda")

# APP USAGE -------------------------------------------------------------------

load(file = "Vanessa/dips3 hourly/df_ep_hourly_pre1.rda")

user_ids <- df_ep_hourly %>% pull(user_id) %>% unique()

# app categorization table from Schoedel et al., 2022 --> edit to new version once available
cat_app <- read_csv("preprocessing/app_categorisation_2020_v2.csv")
cat_app2 <- read_delim("preprocessing/app_categorisation_extended.DIPS2.csv", delim = ";") # updated categorization effort (2022)
cat_app2 <- cat_app2 %>% filter(!is.na(App_name))
cat_app <- bind_rows(cat_app, cat_app2)

# (the rest of the section is mainly based on Ramona's Code: https://gitlab.lrz.de/paperprojects/microsocialrhythms)
options(digits.secs = 3) # making sure that milliseconds are displayed and used for sorting via timestamps
# -> this does change values of the extracted features!

### first, feature extraction for a single user 
user = user_ids[1]

# Step 1: read sensing data for a specified user and apply timestamp correction 
df_app_act = dplyr::tbl(con, "ps_activity") %>% dplyr::filter(user_id %in% user) %>%
  dplyr::filter(!activityName %in% c("PHONESTUDY", "DNAPSOUND", "DNAPACCELEROMETER", "DNAPLIGHT", 
                                     "DNAPPROXIMITY", "DNAPGYROSCOPE", "AWARECONVERSATIONS", "CONTACTS")) %>% 
  dplyr::mutate( created_at = as.character(created_at), 
                 updated_at = as.character(updated_at)) %>% data.frame()
df_app_act$user_id = as.character(df_app_act$user_id) #recode user id variable as character to avoid confusion
df_app_act = ps_activity_preproc_timestamps(df_app_act) # correct timestamps 

# Step 2: extract device information -> not needed anymore, because the new process is independent of SDK 
# device.info =  DBI::dbReadTable(con, "ps_deviceinfo")
# device = unique(df_app_act$deviceinfo_id)[!is.na(unique(df_app_act$deviceinfo_id))]
# device.i = device.info[which(device.info$id == device[1]),]

communication =  DBI::dbReadTable(con, "ps_communication") # also needed for screen event preprocessing
colnames(communication)[which(colnames(communication) == "id")] = "communication_id"

# Step 3: prepare in ESM data the time windows in which to aggregate app usage
# (here, timestamps are already corrected!)
dd_time <- df_ep_hourly %>% filter(user_id==user) %>% 
  select(user_id, username, ep_int, 
         ep_start, ep_end, ep_dur) %>% 
  mutate(interval.id = row_number())

#  Step 4: apply feature extraction function (see App_features.R)
dd_time <- app.features(df_app_act, cat_app, dd_time, "not_used", # define device.info.data as "not_used" for DIPS3 (argument not needed)
                        categories = c("Communication", "Social_Media", "Audio_Entertainment", 
                                       "Career", "Creativity", "Dating", "Finance", "Food",
                                       "Gaming", "Health", "Internet", "Knowledge", "News",
                                       "Orientation", "Photo", "Reading", "Security", "Settings",
                                       "Shopping", "Spirituality", "System", "Time", "Tools",
                                       "Transportation", "Visual_Entertainment", "Weather"
                        ), 
                        categories.names = c("comm", "socmed", "audio", "career", "create",
                                             "dating", "finance", "food", "game", "health", 
                                             "internet", "know", "news", "orient", "photo",
                                             "read", "security", "settings", "shop", "spirit",
                                             "system", "time", "tools", "transport", "visual",
                                             "weather"))# not enough instances of people using dating apps in our sample ("Dating")
dd_time %>% print(width=Inf)

# all categories combined
dd_time <- dd_time %>% 
  mutate(comb_app_freq = 
           
           comm_app_freq + socmed_app_freq + audio_app_freq
         + career_app_freq + create_app_freq + dating_app_freq + finance_app_freq
         + food_app_freq + game_app_freq + health_app_freq + internet_app_freq
         + know_app_freq + news_app_freq + orient_app_freq + photo_app_freq
         + read_app_freq + security_app_freq + settings_app_freq + shop_app_freq
         + spirit_app_freq + system_app_freq + time_app_freq + tools_app_freq
         + transport_app_freq + visual_app_freq + weather_app_freq,
         
         comb_app_time = 
           
           comm_app_time + socmed_app_time + audio_app_time
         + career_app_time + create_app_time + dating_app_time + finance_app_time
         + food_app_time + game_app_time + health_app_time + internet_app_time
         + know_app_time + news_app_time + orient_app_time + photo_app_time
         + read_app_time + security_app_time + settings_app_time + shop_app_time
         + spirit_app_time + system_app_time + time_app_time + tools_app_time
         + transport_app_time + visual_app_time + weather_app_time)

### create summary data frame for collecting all users' extracted features
ms_apps_summaries <- dd_time %>% 
  select(user_id, ep_int, comm_app_freq, comm_app_time,
         socmed_app_freq, socmed_app_time, comb_app_freq, comb_app_time,
         audio_app_freq, audio_app_time, career_app_freq, career_app_time,
         create_app_freq, create_app_time, dating_app_freq, dating_app_time,
         finance_app_freq, finance_app_time, food_app_freq, food_app_time,
         game_app_freq, game_app_time, health_app_freq, health_app_time,
         internet_app_freq, internet_app_time, know_app_freq, know_app_time,
         news_app_freq, news_app_time, orient_app_freq, orient_app_time,
         photo_app_freq, photo_app_time, read_app_freq, read_app_time,
         security_app_freq, security_app_time, settings_app_freq,
         settings_app_time, shop_app_freq, shop_app_time, spirit_app_freq,
         spirit_app_time, system_app_freq, system_app_time, time_app_freq,
         time_app_time, tools_app_freq, tools_app_time, transport_app_freq,
         transport_app_time, visual_app_freq, visual_app_time, weather_app_freq,
         weather_app_time)

### run feature extraction function for all users

for (i in 1:length(user_ids)){
  user = user_ids[i]
  print(paste0("Run ", i, ", User ", user))
  
  # Step 1: read sensing data for a specified user and apply timestamp correction 
  df_app_act = dplyr::tbl(con, "ps_activity") %>% dplyr::filter(user_id %in% user) %>%
    dplyr::filter(!activityName %in% c("PHONESTUDY", "DNAPSOUND", "DNAPACCELEROMETER", "DNAPLIGHT", 
                                       "DNAPPROXIMITY", "DNAPGYROSCOPE", "AWARECONVERSATIONS", "CONTACTS")) %>% 
    dplyr::mutate( created_at = as.character(created_at), 
                   updated_at = as.character(updated_at)) %>% data.frame()
  df_app_act$user_id = as.character(df_app_act$user_id) #recode user id variable as character to avoid confusion
  df_app_act = ps_activity_preproc_timestamps(df_app_act) # correct timestamps 
  
  # Step 2: extract device information
  # device = unique(df_app_act$deviceinfo_id)[!is.na(unique(df_app_act$deviceinfo_id))]
  # device.i = device.info[which(device.info$id == device[1]),]
  # 
  # if(nrow(device.i)==0){ # for some reason, we have exactly 1 user without device information (1489)
  #   device.i[1, "sdkVersion"] = ifelse("SCREEN_INTERACTIVE" %in% # if we have this type of event it is 28 or higher
  #                                        (df_app_act %>% filter(activityName=="APPS") %>% 
  #                                           group_by(event) %>% tally() %>% pull(event)), 28, 27)
  # }
  
  # Step 3: prepare in ESM data the time windows in which to aggregate app usage
  # (here, timestamps are already corrected!)
  dd_time <- df_ep_hourly %>% filter(user_id==user) %>% 
    select(user_id, username, ep_int, 
           ep_start, ep_end, ep_dur) %>% 
    mutate(weekday=wday(ep_start, label=T),
           interval.id = row_number())
  
  if(nrow(df_app_act)>1){
    #  Step 4: apply feature extraction function (see App_features.R)
    dd_time <- app.features(df_app_act, cat_app, dd_time, "not_used", # define device.info.data as "not_used" for DIPS3 (argument not needed)
                            categories = c("Communication", "Social_Media", "Audio_Entertainment", 
                                           "Career", "Creativity", "Dating", "Finance", "Food",
                                           "Gaming", "Health", "Internet", "Knowledge", "News",
                                           "Orientation", "Photo", "Reading", "Security", "Settings",
                                           "Shopping", "Spirituality", "System", "Time", "Tools",
                                           "Transportation", "Visual_Entertainment", "Weather"
                            ), 
                            categories.names = c("comm", "socmed", "audio", "career", "create",
                                                 "dating", "finance", "food", "game", "health", 
                                                 "internet", "know", "news", "orient", "photo",
                                                 "read", "security", "settings", "shop", "spirit",
                                                 "system", "time", "tools", "transport", "visual",
                                                 "weather"))
    # both "social" categories combined
    dd_time <- dd_time %>% 
      mutate(comb_app_freq = 
               
               comm_app_freq + socmed_app_freq + audio_app_freq
             + career_app_freq + create_app_freq + dating_app_freq + finance_app_freq
             + food_app_freq + game_app_freq + health_app_freq + internet_app_freq
             + know_app_freq + news_app_freq + orient_app_freq + photo_app_freq
             + read_app_freq + security_app_freq + settings_app_freq + shop_app_freq
             + spirit_app_freq + system_app_freq + time_app_freq + tools_app_freq
             + transport_app_freq + visual_app_freq + weather_app_freq,
             
             comb_app_time = 
               
               comm_app_time + socmed_app_time + audio_app_time
             + career_app_time + create_app_time + dating_app_time + finance_app_time
             + food_app_time + game_app_time + health_app_time + internet_app_time
             + know_app_time + news_app_time + orient_app_time + photo_app_time
             + read_app_time + security_app_time + settings_app_time + shop_app_time
             + spirit_app_time + system_app_time + time_app_time + tools_app_time
             + transport_app_time + visual_app_time + weather_app_time)
    
    # create summary data frame for collecting all users' extracted features
    ms_apps_summary <- dd_time %>% 
      select(user_id, ep_int,
             comm_app_freq, comm_app_time, socmed_app_freq, socmed_app_time, 
             comb_app_freq, comb_app_time,
             audio_app_freq, audio_app_time, career_app_freq, career_app_time,
             create_app_freq, create_app_time, dating_app_freq, dating_app_time,
             finance_app_freq, finance_app_time, food_app_freq, food_app_time,
             game_app_freq, game_app_time, health_app_freq, health_app_time,
             internet_app_freq, internet_app_time, know_app_freq, know_app_time,
             news_app_freq, news_app_time, orient_app_freq, orient_app_time,
             photo_app_freq, photo_app_time, read_app_freq, read_app_time,
             security_app_freq, security_app_time, settings_app_freq,
             settings_app_time, shop_app_freq, shop_app_time, spirit_app_freq,
             spirit_app_time, system_app_freq, system_app_time, time_app_freq,
             time_app_time, tools_app_freq, tools_app_time, transport_app_freq,
             transport_app_time, visual_app_freq, visual_app_time, weather_app_freq,
             weather_app_time)
    
  } else{ # for those with no sensing data, create NA filled vars
    print(paste("User", user, "has no sensing data!"))
    ms_apps_summary <- dd_time %>% 
      select(user_id, ep_int) %>% 
      mutate(comm_app_freq=NA, comm_app_time=NA, socmed_app_freq=NA, socmed_app_time=NA, 
             comb_app_freq=NA, comb_app_time=NA)
  }
  ms_apps_summaries <- bind_rows(ms_apps_summaries, ms_apps_summary) # bind information from all users together
}


df_ep_hourly <- df_ep_hourly %>%
  left_join(ms_apps_summaries, by = c('user_id', 'ep_int'))

# check for unrealistic cases:
df_ep_hourly %>% filter(comm_app_time > 60 | socmed_app_time > 60) %>% select(user_id, e_s_questionnaire_id, contains("app")) %>% print(n=Inf)

save(df_ep_hourly, file = "Vanessa/dips3 hourly/df_ep_hourly_pre2.rda")


#### CALLS ----------------------------------------------------------------------

load(file = "Vanessa/dips3 hourly/df_ep_hourly_pre2.rda")

#load communication data
communication <-  DBI::dbReadTable(con, "ps_communication") # needed for ms_get_calls() function
colnames(communication)[which(colnames(communication) == "id")] <- "communication_id"

# loop to match each episode with the corresponding calls data; create summary variables

users <- df_ep_hourly %>% pull(username) %>% unique() 
ms_calls_summaries <- tibble(username=character(), ep_int=interval(), 
                             n_ms_calls=integer(), dur_ms_calls=double())

for (i in 1:length(users)) {
  user = users[i]
  print(paste0("Run ", i, ", User ", user))
  
  df_calls_user = ms_get_calls_d3(user)
  
  if(df_calls_user[[2]]$calls_number!=0){ # all with at least 1 call
    # all recorded calls per user
    df_calls_user = as_tibble(df_calls_user[[1]]) %>% select(user_id, dur, time_start, time_end, timezoneOffset)
    # correct timestamps
    ep_calls_user <- df_calls_user %>% 
      mutate(time_start = time_start + lubridate::dhours(timezoneOffset/(60*60*1000)),
             time_end = time_end + lubridate::dhours(timezoneOffset/(60*60*1000))) %>% select(-timezoneOffset)
    
    #zero calls?
    if(nrow(ep_calls_user)==0){
      
      ms_calls_summary <- tibble(username=user, n_ms_calls=0, dur_ms_calls=0)
    
    }else {
      
      # descriptives per interval 
      ep_calls_user <- ep_calls_user %>%
        mutate(hour = (purrr::map2(floor_date(time_start, "hour"), floor_date(time_end, "hour"), seq, by = "hour"))) %>%
        tidyr::unnest(hour)  %>% 
        
        mutate(min = case_when( 
          
          # intv in which call ended -> take into account the minutes in next hour
          floor_date(time_start, "hour") != hour & floor_date(time_end, "hour") == hour ~ minute(time_end) + second(time_end)/60, 
          # intv in which call started -> count minutes from start time to next full hour
          floor_date(time_start, "hour") == hour & floor_date(time_end, "hour") != hour ~ 60 - (minute(time_start) + second(time_start)/60) , 
          # call is in one hour
          floor_date(time_start, "hour") == hour & floor_date(time_end, "hour")  == hour ~ (minute(time_end) + second(time_end)/60) - (minute(time_start) + second(time_start)/60), 
          
          # call exceeds several hours -> add one hour 
          TRUE ~ 60)) 
      
      ms_calls_summary <- ep_calls_user %>%
        # add ep_int back 
        mutate(
          int_start = hour,
          int_end = int_start + hours(1),
          ep_int = interval(int_start, int_end)) %>%
        select(-int_start, -int_end) %>% 
        group_by(username=user, ep_int) %>% 
        summarise(n_ms_calls=n(), dur_ms_calls=sum(min, na.rm = T)) %>%  # sum of overall calls and duration
        ungroup()
      
      # set n_ms_calls to 0 if dur_ms_calls==0 (there are a few calls with 0s duration)
      ms_calls_summary <- ms_calls_summary %>% 
        mutate(n_ms_calls = ifelse(dur_ms_calls==0, 0, n_ms_calls))
    }
    
    ms_calls_summaries <- ms_calls_summaries %>%
      bind_rows(ms_calls_summary)
  }  
}

# merge with data
df_ep_hourly <- left_join(df_ep_hourly, ms_calls_summaries, by = c("username","ep_int"))

df_ep_hourly <- df_ep_hourly %>%
  mutate_at(vars(n_ms_calls, dur_ms_calls), ~replace_na(., 0))

save(df_ep_hourly, file = "Vanessa/dips3 hourly/df_ep_hourly_pre3.rda")

#### TEXTS ####

load(file = "Vanessa/dips3 hourly/df_ep_hourly_pre3.rda")

users <- df_ep_hourly %>% pull(username) %>% unique() 

# -> needs 2nd database ("con2")
source("scripts/connect_MariaDB2.R")
ms_texts_summaries <- as_tibble(get_texts_d3(users[1], df_ep_hourly)) %>% 
  select(username = user_id, ep_int = episode_int, ms_outgoing, ms_len_outgoing)

for (i in 2:length(users)){
  user = users[i]
  print(paste0("Run ", i, ", User ", user))
  
  ms_texts_summary <- as_tibble(get_texts_d3(users[i], df_ep_hourly)) %>% 
    select(username = user_id, ep_int = episode_int, ms_outgoing, ms_len_outgoing)
  ms_texts_summaries <- bind_rows(ms_texts_summaries, ms_texts_summary) # bind information from all users together
}

# merge with data
df_ep_hourly <- left_join(df_ep_hourly, ms_texts_summaries, by = c("username","ep_int"))

# treat NA -> assume that NAs (i.e., no sensed messages) means that no messages were sent 
# (alternative could be that sensing of messages did not work on some devices for some reason...)
df_ep_hourly <- df_ep_hourly %>% 
  replace_na(list(ms_outgoing = 0, ms_len_outgoing = 0)) 

save(df_ep_hourly, file = "Vanessa/dips3 hourly/df_ep_hourly_pre4.rda")

#### NUM OF CONTACTS -----------------------------------------------------------
### Add number of contacts as another indicator of network size
load(file = "Vanessa/dips3 hourly/df_ep_hourly_pre4.rda")

users <- df_ep_hourly %>% pull(username) %>% unique()

ps_con = DBI::dbReadTable(con3, "ps_contact")

# Remove all contacts with no phone number, no email and no adress
ps_con_filtered <- ps_con[rowSums(!is.na(ps_con[, c("number1", "number2", "number3", "email", "address")])) > 0, ] 

sensing_con <- dplyr::tbl(con, "ps_activity") %>% 
  dplyr::filter(activityName=="CONTACTS" & timestamp>='2022-06-21 00:00:00') %>% 
  dplyr::select(user_id, contact_id) %>% 
  data.frame()

colnames(ps_con_filtered) = c("contact_id",colnames(ps_con_filtered)[-1])
ps_con2 = left_join(ps_con_filtered,sensing_con) 

ps_con3 <- filter_users_d3(ps_con2) # defined in subscript

unique_phone_numbers = ps_con3 %>% 
  group_by(user_id) %>%
  summarise(unique_numbers = n_distinct(number1))
hist(unique_phone_numbers$unique_numbers, breaks = seq(0,3000,25))

unique_phone_numbers$username = NA
for(i in 1:nrow(unique_phone_numbers)){
  unique_phone_numbers[i, "username"] = user_app_name(unique_phone_numbers[[i, "user_id"]],con = con3)
}
unique_phone_numbers2 = unique_phone_numbers %>% filter(username %in% users)

#add username to this before join
df_ep_hourly = left_join(df_ep_hourly, unique_phone_numbers)

save(df_ep_hourly, file = "Vanessa/dips3 hourly/df_ep_hourly_pre5.rda")

#### CHECK NA DATA -------------------------------------------------------------

# AWARE DATA

df_aware <- dplyr::tbl(con, "ps_activity") %>% 
  dplyr::filter(activityName=="AWARECONVERSATIONS" & timestamp>='2022-06-21 00:00:00') %>% 
  dplyr::select(user_id, id, timestamp_inv = timestamp, # create valid timestamp from 'description' below
                timezoneOffset, description, client_db_id, created_at, updated_at) %>% 
  data.frame()

df_aware <- filter_users_d3(df_aware) # defined in subscript


View(df_ep_hourly %>%
       filter(user_id == "2055")) 

df_ep_hourly %>%
  filter(user_id == "2055") %>%
  count(n_aware)

df_ep_hourly %>%
  filter(user_id == "2055") %>%
  select(ends_with(c("_freq","_time"))) %>%
  count()

# APPS DATA

View(df_ep_hourly %>%
  select(user_id, ends_with(c("_freq","_time"))) %>%
  filter(if_any(everything(), is.na)))

#### RECODE FOR STATA ---------------------------------------------------------

load(file = "Vanessa/dips3 hourly/df_ep_hourly_pre5.rda")
load(file = "dips3/data/df_dips3_pre7.rda")

# add subsample to df_ep_hourly back
df_dips3 <- df_dips3 %>% select(username, subsample) %>% unique()

df_ep_hourly <- df_ep_hourly %>%
  left_join(df_dips3, by = "username")

# add day, weekend
df_ep_hourly <- df_ep_hourly %>% mutate(day=wday(ep_start, label=T),
                                weekend = ifelse(day=="Sat" | day=="Sun", 1, 0))

# remove revealing information
df_ep_hourly <- df_ep_hourly %>% 
  select(-e_s_questionnaire_id, -user_id, -n_ms_calls, -dur_ms_calls,
         -n_diary, -starts_with("ep_"))

# rename variables
df_ep_hourly <- df_ep_hourly %>%
  rename(texts_outgoing_freq = ms_outgoing,
         texts_outgoing_length = ms_len_outgoing)

df_ep_hourly <- df_ep_hourly %>%
  rename(orientat_app_freq = orient_app_freq,
         orientat_app_time = orient_app_time)

#recode scales
df_ep_hourly <- df_ep_hourly %>%
  mutate(subsample = recode(subsample, "no" = "0", "yes" = "1"),
         day = recode(day, `Mon` = 0L, `Tue` = 1L, `Wed` = 2L, `Thu` = 3L, 
                      `Fri` = 4L, `Sat` = 5L, `Sun` = 6L))

#remove variables not neeeded for publication
df_ep_hourly <- df_ep_hourly %>%
  select(-system_app_freq,-system_app_time, -comb_app_freq, -comb_app_time, -unique_numbers)

#### ID MERGING ####

#list taken from dips3_extract
load(file = "Vanessa/dips3/df_soepis.rda")

# merge via "Phonestudy" ID -> 'username'
df_ep_hourly <- left_join(df_ep_hourly, df_soepis %>% rename(username = phonestudy_id, pid_soep = pid)) %>%
  select(username, pid_soep, everything())

df_ep_hourly <- df_ep_hourly %>% filter(!is.na(pid_soep))

df_ep_hourly <- df_ep_hourly %>% select(-username) %>%
  select(pid=pid_soep, subsample, day, weekend, n_days, everything())

# add syear variable for tracking purposes
df_ep_hourly <- df_ep_hourly %>%
  mutate(syear = 2022) %>%
  select(pid, subsample, syear, everything())

save(df_ep_hourly, file = "Vanessa/dips3 hourly/df_ep_hourly.rda")

#### EXPORT TO STATA ####

# save dataset as .dta file to work in stata
write_dta(df_ep_hourly, "Vanessa/dips3 hourly/dips3_hourly.dta")
