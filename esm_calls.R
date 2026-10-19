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

options(scipen = 999) # prints numeric values in fixed rather than in exponential notation

#### GET ESM DATA

load(file = "dips3/data/df_dips3_pre6.rda")
load(file = "Vanessa/dips3 hourly/df_ep_hourly_raw.rda")

# get timestamp info from dips data

# add subsample var to df_ep_hourly back
df_dips3 <- df_dips3 %>% select(username, subsample) %>% unique()

df_ep_hourly <- df_ep_hourly %>%
  left_join(df_dips3, by = "username")

# build daily diary data
dd_list <- build.dd()
list2env(dd_list, .GlobalEnv)
rm(dd_list)

# correct timestamps
df_dd <- df_dd %>% fix_timestamp_dd()

# filter esm call questionnaires from daily diaries created above
df_ep <- df_dd %>% filter(meta_json=="external app") %>% 
  select(username, subsample, ts_notif, ts_q_started, ts_q_ended, starts_with("trig_")) %>%
  arrange(username, ts_q_started, ts_q_ended)
  
# merge n_diary, n_days to esm data
df_ep <- df_ep %>%
  mutate(ep_start = floor_date(ts_q_started, "day"))

df_ep_hourly <- df_ep_hourly %>%
  select(username, subsample, n_diary, n_days, ep_start) %>%
  mutate(ep_start = floor_date(ep_start, "day")) %>%
  unique()

df_ep <- df_ep %>%
  left_join(df_ep_hourly, by = c("username", "subsample", "ep_start"))

df_ep <- df_ep %>%
  filter(n_days >= 1 & (n_diary <= 14 | (is.na(n_diary) & n_days <= 14)))

### TEST: if df_dips3 results in same outcome

df_dips3 <- df_dips3 %>%
  select(username, subsample, n_diary, n_days, ep_start) %>%
  mutate(ep_start = floor_date(ep_start, "day")) 

# build daily diary data
dd_list <- build.dd()
list2env(dd_list, .GlobalEnv)
rm(dd_list)

# correct timestamps
df_dd <- df_dd %>% fix_timestamp_dd()

# filter episodes from daily diary data created above

df_ep <- df_dd %>% filter(meta_json=="external app") %>% 
  select(username, subsample, ts_notif, ts_q_started, ts_q_ended, starts_with("trig_")) %>%
  arrange(username, ts_q_started, ts_q_ended)

df_ep <- df_ep %>%
  mutate(ep_start = floor_date(ts_q_started, "day"))

# merge n_diary, n_days

df_eptest2 <- df_ep %>%
  left_join(df_dips3, by = c("username", "subsample", "ep_start"))

#### ADD DAY, WEEKEND VARIABLE -------------------------------------------------

load(file = "dips3/data/df_dips3.rda")

df_dips3 <- df_dips3 %>% select(username, subsample, n_diary, n_days, day, weekend)

df_ep <- left_join(df_ep, df_dips3) %>%
  select(username, n_diary, n_days, day, weekend, everything())

#### ID MERGING ----------------------------------------------------------------

#list taken from dips3_extract
load(file = "Vanessa/dips3/df_soepis.rda")

# merge via "Phonestudy" ID -> 'username'
df_ep <- left_join(df_ep, df_soepis %>% rename(username = phonestudy_id, pid_soep = pid)) %>%
  select(username, pid_soep, everything())

df_ep <- df_ep %>% filter(!is.na(pid_soep))

df_ep <- df_ep %>%
  select(pid=pid_soep, subsample, n_days, n_diary, day, weekend, ts_notif, starts_with("trig_"))

save(df_ep, file = "dips3/data/trig_calls.rda")

#### RECODE FOR STATA ---------------------------------------------------------

load(file = "dips3/data/trig_calls.rda")

# recode scales
df_ep <- df_ep %>% 
  mutate(trig_partner = case_when(trig_partner == "Partner" ~ 1L, 
                                  trig_partner == "Kinder" ~ 2L, 
                                  trig_partner == "Weitere Familie" ~ 3L, 
                                  trig_partner == "Freunde" ~ 4L, 
                                  trig_partner == "Kollegen" ~ 5L, 
                                  trig_partner == "Fremde" ~ 6L, 
                                  trig_partner == "Sonstige" ~ 7L),
         day = recode(day, `Mon` = 0L, `Tue` = 1L, `Wed` = 2L, `Thu` = 3L, 
                      `Fri` = 4L, `Sat` = 5L, `Sun` = 6L),
         subsample = recode(subsample, "no" = "0", "yes" = "1"))

# replace NA values with -1 (no data available) or -2 (not applicable)
na_cols <- colnames(df_ep %>% select(trig_partner, trig_valence))
df_ep <- df_ep %>%
  mutate_at(na_cols, replace_na, -2)

df_ep <- df_ep %>%
  mutate_all(replace_na, -1)

# add syear variable for tracking purposes
df_ep <- df_ep %>%
  mutate(syear = 2022)

df_ep <- df_ep %>%
  select(pid, subsample, syear, everything())

#### EXPORT TO STATA ----------------------------------------------------------

# save dataset as .dta file to work in stata
write_dta(df_ep, "Vanessa/dips3/dips3_calls.dta")
































