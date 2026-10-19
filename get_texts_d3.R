get_texts_d3 = function(user, df_esm){
  username = user_app_name(user)
  
  esm_user = df_esm[which(df_esm$username == username), 
                    c("username","ep_start","ep_end", "ep_int")]
  
  #initialise output-dataset
  row_numbers = ifelse(nrow(esm_user)==0,1,nrow(esm_user))
  texts_user = data.frame(matrix(data = NA, nrow = row_numbers, ncol = 12, dimnames = 
                                   list(1:row_numbers,c("user_id","episode_start","episode_end", "ep_int", "ms_timestamp",
                                                        "ms_incoming","ms_from","ms_len_incoming",
                                                        "ms_outgoing","ms_to","ms_len_outgoing",
                                                         "day"))))
  if(nrow(esm_user) == 0){
    print(paste("no esm for user",user))
    return(texts_user)
  }
  
  #get information from esm_user
  texts_user$user_id = user
  texts_user$episode_start = esm_user$ep_start
  texts_user$episode_end = esm_user$ep_end
  texts_user$episode_int = esm_user$ep_int
  #texts_user$es_t_read = esm_user$t_read
  #texts_user$es_t_send = esm_user$t_send
  #texts_user$es_t_to   = esm_user$t_to
  #texts_user$es_t_from = esm_user$t_from
  #texts_user$es_e_read = esm_user$e_read
  #texts_user$es_e_send = esm_user$e_send
  
  
  #get text data from user
  
  messtats_user = dplyr::tbl(con2, "message_statistics")  %>% dplyr::filter(user_uuid %in% username) %>% data.frame()
  
  if(nrow(messtats_user) == 0){
    print(paste("no text typed",user, "episodes number is", nrow(texts_user)))
    return(texts_user)
  }
  
  #read in list of communication apps and allowlist of text hints - better to do this in main script later:
  com_apps = read.table("preprocessing/communication_apps.txt")$V1 # MK: changed path to (public) 'preprocessing'
  bemmann_et_al_list = read.csv("preprocessing/hint_text_cats.txt", sep = "\t")
  communication_hints = bemmann_et_al_list %>% filter(Kategorie.Final == "Messaging")
  allowlist_text_hints_dips = c(NA,"Chat", "Bitte geben Sie hier Ihre Nachricht ein","Antworten",
                                "Chatnachricht","Füg eine Nachricht hinzu",
                                "Gib eine Nachricht ein ...","Message","Nachricht",
                                "Nachricht schreiben","Nachricht eingeben","Nachrichtentext. Zum Bearbeiten doppeltippen",
                                "Sende eine Nachricht", "Signal-Nachricht", "Textnachricht", "Tippen, um Text einzugeben", 
                                "Text eingeben", "Textnachricht (o2 - de)", "Write your message here",
                                "Введите сообщение...","Сообщение","Текст сообщения",
                                "Текст сообщения. Коснитесь дважды, чтобы редактировать",
                                "Füge eine Nachricht hinzu","Attach a message", "Geben Sie eine Nachricht ein", 
                                "Chat-Nachricht eingeben", "Nachricht eingeben...", "Enter message", "Type a message…", 
                                "Add a reply...", "Tippen Sie hier zum Chatten")
  
  allowlist_text_hints_bemmann_et_al = communication_hints$field_hint_text
  allowlist_text_hints = c(allowlist_text_hints_dips, allowlist_text_hints_bemmann_et_al)
  
  messtats_user = messtats_user %>% filter(input_target_app %in% com_apps) %>% filter(field_hint_text %in% allowlist_text_hints)
  
  if(nrow(messtats_user) == 0){
    print(paste("no text typed in com apps", user, "episodes number is", nrow(texts_user)))
    return(texts_user)
  }
  
  messtats_user$timestamp_type_start = lubridate::as_datetime(as.numeric(messtats_user$timestamp_type_start/1000))
  messtats_user$timestamp_type_end = lubridate::as_datetime(as.numeric(messtats_user$timestamp_type_end/1000))
  
  ###correct timestamps
  timezone_info = read.csv("data/timezone_info_DIPS3.csv")
  correct.cols = c("timestamp_type_start","timestamp_type_end")
  
  if(is.na(timezone_info[which(timezone_info$username == username), "timezone_switch"]) == F){
    for(i in 1:nrow(messtats_user)){
      row = messtats_user[i, correct.cols]
      #check if user has timezone_switch
      if(timezone_info[which(timezone_info$username == username), "timezone_switch"] == F){
        messtats_user[i, correct.cols] = sapply(row, function(x) (lubridate::ymd_hms(x) + lubridate::hours(timezone_info[which(timezone_info$username == username),"timezoneOffset1"]/(60*60*1000))), simplify =  F)
      } else {
        latest = max(do.call("c", row),na.rm = T)
        Offset_1 = latest %within% interval(timezone_info[which(timezone_info$username == username), "timezone1_start"],timezone_info[which(timezone_info$username == username), "timezone1_end"])
        if(Offset_1){
          #print(paste("user", user, "row", i, "Offset1 used"))
          messtats_user[i, correct.cols] = sapply(row, function(x) (lubridate::ymd_hms(x) + lubridate::hours(timezone_info[which(timezone_info$username == username),"timezoneOffset1"]/(60*60*1000))), simplify =  F)
        } else {
          #print(paste("user", user, "row", i, "Offset2 used"))
          messtats_user[i, correct.cols] = sapply(row, function(x) (lubridate::ymd_hms(x) + lubridate::hours(timezone_info[which(timezone_info$username == username),"timezoneOffset2"]/(60*60*1000))), simplify =  F)
        }
      }
    }
  } else {print(paste("no timestamp correction for user", user))}
  
  ###filter artifacts###
  messtats_user = messtats_user %>% mutate(timestamp_type_start_lag1 = lag(timestamp_type_start))
  puffer = messtats_user$timestamp_type_start_lag1 + lubridate::seconds(0.1)
  
  overlap = c(FALSE, messtats_user$timestamp_type_start[-1] %within% 
                interval(messtats_user$timestamp_type_start_lag1[-1], puffer[-1]))
  messtats_user = messtats_user[which(overlap == F),]
  if(sum(overlap >= 1)){
    print(paste("user", user, "has", sum(overlap), "text artifact(s)"))
  }
  
  # for each episode, check how many texts were written/received?
  
  ms_timestamp = messtats_user$timestamp_type_end
  
  for(i in 1:nrow(texts_user)){
    episode_start = texts_user[i,"episode_start"]
    episode_end = texts_user[i,"episode_end"]
    
    interv = interval(episode_start,episode_end)
    matches = ms_timestamp %within% interv
    outgoing = messtats_user[matches,] 
    #timestamps = paste(sms_timestamp[matches],collapse=" ")
    #texts_user[i, sms_timestamp] = timestamps
    texts_user[i, "ms_outgoing"] = nrow(outgoing)
    texts_user[i, "ms_len_outgoing"] = sum(outgoing$character_count_submitted)
  }
  
  #aae_user = dplyr::tbl(con2, "abstracted_action_event")  %>% dplyr::filter(user_uuid == username) %>% data.frame() 
  #its not possible to say how many people were texted from ms data
  
  return(texts_user)
}

### are sms already counted? -> Yes com.google.android.apps.messaging
