# Create final user list for DIPS3 study 

# Treat duplicates IDs (infas IDs that were scanned twice during enrollment)

user_list_final <- read.csv("data/dips3_finallist_2023-04-27.csv")
matching_list <- read.csv("data/7705_pid_apppwd.csv", sep = ";")

dips3_users <- left_join(user_list_final, matching_list %>% rename(infas_id = apppwd))
dips3_users %>% filter(is.na(pid))
dips3_users %>% group_by(infas_id) %>% summarize(dupl = n()) %>% arrange(dupl) %>% print(n=Inf)
table(dips3_users$qrcode)

load("data/count_valids_dips3.rda")
count_valids <- count_valids %>% left_join(dips3_users %>% rename(username = phonestudy_id))
count_valids %>% filter(is.na(pid)) # thank God
