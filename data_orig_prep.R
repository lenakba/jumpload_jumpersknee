library(tidyverse)
library(readxl)

data_folder = "D:\\phd\\jump load\\data\\"
file_name = "Raw Jump Data.xlsx"
sheets = excel_sheets(paste0(data_folder, file_name))

# read all sheets in file
load_sheets = sheets[which(str_detect(sheets, "[[:alpha:]]-\\d"))]
d_original = data.frame()
for(i in load_sheets){
  if(i == "B-1" | i == "B-2"){
    temp_data = read_excel(paste0(data_folder, file_name), sheet = i, skip = 1)
  } else {
    temp_data = read_excel(paste0(data_folder, file_name), sheet = i)      
    }
  d_original = rbind(d_original, temp_data)
  d_original
}

# Team B sometimes added practice but specified match sets
# this is because they had a practice and match on the same day
# B was the only Team that registered data at the session-level
# all other teams registered at the daily level
d_original = d_original %>% mutate(SessionType = 
  ifelse((SessionType == "practice") & (GameClassification == "G3" |GameClassification == "G4"), 
         "match", SessionType))


improved_names = c("date", "datetime", "time_seconds", "jump_height", "match_number", "session_type", 
  "jump_height_max", "jump_height_max_percent", "position", "game_type", "imputed", 
  "jump_height_max_0.1", "jump_height_max_percent_0.1", "id_team_player", 
  "id_player", "id_team", "id_season", "weight", "height_ke_modified", "load_index_KE", "height_KE_updated")
colnames(d_original) = improved_names

# fix dates and times
d_original = d_original %>% mutate(time_seconds = as.numeric(time_seconds),
                      datetime = str_replace(datetime, "T", " "),
                      datetime = str_extract(datetime, 
                      "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2} [0-9]{2}\\:[0-9]{2}\\:[0-9]{2}\\.[0-9]{3}"),
                      datetime = as.character(as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%OS")),
                      date = lubridate::as_date(date))

# write to .csv file
write_excel_csv(d_original, 
                "data_per_jump.csv", 
                delim = ";", na = "")

baseline = sheets[!sheets %in% load_sheets]
d_baseline = read_excel(paste0(data_folder, file_name), sheet = baseline, skip = 2)

improved_names_bl = c("id_team", "season", "id_player", "id_team_player", "position",
                   "jump_height_max", 
                   "jump_height_max_rank", 
                   "jump_height_max_rank2cat", 
                   "jump_height_max_rank3cat", 
                   "jump_height_max_0.1", 
                   "weight", "height", "match_participation")

colnames(d_baseline) = improved_names_bl
