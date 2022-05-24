library(tidyverse)
library(readxl)

data_folder = "D:\\phd\\jump load\\data\\"
d_all = read_excel(paste0(data_folder, "Final Data Set_Daily_Weekly_Pre.xlsx"), "FDS_Daily_Pre_All", na = "", skip = 4)
d_all = d_all %>% tibble()

# need to convert integer to date, excel origin date is 1899-12-30
d_all$Date = as.Date(d_all$Date,format="%Y-%m-%d", origin = "1899-12-30")
d_all = d_all %>% mutate(year = lubridate::year(Date))
d_all = d_all %>% mutate(month_day = format(as.Date(d_all$Date,format="%Y-%m-%d"), format = "%m-%d")) 

# make season-year variable
d_all = d_all %>% mutate(season = 
                          case_when(TeamSeason == "A-1" | TeamSeason == "B-1" | TeamSeason == "C-1" | TeamSeason == "D-1" ~ "2017/2018", 
                                    TeamSeason == "A-2" | TeamSeason == "B-2" | TeamSeason == "D-2" ~ "2018/2019",
                                    TeamSeason == "A-3" ~ "2019/2020")) 

# fix so "-" means not applicable
d_all = d_all %>%mutate_if(is.character, ~ifelse(. == "-", "not applicable", .))

# add match variable and fix missing session-types
d_all = d_all %>% mutate(SessionType = ifelse(is.na(SessionType), "no volleyball", SessionType))
d_all = d_all %>% mutate(Match = as.character(ifelse(SessionType == "match", 1, 0)))
# calculate number of days since previous match
d_match = d_all %>% select(Date, PlayerID , Team, Match)

# find match dates
d_match_index = d_match %>% 
  filter(Match == 1) %>% 
  group_by(Team, PlayerID) %>% 
  mutate(match_index = 1:n()) %>% 
  select(-Match) %>% 
  ungroup() %>% rename(match_date = Date)

# join with the match index data to find
# the match considered the "previous" match
# note that if the current day is a match, we will still calculate
# days from the previous match
# also, the first match is NA, and all days before the first match is NA
# because we don't know how long they went without a match in the very first match in the season
d_all_prevmatches = d_all %>% 
  left_join(d_match_index, by = c("Team", "PlayerID", "Date" = "match_date")) %>% 
  group_by(Team, PlayerID) %>% 
  fill(match_index) %>% 
  left_join(d_match_index, by = c("Team", "PlayerID", "match_index")) %>% 
  group_by(Team, PlayerID) %>% mutate(match_date = lag(match_date))

# calculate time since previous match
d_all = d_all_prevmatches %>% 
  mutate(t_prevmatch = as.numeric(difftime(Date, match_date, "days"))) %>% 
  select(-match_date, -match_index)

# calculate sum of jump heights
# and how many cm they technically could have jumped
d_all = d_all %>% mutate(jump_height_sum = jump_height_avg_cm*jumps_n,
                         jump_height_sum = ifelse(SessionType == "no volleyball", 0, jump_height_sum)
                         )


# add complaint yes/no variables
d_all = d_all %>% mutate(inj_knee = ifelse(Knee_1 >= 1| Knee_2 >= 1 | Knee_3 >= 1 | Knee_4 >= 1, 1, 0),
                         inj_knee_subst = ifelse(Knee_2 >= 17 | Knee_3 >= 17, 1, 0),
                         inj_shoulder = ifelse(Shoulder_1 >= 1| Shoulder_2 >= 1 | Shoulder_3 >= 1 | Shoulder_4 >= 1, 1, 0),
                         inj_shoulder_subst = ifelse(Shoulder_2 >= 17 | Shoulder_3 >= 17, 1, 0),
                         inj_lowback = ifelse(LowBack_1 >= 1| LowBack_2 >= 1 | LowBack_3 >= 1 | LowBack_4 >= 1, 1, 0),
                         inj_lowback_subst = ifelse(LowBack_2 >= 17 | LowBack_3 >= 17, 1, 0))

# baseline injuries
# add column for baseline injury
d_bl_injury = d_all %>% 
  group_by(PlayerID) %>% 
  slice(1) %>% 
  select(Date, Team, TeamSeason, PlayerID, 
         inj_knee, inj_shoulder, inj_lowback) %>% 
  mutate(inj_bl = case_when(inj_shoulder == 1 ~ 1,
                            inj_knee == 1 ~ 1,
                            inj_lowback == 1 ~ 1,
                            TRUE ~ 0)
         ) %>% ungroup() %>% select(Date, Team, TeamSeason, PlayerID, inj_bl)

d_all = d_all %>% left_join(d_bl_injury, by = c("Date", "PlayerID", "Team", "TeamSeason"))

# checking that no jump height sums are unrealistically extreme
d_jump_height_test = d_all %>% select(Date, PlayerID, jumps_n, jump_height_sum) %>% 
                               mutate(max_height_possible = jumps_n * 120,
                               within_limits_no = ifelse(jump_height_sum <= max_height_possible, 0, 1))
n_errors = d_jump_height_test %>% summarise(n_errors = sum(within_limits_no == 1, na.rm = TRUE))

# look at histograms. Any negative values or other abnormalities?
hist(d_all$jumps_n)
hist(d_all$jump_height_sum)
hist(d_all$age)
hist(d_all$TotalDailyKELoadIndex)

d_all %>% count(Team)
d_all %>% count(Position)
d_all %>% count(SessionType)
d_all %>% count(`Season Phase`)
d_all %>% count(MatchParticipation) 

key_cols = c("date", "id_player", "id_team", "id_team_player", "id_season")

d_all = d_all %>% rename(date = Date, 
                         id_team = Team, 
                         season_phase = `Season Phase`,
                         id_season = TeamSeason,  
                         id_team_player = Team_Player_ID,
                         id_player = PlayerID,
                         session_type = SessionType,
                         position = Position)

d_baseline = read_delim(paste0(data_folder,"d_baseline.csv"), delim = ";", na = "")

d_all = d_all %>% left_join(d_baseline %>% select(any_of(key_cols), weight, height), 
                    by = c("id_player", "id_team", "id_team_player", "id_season"))

# write csv to read in other scripts
# write .csv
# write_delim is preferable, but write_excel_csv is required for excel to understand
# that the file encoding is UTF-8
# write_excel_csv(d_all, paste0(data_folder, "d_volleyball.csv"), delim = ";", na = "")

#---------------------------------------------------exposure data
d_daily = d_all %>% select(all_of(key_cols), 
                 starts_with("Knee"), 
                 starts_with("Shoulder"), 
                 starts_with("LowBack"), 
                 starts_with("inj"), 
                 year, month_day, season, 
                 season_phase, session_type,
                 age, position,
                 weight,
                 height,
                 starts_with("Match"),
                 session_type,
                 t_prevmatch)
remove(d_all)
# column specs for jump data
jump_level_cols = cols(
  .default = col_double(),
  date = col_date(format = ""),
  datetime = col_character(),
  match_number = col_character(),
  session_type = col_character(),
  position = col_character(),
  game_type = col_character(),
  imputed = col_character(),
  id_player = col_character(),
  id_team_player = col_character(),
  id_team = col_character(),
  id_season = col_character(),
  height_KE_updated = col_character()
)

d_jump_all = read_delim(paste0(data_folder,"data_per_jump.csv"), delim = ";", na = "", col_types = jump_level_cols)
#d_jump_all = d_jump_all %>% mutate(imputed = ifelse(is.na(imputed), "No", imputed))
#d_jump_all = d_jump_all %>% mutate(jump_height = ifelse(imputed == "Yes", NA, jump_height))

# how many missing jump heights?
d_jump_all %>% summarise(n_m_height = sum(imputed == "Yes"),
                         denom = n(),
                         prop = n_m_height/denom,
                         perc = 100*prop)

# how about missing daily jumps?
d_unimputed = d_jump_all %>% filter(imputed == "No") 

d_unimputed = 
  d_unimputed %>% mutate(n_jump = 1) %>% 
  group_by(id_player, date) %>% 
  mutate(jump_daily_n = sum(n_jump),
         jump_daily_n = ifelse(session_type == "no volleyball", 0, jump_daily_n),
         jump_height_sum = sum(jump_height, na.rm = TRUE),
         jump_height_sum = ifelse(session_type == "no volleyball", 0, jump_height_sum)) %>% ungroup()

d_unimputed_daily = d_unimputed %>% 
  distinct(date, id_player, id_team, id_team_player, id_season, session_type, .keep_all = TRUE)

d_unimputed_daily = d_unimputed_daily %>% select(all_of(key_cols), session_type, game_type, jump_daily_n, 
                             jump_height_sum, jump_height_max, 
                             jump_height_max_percent,
                             height_ke_modified, load_index_KE, height_KE_updated)

d_daily = d_daily %>% mutate(id_player = as.character(id_player))

# fixme! 8000 jumps ish that don't have player ID
# heights and weights calc wrong
# join daily unimputed with the daily data
d_daily_jumps = d_daily %>% left_join(d_unimputed_daily, by = key_cols)

d_daily_jumps = d_daily_jumps %>% 
  mutate(jump_daily_n = ifelse(session_type == "no volleyball", 0, jump_daily_n)) 

d_daily_jumps = d_daily_jumps %>% arrange(desc(height)) %>% group_by(id_player) %>% fill(height) %>% ungroup()
d_daily_jumps = d_daily_jumps %>% arrange(desc(weight)) %>% group_by(id_player) %>% fill(weight) %>% ungroup()
d_daily_jumps = d_daily_jumps %>% arrange(date)

d_daily_jumps %>% 
  summarise(n_missing_daily = sum(is.na(jump_daily_n)), 
                                  denom = n(), 
                                  prop = n_missing_daily/denom, 
                                  perc = 100*prop)


write_excel_csv(d_daily_jumps, 
                "d_jump_daily.csv", 
                delim = ";", na = "")

# checking missing dates
dates_matchpractice = d_jump_all %>% filter(session_type == "match" | session_type == "practice" | session_type == "friendly") %>% distinct(date)
dates_matchpractice_daily = d_daily %>% filter(session_type == "match" | session_type == "practice" | session_type == "friendly") %>% distinct(date)
setdiff(dates_matchpractice, dates_matchpractice_daily)

dates = d_jump_all  %>% distinct(date)
dates_daily = d_daily %>% distinct(date)
diffdates = setdiff(dates_daily, dates)

d_daily %>% filter(date %in% diffdates$date) %>% count(session_type)



d_jump_all %>% filter(date == "2018-09-21")
d_jump_all %>% filter(date == "2018-09-15") %>% View()

d_jump_all %>% filter(date == "2017-09-04") %>% View()
d_daily %>% filter(date == "2017-09-04") %>% View()

d_jump_all %>% select(key_cols)
d_daily %>% select(key_cols, inj_knee)