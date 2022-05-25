library(tidyverse)
library(readxl)
library(lubridate) # for wrangling dates and datetimes
library(slider) # iterative calculations across a window

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
  group_by(Team, PlayerID) %>% mutate(match_date = lag(match_date)) %>% ungroup()

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


# find which days the OSTRC-questionnaire actually pertains to
# since each answer is for the previous 6 days including the current day
d_ostrc_dates = d_all %>% select(date, id_player, inj_knee) %>% filter(!is.na(inj_knee))
d_ostrc_dates = d_ostrc_dates %>% mutate(date_first = date-6)

nested_list = d_ostrc_dates %>% group_by(id_player) %>% nest()
nested_list$data = nested_list$data %>% 
  map(., ~map2(.x = .$date_first, .y = .$date, .f = ~seq(ymd(.x), ymd(.y), by = "1 day")))
nested_list$data = nested_list$data %>% map(., ~do.call("c", .))
d_ostrc_dates_valid = unnest(nested_list, cols = data) %>% ungroup() %>% rename(date = data)

d_ostrc_dates_valid = d_ostrc_dates_valid %>% 
  left_join(d_ostrc_dates, by = c("id_player", "date")) %>% 
  fill(inj_knee, .direction = "down") %>% select(-date_first) %>% rename(inj_knee_filled = inj_knee)

d_all = d_all %>% left_join(d_ostrc_dates_valid, by = c("id_player", "date"))

# do the same for the other injuries, shoulder and low back pain
d_all = d_all %>% mutate(inj_other = ifelse(inj_lowback == 1 |  inj_shoulder == 1, 1, 0))

d_ostrc_dates_other = d_all %>% select(date, id_player, inj_other) %>% filter(!is.na(inj_other))
d_ostrc_dates_other = d_ostrc_dates_other %>% mutate(date_first = date-6)

nested_list = d_ostrc_dates_other %>% group_by(id_player) %>% nest()
nested_list$data = nested_list$data %>% 
  map(., ~map2(.x = .$date_first, .y = .$date, .f = ~seq(ymd(.x), ymd(.y), by = "1 day")))
nested_list$data = nested_list$data %>% map(., ~do.call("c", .))
d_ostrc_dates_valid_other = unnest(nested_list, cols = data) %>% ungroup() %>% rename(date = data)

d_ostrc_dates_valid_other = d_ostrc_dates_valid_other %>% 
  left_join(d_ostrc_dates_other, by = c("id_player", "date")) %>% 
  fill(inj_other, .direction = "down") %>% select(-date_first) %>% rename(inj_other_filled = inj_other)

d_all = d_all %>% left_join(d_ostrc_dates_valid_other, by = c("id_player", "date"))

# add preseason variable
d_all = d_all %>% mutate(preseason = ifelse(season_phase == "Preseason", 1, 0))

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
                 preseason, session_type,
                 age, position,
                 weight,
                 height,
                 jump_height_max,
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
  mutate(jumps_n = sum(n_jump),
         jumps_n = ifelse(session_type == "no volleyball", 0, jumps_n),
         jump_height_sum = sum(jump_height, na.rm = TRUE),
         jump_height_sum = ifelse(session_type == "no volleyball", 0, jump_height_sum)) %>% ungroup()

d_unimputed_daily = d_unimputed %>% 
  distinct(date, id_player, id_team, id_team_player, id_season, session_type, .keep_all = TRUE)

d_unimputed_daily = d_unimputed_daily %>% 
  select(all_of(key_cols), session_type, game_type, jumps_n, 
                             jump_height_sum,
                             jump_height_max_percent,
                             height_ke_modified, load_index_KE, height_KE_updated)

d_daily = d_daily %>% mutate(id_player = as.character(id_player))

# test - are session types the same?
# they should be!
d_unimputed_daily_sessiontypes = d_unimputed_daily %>% 
                                 select(date, id_player, session_type_raw = session_type) 
d_daily_sessiontypes = d_daily %>% select(date, id_player, session_type_daily = session_type)
d_sessiontypes = d_daily_sessiontypes %>% 
  left_join(d_unimputed_daily_sessiontypes, by = c("date", "id_player")) %>% filter(!is.na(session_type_raw))

testthat::expect_equal(d_sessiontypes$session_type_daily, d_sessiontypes$session_type_raw)

# fixme! 8000 jumps ish that don't have player ID
# fixme! session type is not the same between daily data and the raw data
d_unimputed_daily = d_unimputed_daily %>% rename(session_type_raw = session_type)
# join daily unimputed with the daily data
d_daily_jumps = d_daily %>% left_join(d_unimputed_daily, by = key_cols)

d_daily_jumps = d_daily_jumps %>% 
  mutate(jumps_n = ifelse(session_type == "no volleyball", 0, jumps_n)) 

d_daily_jumps %>% 
  summarise(n_missing_daily = sum(is.na(jumps_n)), 
                                  denom = n(), 
                                  prop = n_missing_daily/denom, 
                                  perc = 100*prop)

# write_excel_csv(d_daily_jumps, 
#                 paste0(data_folder, "d_jump_daily.csv"), 
#                 delim = ";", na = "")

# checking missing dates
dates_matchpractice = d_jump_all %>% filter(session_type == "match" | session_type == "practice" | session_type == "friendly") %>% distinct(date)
dates_matchpractice_daily = d_daily %>% filter(session_type == "match" | session_type == "practice" | session_type == "friendly") %>% distinct(date)
setdiff(dates_matchpractice, dates_matchpractice_daily)

dates = d_jump_all  %>% distinct(date)
dates_daily = d_daily %>% distinct(date)
diffdates = setdiff(dates_daily, dates)

#-------------------------------multiple imputation

# Performing multiple imputation
# note that the data are likely MAR
# with more missing in the earlier years than in later years
# need the correct variable types
d_pre_impute = d_pre_impute %>% mutate_at(vars(starts_with("Knee"), starts_with("Shoulder"), 
                                 starts_with("LowBack"), starts_with("inj")), ~as.character(.))

# Year is not to be included in the multiple imputation
# (one of the pitfalls of multiple imputation is to inlcude MAR-causing variables)
# specify in imputation model



#-------------------------------------calculate weekly jump load
# function to calculate sums across a user-specified window size
slide_sum = function(x, window_size){
  l = slide(x, ~sum(.), .before = 6, step = window_size, .complete = FALSE)
  vector = unlist(l)
  v_calc = vector[seq(1, length(vector), window_size)]
  v_calc
}

# make window object to designated length (7 days)
window_7 = 7

# First step is to test that our automated function 
# for calculating weekly sums (slide_sum) calculates correctly. 
example_player = d_daily_jumps %>% filter(id_player == 1) %>% select(date, jumps_n, id_player, inj_knee)

# manual calculation of first 3 weekly sums
v_correct = c(
  example_player %>% slice(1:7) %>% summarize(the_sum = sum(jumps_n)) %>% pull(the_sum),
  example_player %>% slice(8:14) %>% summarize(the_sum = sum(jumps_n)) %>% pull(the_sum),
  example_player %>% slice(15:21) %>% summarize(the_sum = sum(jumps_n)) %>% pull(the_sum)
)

# function above used to calculate weekly sums
test_loads = (example_player %>% pull(jumps_n))[1:(window_7*3)]
v_calc = slide_sum(test_loads, window_7)

# test that the output from the function equals the manual calculation
testthat::expect_equal(v_correct, v_calc)

# Next step is to perform the calculation on the data
# We nest on each player in the data so that weekly sum-calculation
# does not slide over different players
window = 1
nested_list = d_daily_jumps %>% group_by(id_player) %>% nest()
nested_list$data = nested_list$data %>% map(., ~slide_sum(.$jumps_n, window))
d_weekly_load = unnest(nested_list, cols = c(data)) %>% 
  ungroup() %>% mutate(index = 1:n()) %>% 
  rename(jumps_n_weekly = data)

d_daily_jumps_joined = d_daily_jumps %>% mutate(index = 1:n()) %>% left_join(d_weekly_load, by = c("id_player", "index"))

# repeat for jump height
nested_list = d_daily_jumps %>% group_by(id_player) %>% nest()
nested_list$data = nested_list$data %>% map(., ~slide_sum(.$jump_height_sum, window))
d_weekly_load = unnest(nested_list, cols = c(data)) %>% 
  ungroup() %>% mutate(index = 1:n()) %>% 
  rename(jumps_height_weekly = data)
d_daily_jumps_joined2 = d_daily_jumps_joined %>% mutate(index = 1:n()) %>% left_join(d_weekly_load, by = c("id_player", "index"))

# remove the first 6 rows per person for the weekly jump heights and jump frequencies
d_pre_impute = d_daily_jumps_joined2 %>% 
  group_by(id_player) %>% 
  mutate(index = 1:n(),
         jumps_n_weekly = ifelse(index %in% 1:6, NA, jumps_n_weekly),
         jumps_height_weekly = ifelse(index %in% 1:6, NA, jumps_height_weekly)
  ) %>% ungroup()

