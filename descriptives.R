library(tidyverse)
library(readxl)

data_folder = "D:\\phd\\jump load\\data\\"
col_specs = cols(
  date = col_date(format = ""),
  datetime = col_datetime(format = "%Y-%m-%d %H:%M:%OS"),
  time_seconds = col_double(),
  jump_height = col_double(),
  session_type = col_character(),
  jump_height_max = col_double(),
  game_type = col_character(),
  jump_height_max_percent = col_double(),
  position = col_character(),
  imputed = col_character(),
  jump_height_max_0.1 = col_double(),
  jump_height_max_percent_0.1 = col_double(),
  id_team_player = col_character(),
  id_player = col_character(),
  id_team = col_character(),
  id_season = col_character(),
  weight = col_double(),
  height_ke_modified = col_double(),
  load_index_KE = col_double(),
  height_KE_updated = col_character(),
  n_jump = col_double(),
  jump_daily_n = col_double(), 
  Knee_1 = col_character(), 
  Knee_2 = col_character(), 
  Knee_3 = col_character(), 
  Knee_4 = col_character(), 
  Knee_Total = col_character(),
  Shoulder_1 = col_character(), 
  Shoulder_2 = col_character(), 
  Shoulder_3 = col_character(), 
  Shoulder_4 = col_character(), 
  Shoulder_Total = col_character(),
  LowBack_1 = col_character(), 
  LowBack_2 = col_character(),
  LowBack_3 = col_character(), 
  LowBack_4 = col_character(), 
  LowBack_Total = col_character(), 
  inj_knee = col_character(),
  inj_knee_subst = col_character(),
  inj_shoulder = col_character(), 
  inj_shoulder_subst = col_character(), 
  inj_lowback = col_character(),
  inj_lowback_subst = col_character(),
  season = col_character(),                     
  season_phase = col_character(),              
  age = col_double(),                        
  MatchParticipation = col_character(),          
  Match_number = col_character(),                
  Match_Opponent = col_character(),           
  Match_Type  = col_character(),                
  Match_Result = col_character(),               
  MatchSets = col_double(),             
  Match_dateofnext = col_character(),          
  MatchRelatedDay  = col_character(),           
  Match = col_character()
)

#d_full = read_delim(paste0(data_folder,"d_jump.csv"), delim = ";", na = "", col_types = col_specs)
#key_cols = c("date", "id_player", "id_team", "id_team_player", "id_season")


col_specs_daily = cols(
  date = col_date(format = ""),
  session_type = col_character(),
  jump_height_max = col_double(),
  game_type = col_character(),
  jump_height_max_percent = col_double(),
  position = col_character(),
  id_team_player = col_character(),
  id_player = col_character(),
  id_team = col_character(),
  id_season = col_character(),
  weight = col_double(),
  height_ke_modified = col_double(),
  load_index_KE = col_double(),
  height_KE_updated = col_character(),
  jump_height_sum = col_double(),
  jump_daily_n = col_double(), 
  Knee_1 = col_character(), 
  Knee_2 = col_character(), 
  Knee_3 = col_character(), 
  Knee_4 = col_character(), 
  Knee_Total = col_character(),
  Shoulder_1 = col_character(), 
  Shoulder_2 = col_character(), 
  Shoulder_3 = col_character(), 
  Shoulder_4 = col_character(), 
  Shoulder_Total = col_character(),
  LowBack_1 = col_character(), 
  LowBack_2 = col_character(),
  LowBack_3 = col_character(), 
  LowBack_4 = col_character(), 
  LowBack_Total = col_character(), 
  inj_knee = col_character(),
  inj_knee_subst = col_character(),
  inj_shoulder = col_character(), 
  inj_shoulder_subst = col_character(), 
  inj_lowback = col_character(),
  inj_lowback_subst = col_character(),
  season = col_character(),                     
  season_phase = col_character(),              
  age = col_double(),                        
  MatchParticipation = col_character(),          
  Match_number = col_character(),                
  Match_Opponent = col_character(),           
  Match_Type  = col_character(),                
  Match_Result = col_character(),               
  MatchSets = col_double(),             
  Match_dateofnext = col_character(),          
  MatchRelatedDay  = col_character(),           
  Match = col_character()
)
d_daily = read_delim(paste0(data_folder,"d_jump_daily.csv"), delim = ";", na = "", col_types = col_specs_daily)

#------------------------------------------ injury descriptives

# number of knee complaint cases
d_full %>% 
  arrange(desc(inj_knee)) %>% 
  distinct(date, id_player, .keep_all = TRUE) %>% summarise(sum(inj_knee == 1, na.rm = TRUE))

# how many players have symptoms?
d_knee_perplayer = d_all %>% count(PlayerID, inj_knee) 

d_knee_perplayer %>% group_by(PlayerID) %>% 
  summarise(injured = sum(inj_knee == 1, na.rm = TRUE)) %>% 
  count(injured)

# number of players who had complaints for more than 1 week
d_knee_perplayer %>% filter(inj_knee == 1) %>% summarise(sum(n>=2))

n_weeks_perplayer = d_knee_perplayer %>% filter(!is.na(inj_knee)) %>% group_by(PlayerID) %>% summarise(denom = sum(n))

mean_weeks_complaints = d_knee_perplayer %>% 
  filter(inj_knee == 1) %>% 
  left_join(n_weeks_perplayer, by = "PlayerID") %>% 
  mutate(prop = n/denom, mean_prop = mean(prop))

# how many our missing the OSTRC questionnaire?
d_ostrc_completed = d_all %>% select(starts_with("completed")) %>% filter(!is.na(completed_team_ostrc))
nrow(d_ostrc_completed)

# missing on completed variable means that the response was missing
d_ostrc_completed %>% filter(is.na(completed_today_Knee_OSTRC))

# only questions 2 and 3 pertain to substantial injuries, 
# and only if they answered reply number 3 or 4
# our data is structured for severity scores, 
# and have the options 0, 8, 17, 25, equaling response number 1, 2, 3, or 4

# number of days with jumper's knee
d_all %>% summarise(sum(inj_knee == 1, na.rm = TRUE))

#------------------------------------------------jump load descriptives

d_daily = d_daily %>% mutate(preseason = ifelse(season_phase == "Preseason", 1, 0))
d_daily_nomissing = d_daily %>% filter(!is.na(jump_daily_n), session_type != "no volleyball")

# function for calculating mean, median etc. of anything
calc_descs = function(d, var){
  var = enquo(var)
  
  d %>% summarise(mean = mean(!!var, na.rm = TRUE), 
                  sd = sd(!!var, na.rm = TRUE),
                  median = median(!!var, na.rm = TRUE), 
                  iqr = IQR(!!var, na.rm = TRUE),
                  max = max(!!var, na.rm = TRUE))
}

calc_descs(d_daily_nomissing, jump_daily_n) 
d_daily_nomissing %>% group_by(season_phase) %>% calc_descs(., jump_daily_n)
d_daily_nomissing %>% group_by(preseason) %>% calc_descs(., jump_daily_n)
d_daily_nomissing %>% group_by(Match) %>% calc_descs(., jump_daily_n)
d_daily_nomissing %>% filter(season_phase != "Preseason") %>% group_by(Match) %>% calc_descs(., jump_daily_n)

#------------------------------------------------player characteristics

d_player = d_daily %>% distinct(id_player, .keep_all = TRUE)

# 49, 36, 51, 48, 20 missing weight?
d_player %>% select(id_player, weight) %>% tail()
# 20 missing height?
d_player %>% select(id_player, height) %>% tail()

calc_descs(d_player, age)
calc_descs(d_player, weight)
calc_descs(d_player, height)
calc_descs(d_player, jump_height_max)

#----counts
d_player %>% select(position)
d_player %>% count(position)

#---------------------------------------------------trends--------------------------------------


# trends. Does there seem to be a season effect?
# only looking at days that players play volleyball (match or training)
# see if there is any pattern in the injuries
d_mean_day = d_all %>% filter(SessionType != "no volleyball") %>% 
  group_by(Date, year, season, month_day) %>% summarise(mean_jumps = mean(jumps_n), 
                                                        mean_height = mean(jump_height_sum, na.rm = TRUE),
                                                        sum_injuries = sum(inj_knee, na.rm = TRUE)) %>% ungroup()

ggplot(d_mean_day, aes(x = month_day, y = mean_jumps, group = 1)) +
  facet_wrap(~year) +
  geom_line(size = 1) +
  geom_point(aes(x = month_day, y = sum_injuries))

ggplot(d_mean_day, aes(x = Date, y = mean_height)) +
  geom_line()

# do matches have higher/more frequent than other training?
d_mean_day_match = d_all %>% 
  filter(SessionType != "no volleyball") %>% group_by(Date, Match) %>% 
  summarise(mean_jumps = mean(jumps_n), 
            mean_height = mean(jump_height_sum, na.rm = TRUE))


ggplot(d_mean_day_match, aes(x = Date, y = mean_jumps, group = Match)) +
  geom_line()


d_mean_match = d_all %>% 
  filter(SessionType != "no volleyball") %>% group_by(Match) %>% 
  summarise(mean_jumps = mean(jumps_n), 
            mean_height = mean(jump_height_sum, na.rm = TRUE)) %>% ungroup()

# fewer and lower jumping during match? 
ggplot(d_mean_match, aes(y = Match, x = mean_jumps)) +
  ggstance::geom_barh(stat = "identity", color = "black")

ggplot(d_mean_match, aes(y = Match, x = mean_height)) +
  ggstance::geom_barh(stat = "identity", color = "black")

# what happens if we remove preseason?

d_mean_match = d_all %>% 
  filter(SessionType != "no volleyball", Season.Phase != "Preseason") %>% group_by(Match) %>% 
  summarise(mean_jumps = mean(jumps_n), 
            mean_height = mean(jump_height_sum, na.rm = TRUE)) %>% ungroup()

# fewer and lower jumping during match?
# effect not as extreme
ggplot(d_mean_match, aes(y = Match, x = mean_jumps)) +
  ggstance::geom_barh(stat = "identity", color = "black")

ggplot(d_mean_match, aes(y = Match, x = mean_height)) +
  ggstance::geom_barh(stat = "identity", color = "black")