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
d_all = d_all %>%mutate(Match = as.character(ifelse(SessionType == "match", 1, 0)))
d_all = d_all %>%mutate(SessionType = ifelse(is.na(SessionType), "no volleyball", SessionType))

# calculate sum of jump heights
d_all = d_all %>%mutate(jump_height_sum = jump_height_avg_cm*jumps_n)

# add complaint yes/no variables
d_all = d_all %>% mutate(inj_knee = ifelse(Knee_1 >= 1| Knee_2 >= 1 | Knee_3 >= 1 | Knee_4 >= 1, 1, 0),
                         inj_knee_subst = ifelse(Knee_2 >= 17 | Knee_3 >= 17, 1, 0),
                         inj_shoulder = ifelse(Shoulder_1 >= 1| Shoulder_2 >= 1 | Shoulder_3 >= 1 | Shoulder_4 >= 1, 1, 0),
                         inj_shoulder_subst = ifelse(Shoulder_2 >= 17 | Shoulder_3 >= 17, 1, 0),
                         inj_lowback = ifelse(LowBack_1 >= 1| LowBack_2 >= 1 | LowBack_3 >= 1 | LowBack_4 >= 1, 1, 0),
                         inj_lowback_subst = ifelse(LowBack_2 >= 17 | LowBack_3 >= 17, 1, 0))

# number of knee complaint cases
d_all %>% summarise(sum(inj_knee == 1, na.rm = TRUE))

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

# write csv to read in other scripts
# write .csv
# write_delim is preferable, but write_excel_csv is required for excel to understand
# that the file encoding is UTF-8
# write_excel_csv(d_all, paste0(data_folder, "d_volleyball.csv"), delim = ";", na = "")


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

#---------------------------------------------------exposure data

key_cols = c("date", "id_player", "id_team", "id_team_player", "id_season")
d_all = d_all %>% rename(date = Date, 
                         id_team = Team, 
                         season_phase = `Season Phase`,
                         id_season = TeamSeason,  
                         id_team_player = Team_Player_ID,
                         id_player = PlayerID,
                         session_type = SessionType)

d_daily = d_all %>% select(key_cols, 
                 starts_with("Knee"), 
                 starts_with("Shoulder"), 
                 starts_with("LowBack"), 
                 starts_with("inj"), 
                 year, month_day, season, 
                 season_phase,
                 age,
                starts_with("Match"))

# column specs for jump data
jump_level_cols = cols(
  .default = col_double(),
  date = col_date(format = ""),
  datetime = col_datetime(format = "%Y-%m-%d %H:%M:%OS"),
  match_number = col_character(),
  session_type = col_character(),
  position = col_character(),
  game_type = col_character(),
  imputed = col_character(),
  id_team_player = col_character(),
  id_team = col_character(),
  id_season = col_character(),
  height_KE_updated = col_character()
)

d_jump_all = read_delim(paste0(data_folder,"data_per_jump.csv"), delim = ";", na = "", col_types = jump_level_cols)
d_jump_all = d_jump_all %>% mutate(session_type = ifelse(is.na(session_type), "no volleyball", session_type))

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

