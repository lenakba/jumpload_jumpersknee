library(tidyverse)
library(readxl)

data_folder = "O:\\Prosjekter\\Bache-Mathiesen-Biostatistikk\\Data\\volleyball\\"
key_cols = c("date", "id_player", "id_team", "id_team_player", "id_season")

# read datasets
d_full = read_delim(paste0(data_folder,"d_volleyball.csv"), delim = ";", na = "")
d_daily = read_delim(paste0(data_folder,"d_jump_daily.csv"), delim = ";", na = "")

#------------------------------------------ injury descriptives

# number of knee complaint cases
d_full %>% 
  arrange(desc(inj_knee)) %>% 
  distinct(date, id_player, .keep_all = TRUE) %>% summarise(sum(inj_knee == 1, na.rm = TRUE),
                                                            sum(inj_knee == 0, na.rm = TRUE))

# how many players have symptoms?
d_knee_perplayer = d_full %>% count(id_player, inj_knee) 

d_knee_perplayer %>% group_by(id_player) %>% 
  summarise(injured = sum(inj_knee == 1, na.rm = TRUE)) %>% 
  count(injured)

# number of players who had complaints for more than 1 week
d_knee_perplayer %>% filter(inj_knee == 1) %>% summarise(sum(n>=2))

n_weeks_perplayer = d_knee_perplayer %>% filter(!is.na(inj_knee)) %>% group_by(PlayerID) %>% summarise(denom = sum(n))

mean_weeks_complaints = d_knee_perplayer %>% 
  filter(inj_knee == 1) %>% 
  left_join(n_weeks_perplayer, by = "PlayerID") %>% 
  mutate(prop = n/denom, mean_prop = mean(prop))

# how many substantial injuries
inj_knee_subst_filled

d_full %>% select(inj_knee_subst)

d_full %>% 
  arrange(desc(inj_knee_subst)) %>% 
  distinct(date, id_player, .keep_all = TRUE) %>% 
  summarise(sum(inj_knee_subst == 1, na.rm = TRUE),
            sum(inj_knee_subst == 0, na.rm = TRUE))


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
d_daily_nomissing = d_daily %>% filter(!is.na(jumps_n), session_type != "no volleyball")

d_daily %>% View()


# function for calculating mean, median etc. of anything
calc_descs = function(d, var){
  var = enquo(var)
  
  d %>% summarise(mean = mean(!!var, na.rm = TRUE), 
                  sd = sd(!!var, na.rm = TRUE),
                  median = median(!!var, na.rm = TRUE), 
                  iqr = IQR(!!var, na.rm = TRUE),
                  max = max(!!var, na.rm = TRUE))
}

calc_descs(d_daily_nomissing, jump_height_max) 
calc_descs(d_daily_nomissing, jumps_n) 
calc_descs(d_daily_nomissing, jump_height_sum) 
calc_descs(d_daily_nomissing, jump_height_perc_sum)

d_daily_nomissing %>% group_by(season_phase) %>% calc_descs(., jumps_n)
d_daily_nomissing %>% group_by(preseason) %>% calc_descs(., jumps_n)
d_daily_nomissing %>% group_by(Match) %>% calc_descs(., jumps_n)
d_daily_nomissing %>% filter(season_phase != "Preseason") %>% group_by(Match) %>% calc_descs(., jumps_n)

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
