library(tidyverse) # data wrangling
library(dlnm) # distributed lag non-linear models
library(survival)
library(lubridate) # to manipulate dates

# so we don't have to deal with scientific notations
# and strings aren't automatically read as factors:
options(scipen = 30, 
        stringsAsFactors = FALSE)

data_folder = "D:\\phd\\jump load\\data\\"
d_jumpload = readRDS(paste0(data_folder, "d_jumpload_multimputed.rds"))

# define key columns
key_cols = c("date", "id_player", "id_team", "id_team_player", "id_season")
conf_cols = c("age", "jump_height_max", "position", 
              "match", "t_prevmatch", "jumps_n_weekly", "preseason", "jump_height_sum")


# define the min and max lag
lag_min = 0
lag_max = 27

# select columns that may be useful in analyses
d_analysis = d_jumpload %>% 
  select(all_of(key_cols), 
         jumps_n, 
         jumps_n_weekly,
         jump_height_sum,
         jumps_height_weekly,
         jump_height_max,
         load_index_KE,
         starts_with("knee"),
         starts_with("inj"), 
         year, month_day, season, 
         preseason,
         age, weight, height, position,
         session_type, match = Match, 
         t_prevmatch, game_type, match_sets_n = MatchSets,
         d_imp = .imp)

d_analysis = d_analysis %>% mutate_at(vars(starts_with("inj"), starts_with("knee")), ~as.numeric(.))

# We will find the change state of each OSTRC questionnaire.

# define states
# we can either take this problem through a 
# regular vs. substantial injury defition
# or going from one level (no matter what level)
# to something worse
# we will consider the "worsening" problem
# we've included improvement, but for statistical power,
# maybe it should be the same as symptomatic/worse

# find positions of change 
d_kneelevels = d_analysis %>% group_by(d_imp, id_player) %>% 
  mutate(knee_total_filled_lag = lag(knee_total_filled),
         knee_total_diff = knee_total_filled - knee_total_filled_lag,
         change = case_when(Knee_Total >= 1 & knee_total_filled_lag == 0 ~ 2,
                            Knee_Total >= 1 & is.na(knee_total_filled_lag) ~ 2,
                            knee_total_diff < 0 & Knee_Total != 0 ~ 2,
                            knee_total_diff > 0 & Knee_Total != 0 ~ 3,
                            Knee_Total == 0 ~ 1)
  ) %>% ungroup()

# these too, have to be filled.
# We start by filling the values 6 days down.
# find which days the OSTRC-questionnaire actually pertains to
# since each answer is for the previous 6 days including the current day
d_ostrc_dates = d_kneelevels %>% select(d_imp, all_of(key_cols), change) %>% filter(!is.na(change))
d_ostrc_dates = d_ostrc_dates %>% mutate(date_last = date+6)

nested_list = d_ostrc_dates %>% group_by(d_imp, id_player, id_team, id_team_player, id_season) %>% nest()
nested_list$data = nested_list$data %>% 
  map(., ~map2(.x = .$date, .y = .$date_last, .f = ~seq(ymd(.x), ymd(.y), by = "1 day")))
nested_list$data = nested_list$data %>% map(., ~do.call("c", .))
d_ostrc_dates_valid = unnest(nested_list, cols = data) %>% ungroup() %>% rename(date = data)

# remove duplicated dates
# as some OSTRC intervals overlapped
d_ostrc_dates_valid = d_ostrc_dates_valid %>% 
  distinct(d_imp, id_player, id_team, id_team_player, id_season, date)

d_ostrc_dates_valid = d_ostrc_dates_valid %>% 
  left_join(d_ostrc_dates, by = c("d_imp", "id_player", "id_team", "id_team_player", "id_season", "date")) %>% 
  fill(change, .direction = "down") %>% select(-date_last) %>% rename(knee_state = change)

d_kneelevels = d_kneelevels %>% left_join(d_ostrc_dates_valid, 
                                          by = c("d_imp", "id_player", "id_team", "id_team_player", "id_season", "date"))

d_kneelevels = d_kneelevels %>% 
  group_by(d_imp) %>% 
  mutate(knee_state = case_when(is.na(knee_state) & knee_total_filled == 0 ~ 1,
                                is.na(knee_state) & knee_total_filled > 0 ~ 2,
                                TRUE ~ knee_state)
  ) %>% ungroup()

# select variables wer are going to use.
d_selected = d_kneelevels  %>% 
  select(d_imp, id_player, date, knee_total_filled, knee_state, jumps_n, all_of(conf_cols))

#----------------------------------------Structuring into counting process form

# The msm::msm2Surv could have been used on our longitudinl data to 
# prep it into counting process form. However, because we have a time-varying covariate
# that changes every day, we cannot do so.
# mstate::msprep is also an alternative, if the data is in wide format. However, the
# mstate package assumes survival data with an intiial state and an absorbing state,
# and cannot handle circular transition matrices
# we will therefore structure our data MANUALLY. 
# (yes, I died a little inside)
statenames = c("asymptomatic", "symptomatic", "worse")
l_transitions = list(c(2), 
                     c(1, 3), 
                     c(1, 2))
transmat = mstate::transMat(l_transitions, statenames)

# state 1 is asymptomatic
# state 2 is symptomatic
# state 3 is worse
# from 1 to 2 is transition 1
# from 2 to 1 is transition 2
# from 2 to 3 is transition 3
# from 3 to 1 is transition 4
# from 3 to 2 is transition 5

# find moments of transitions 
# find the number of days until the transition
d_selected = d_selected %>% 
  mutate(diff_prev = knee_state-lag(knee_state),
         change_missing = ifelse(!is.na(knee_state) & is.na(diff_prev), 1, 0),
         change = ifelse(diff_prev != 0 | change_missing == 1, 1, 0),
         change = ifelse(is.na(change), 0, change)) %>% 
  group_by(d_imp, id_player) %>% 
  mutate(day = 1:n(),
         Fup = ifelse(change == 1, day, NA)) %>% fill(Fup, .direction = "up") %>% ungroup()

# find start and stop times
d_surv = d_selected %>% group_by(d_imp, id_player) %>% 
  rename(stop = day) %>% 
  mutate(start = lag(stop),
         start = ifelse(is.na(start), 0, start)) %>% ungroup()

# select again for easier analysis
d_surv = d_surv %>% 
  select(d_imp, id_player, date, start, stop, knee_state, Fup, jumps_n) 

# fixme! better missing solution.
d_test1 = d_surv %>% group_by(d_imp, id_player) %>% fill(knee_state, .direction = "downup") %>% ungroup()
d_test2 = d_test1 %>% mutate(from = lag(knee_state))

# find transitions and put them into long format
# not that since state 1 can only go to 2,
# state 2 can only go to 1 and 3 etc.
# we have to do this for each "from" state separately
d_from1 = d_test2  %>% 
  filter(from == 1) %>% 
  group_by(d_imp, id_player) %>% 
                  mutate(to = 2,
                         trans = as.character(1),
                         status = ifelse(knee_state == 2, 1, 0),
                         status = ifelse(is.na(status), 0, status))

d_from2 = d_test2  %>% 
  filter(from == 2) %>% 
  group_by(d_imp, id_player) %>% 
  mutate(trans2 = ifelse(knee_state == 1, 1, 0),
         trans2 = ifelse(is.na(trans2), 0, trans2),
         trans3 = ifelse(knee_state == 3, 1, 0),
         trans3 = ifelse(is.na(trans3), 0, trans3)) %>% ungroup() %>% 
 pivot_longer(cols = c("trans2", "trans3"), names_to = "trans", values_to = "status") %>% 
  mutate(trans = str_replace_all(trans, "trans", ""),
         to = ifelse(trans == 2, 1, 3))


d_from3 = d_test2  %>% 
  filter(from == 3) %>% 
  group_by(d_imp, id_player) %>% 
  mutate(trans4 = ifelse(knee_state == 1, 1, 0),
         trans4 = ifelse(is.na(trans4), 0, trans4),
         trans5 = ifelse(knee_state == 2, 1, 0),
         trans5 = ifelse(is.na(trans5), 0, trans5)) %>% ungroup() %>% 
  pivot_longer(cols = c("trans4", "trans5"), names_to = "trans", values_to = "status") %>% 
  mutate(trans = str_replace_all(trans, "trans", ""),
         to = ifelse(trans == 4, 1, 2))

# bind data
# congratulations, they are now ready for analysis
d_test3 = bind_rows(d_from1, d_from2, d_from3) %>% arrange(d_imp, id_player, date)


library(flexsurv)
flexsurvreg(Surv(start, stop, status) ~ 1, subset=(trans==1),
            data = d_test3 %>% filter(d_imp == 1), dist = "exp")

flexsurvreg(Surv(start, stop, status) ~ jumps_n, subset=(trans==2),
            data = d_test3 %>% filter(d_imp == 1), dist = "exp")

# for all states at a time
n_trans = max(transmat, na.rm = TRUE)
trans_vec = 1:n_trans
trans_vec %>% map(.x = ., ~flexsurvreg(Surv(start, stop, status) ~ jumps_n,
                                       data = subset(d_test3, trans == .x),
                                       dist = "exp"))
