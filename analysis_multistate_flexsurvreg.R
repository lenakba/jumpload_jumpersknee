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

# fixme! find better solution for handling missing responses
# d_analysis = d_analysis %>% mutate(knee_total_filled = ifelse(is.na(knee_total_filled), -1, knee_total_filled),
#                                    inj_knee_filled = ifelse(is.na(inj_knee_filled), -1, inj_knee_filled))

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
  select(d_imp, id_player, date, knee_state, jumps_n, all_of(conf_cols))

# we will find each event interval and give it an ID.
# this so we can find the follow up time per event
d_selected = d_selected %>% 
  mutate(diff_prev = knee_state-lag(knee_state),
         change_missing = ifelse(!is.na(knee_state) & is.na(diff_prev), 1, 0),
         change = ifelse(diff_prev != 0 | change_missing == 1, 1, 0),
         change = ifelse(is.na(change), 0, change)) %>% 
  group_by(d_imp, id_player) %>% 
  mutate(day = 0:(n()-1),
         Fup = ifelse(change == 1, day, NA)) %>% fill(Fup, .direction = "up") %>% ungroup()
















library(msm)
# add number of days per individual
d_kneelevels = d_kneelevels %>% arrange(d_imp, id_player, date) %>%
  group_by(d_imp, id_player) %>%
  mutate(day = 0:(n()-1)) %>% ungroup() 


d_selected = d_kneelevels  %>% 
  select(d_imp, id_player, date, knee_state, day, jumps_n, all_of(conf_cols))

# number of transitions from one state to another
statetable.msm(knee_state, id_player, data = d_selected %>% filter(d_imp == 1))

l_selected = (d_selected %>% group_by(d_imp) %>% nest())$data

# matrix of exposure histories
l_q_mat = l_selected %>% map(., ~tsModel::Lag(.$jumps_n, lag_min:lag_max))

# obtain the crossbasis
# since data har heavily skewed, we will subjectively specify location of knots
l_cb_dlnm =  l_q_mat %>% map(~crossbasis(., lag=c(lag_min, lag_max),
                                         argvar = list(fun="ns", knots = c(50, 100, 150)),
                                         arglag = list(fun="ns", knots = 3)))


# intitially we had 4 states (one was "improvement")
# however, we struggled with convergence. 
# we therefore reduced to studying only 3 states. 
statenames = c("asymptomatic", "symptomatic", "worsening")
l_transitions = list(c(2), 
                     c(1, 3), 
                     c(1, 2))
transmat = mstate::transMat(l_transitions, statenames) %>% replace_na(0)
transmat_init = crudeinits.msm(knee_state ~ day, id_player, data=d_selected %>% filter(d_imp == 1), qmatrix=transmat)



# the msm package did not work, because it could not handle the DLNM matrix
# and we can't reshape to the counting process form
# we will try the flexsurvreg package instead
cav.msm = msm(knee_state ~ day, subject = id_player, data = d_selected %>% filter(d_imp == 1),
              qmatrix = transmat_init, control=list(fnscale=5000,maxit=500), method = "BFGS",
              covariates = ~ l_cb_dlnm[[1]] + position + age + 
                jump_height_max + match + t_prevmatch)
cav.msm