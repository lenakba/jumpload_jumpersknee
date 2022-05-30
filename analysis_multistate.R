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
d_analysis = d_analysis %>% mutate(knee_total_filled = ifelse(is.na(knee_total_filled), 0, knee_total_filled),
                                   inj_knee_filled = ifelse(is.na(inj_knee_filled), 0, inj_knee_filled))

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
                            knee_total_diff < 0 & Knee_Total != 0 ~ 4,
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




library(msm)
# add number of days per individual
d_kneelevels = d_kneelevels %>% arrange(d_imp, id_player, date) %>%
  group_by(d_imp, id_player) %>%
  mutate(day = 0:(n()-1)) %>% ungroup() 


d_selected = d_kneelevels  %>% 
  select(d_imp, id_player, date, knee_state, day, all_of(conf_cols))

# number of transitions from one state to another
statetable.msm(knee_state, id_player, data = d_selected %>% filter(d_imp == 1))




twoway4.q <- rbind(c(0, 0.25, 0, 0.25), c(0.166, 0, 0.166, 0.166),
                     + c(0, 0.25, 0, 0.25), c(0, 0, 0, 0))
rownames(twoway4.q) = colnames(twoway4.q) = c("Well", "Mild",
                                                   "Severe", "Death")



statenames = c("asymptomatic", "symptomatic", "worsening", "improvement")
l_transitions = list(c(2), 
                     c(1, 3, 4), 
                     c(1, 4),
                     c(1, 3))
transmat = mstate::transMat(l_transitions, statenames) %>% replace_na(0)


d_selected %>% count(knee_state)

cav.msm = msm(knee_state ~ day, subject = id_player, data = d_selected %>% filter(d_imp == 1),
               qmatrix = transmat, control=list(fnscale=5000,maxit=500))
cav.msm


# put states into their own variables
d_kneelevels = d_kneelevels %>% mutate(asymptomatic = ifelse(knee_state == 0, 1, 0),
                                     symptomatic = ifelse(knee_state == 1, 1, 0),
                                     worse = ifelse(knee_state == 2, 1, 0),
                                     better = ifelse(knee_state == 3, 1, 0))






# add number of days of follow up per event
d_kneelevels = d_kneelevels %>% arrange(d_imp, id_player, date) %>%
  group_by(d_imp, id_player, id_event) %>%
  mutate(Fup = n(), day = 1:n()) %>% ungroup()

msprep(data = d_kneelevels, trans = tmat, 
       time = c(NA, "rec", "ae","recae", "rel", "srv"), 
       status = c(NA, "rec.s", "ae.s", "recae.s", "rel.s", "srv.s"), 
       keep = c("match", "proph", "year", "agecl"))

msprep(data = d_kneelevels, trans = transmat, 
       time = c(NA, rep("day", 4)), 
       status = c(NA, "asymptomatic", "symptomatic", "worse", "better"), 
       keep = c(key_cols, "jumps_n"))


n_trans = max(transmat, na.rm = TRUE)
trans_vec = 1:n_trans

trans_vec %>% map(.x = ., ~flexsurvreg(Surv(enter, exit, event) ~ load,
                                       data = subset(d_u19_cpform, event == .x),
                                       dist = "weibull"))

#--------------------------------Try again using msprep instead of SurvSplit


library(mstate)
# simple overuse injury transition matrix
transitions_u19 = list(c(2, 3), 
                       c(3, 4), 
                       c(4),
                       c())

statenames_u19 = c("healthy", "overuse", "acute", "asymptomatic")
transmat_u19 = transMat(transitions_u19, statenames_u19)


# put states into their own variables
d_u19_states_temp = d_u19 %>% mutate(overuse = ifelse(injury == 1 & overuse == 2, 1, 0),
                                     acute = ifelse(injury == 1 & substantial == 0 & overuse == 1, 1, 0),
                                     acute_subst = ifelse(injury == 1 & substantial == 1 & overuse == 1, 1, 0)) %>% 
  select(p_id, date_training, load, overuse, acute, acute_subst, injury)
