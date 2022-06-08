library(tidyverse) # data wrangling
library(dlnm) # distributed lag non-linear models
library(survival)
library(lubridate) # to manipulate dates
library(mstate) # for fitting multistate models

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
# mstate package assumes survival data with an initial state and an absorbing state,
# and cannot handle circular transition matrices
# we will therefore structure our data MANUALLY. 
# (yes, I died a little inside)

# To analyze the transitions, we need to have have the event vs. censored status
# for each possible transition 
# for each observed timepoint
# for each individual
# for each imputed dataset
# since we have a time-varying covariate that changes evey day (every timepoint)
# we must have this for every day.
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
  select(d_imp, id_player, date, start, stop, knee_state, Fup, jumps_n, all_of(conf_cols)) 

# fixme! better missing solution.
d_filled = d_surv %>% group_by(d_imp, id_player) %>% fill(knee_state, .direction = "downup") %>% ungroup()
d_filled = d_filled %>% mutate(from = lag(knee_state))

# function to find event intervals and append them to a dataset
find_events = function(d, id){
  d1 = d %>% filter(id_player == id)
  
  # find intervals
  pos_symptoms = which(d1$status == 1)
  big_skips = lead(pos_symptoms)-pos_symptoms
  pos_from = which(big_skips>1)
  
  from_after_first = pos_symptoms[pos_from]+1
  to_after_first = pos_symptoms[pos_from+1]
  # append to the first extraction
  from_rows = c(1, from_after_first)
  to_rows = c(pos_symptoms[1], to_after_first)
  
  # if player ends without sympts, needs to be included
  if((d1 %>% nrow()) > last(pos_symptoms)){
    from = last(pos_symptoms) + 1
    end = d1 %>% nrow()
    from_rows = c(from_rows, from)
    to_rows = c(to_rows, end)
  }
  
  # slice number of times equal to number of intervals
  l1mann = replicate(length(from_rows), d1, simplify = FALSE)
  l_data1person = pmap(list(as.list(from_rows), as.list(to_rows), l1mann), 
                       function(x, y, z) slice(z, x:y))
  event_ids = 1:length(l_data1person)
  l_data1person = map2(l_data1person, event_ids, ~.x %>% mutate(id_event = .y))
  
  # collect list of intervals to dataset
  d_1person = l_data1person %>% bind_rows()
  d_1person
}

# function for adding event ids too all datasets in
# a multiply imputed dataset
add_event_id = function(d, status){
  # Find the Q matrix for just transition 1
  
  no_inj_players = d %>% group_by(id_player) %>% summarise(sum = sum(status)) %>% 
    ungroup %>% filter(sum == 0) %>% pull(id_player)
  
  # fetch those with injuries
  d_inj_only = d %>% filter(!id_player %in% no_inj_players)
  ids = d_inj_only %>% distinct(id_player) %>% pull()
  
  
  # running above function for each player
  # for each imputed dataset
  datasets = d_inj_only %>% distinct(d_imp) %>% pull() 
  d_maindata = data.frame()
  for(i in datasets){
    
    tempdata2 = d_inj_only %>% filter(d_imp == i)  
    d_1person = data.frame()
    for(j in ids){
      tempdata = find_events(tempdata2, j)
      d_1person = rbind(d_1person, tempdata)
      d_1person
    }
    d_maindata = rbind(d_maindata, d_1person)
  }
  
  # add on the players without any symptoms
  d_time_to_sympt = bind_rows(d_maindata, d %>% filter(id_player %in% no_inj_players)) %>% 
    mutate(id_event = ifelse(id_player %in% no_inj_players, 1, id_event)) %>% 
    rename(not_unique_id = id_event) 
  
  d_eventid = d_time_to_sympt %>% 
    distinct(id_player, not_unique_id) %>% mutate(id_event = 1:n())
  d_time_to_sympt = d_time_to_sympt %>% 
    left_join(d_eventid, by = c("id_player", "not_unique_id")) %>% 
    select(-not_unique_id)
  d_time_to_sympt
}

# find transitions and put them into long format
# not that since state 1 can only go to 2,
# state 2 can only go to 1 and 3 etc.
# we have to do this for each "from" state separately
d_from1 = d_filled  %>% 
  filter(from == 1) %>% 
  group_by(d_imp, id_player) %>% 
                  mutate(to = 2,
                         trans = as.character(1),
                         status = ifelse(knee_state == 2, 1, 0),
                         status = ifelse(is.na(status), 0, status),
                         id_state = 1) %>% 
  ungroup() %>% add_event_id(status)

d_from2 = d_filled  %>% 
  filter(from == 2) %>% 
  group_by(d_imp, id_player) %>% 
  mutate(trans2 = ifelse(knee_state == 1, 1, 0),
         trans2 = ifelse(is.na(trans2), 0, trans2),
         trans3 = ifelse(knee_state == 3, 1, 0),
         trans3 = ifelse(is.na(trans3), 0, trans3)) %>% ungroup() %>% 
 pivot_longer(cols = c("trans2", "trans3"), names_to = "trans", values_to = "status") %>% 
  mutate(trans = str_replace_all(trans, "trans", ""),
         to = ifelse(trans == 2, 1, 3),
         id_state = ifelse(to == 1, 2, 3))

d_from2 = bind_rows(d_from2 %>% filter(id_state == 2) %>% add_event_id(status),
d_from2 %>% filter(id_state == 3) %>% add_event_id(status))


d_from3 = d_filled  %>% 
  filter(from == 3) %>% 
  group_by(d_imp, id_player) %>% 
  mutate(trans4 = ifelse(knee_state == 1, 1, 0),
         trans4 = ifelse(is.na(trans4), 0, trans4),
         trans5 = ifelse(knee_state == 2, 1, 0),
         trans5 = ifelse(is.na(trans5), 0, trans5)) %>% ungroup() %>% 
  pivot_longer(cols = c("trans4", "trans5"), names_to = "trans", values_to = "status") %>% 
  mutate(trans = str_replace_all(trans, "trans", ""),
         to = ifelse(trans == 4, 1, 2),
         id_state = ifelse(to == 1, 4, 5))

d_from3 = bind_rows(d_from3 %>% filter(id_state == 4) %>% add_event_id(status),
                    d_from3 %>% filter(id_state == 5) %>% add_event_id(status))

# bind data
# congratulations, they are now ready for analysis
d_multistate = bind_rows(d_from1, d_from2, d_from3) %>% arrange(d_imp, id_player, date)
# add combined id for dlnm
# it should identify each player
# each event per player
# the state the player is currently in
# the event of interest (per state the player can transition to)
d_multistate = d_multistate %>% mutate(id_dlnm = paste0(id_player, "-", id_event, "-", from, "-", trans))
d_from1 %>% filter(d_imp == 1, id_event == 1)
d_multistate %>% filter(d_imp == 1, id_dlnm == "1-1-1-1")

# function for calculating the q matrix (needed for DLNM) given the survival data in counting process form
# and the exposure history spread in wide format in a matrix
calc_q_matrix = function(d_counting_process, d_tl_hist_wide, id, exit){
  
  id = id
  exit = exit
  
  # for each individual, for each of these exit times, we will extract the exposure history 
  # for the given lag-time which we are interested in
  # This is called the Q-matrix. The Q-matrix should be nrow(dataspl) X 0:lag_max dimensions.
  q = exit %>% map(., ~exphist(test_wide, ., c(lag_min, lag_max))) %>% 
    do.call("rbind", .)
  q
}

l_multistate = (d_multistate %>% group_by(d_imp) %>% nest())$data
d_multistate1 = d_multistate %>% filter(d_imp == 1)

l_tl_hist = l_multistate %>% map(. %>% select(id_dlnm, jumps_n, stop) %>% arrange(stop, id_dlnm))
l_tl_hist_spread_day = 
  l_tl_hist %>% map(. %>% pivot_wider(names_from = stop, values_from = jumps_n) %>% 
                      select(-id_dlnm) %>% as.matrix)
# calc Q matrices
l_q_mat = map2(.x = l_tl_hist,
               .y = l_tl_hist_spread_day, 
               ~calc_q_matrix(.x, .y, .x$id_dlnm, .x$stop))

l_cb_dlnm = l_q_mat %>% map(~crossbasis(., lag=c(lag_min, lag_max), 
                                        argvar = list(fun="ns", knots = 3),
                                        arglag = list(fun="ns", knots = 3)))


# performing a regular Cox model (intercept only)
crcox1 = coxph(Surv(start, stop, status) ~ strata(trans), data = d_multistate1)
mrcox1 = msfit(crcox1, trans = transmat)
plot(mrcox1)
AIC(crcox1)

# add the regular covariates
crcox2 = coxph(Surv(start, stop, status) ~ strata(trans) + position + age + 
                jump_height_max + match + t_prevmatch + frailty(id_player), data = d_multistate1)
AIC(crcox2)
summary(crcox2)
# predicted values
n_trans = max(transmat, na.rm = TRUE)
trans_vec = 1:n_trans
d_preddate =  tibble(
  strata = trans_vec,
  age = rep(30, 5),
  jump_height_max = rep(86, 5),
  match = as.factor(rep(0, 5)),
  p_id = rep(1, 5),
  position = rep("Setter", 5),
  t_prevmatch = rep(6, 5)
)
mrcox2 = msfit(crcox2, newdata = d_preddate, trans = transmat)
plot(mrcox2)

# the cox.zph tests proportional hazards per covariate
cox.zph(crcox2)


# add the DLNM
crcox4 = coxph(Surv(start, stop, status) ~ strata(trans) + position + age + l_cb_dlnm[[1]] +
                 jump_height_max + match + t_prevmatch + frailty(id_player), data = d_multistate1)
AIC(crcox4)

crcox5 = coxme::coxme(Surv(start, stop, status) ~ strata(trans) + position + age + l_cb_dlnm[[1]] +
                 jump_height_max + match + t_prevmatch + (1|id_player), data = d_multistate1)
AIC(crcox5)


# flexsurv have royston parmar models if we are worried about proportional hazards
library(flexsurv)
flexsurv1 = flexsurvreg(Surv(start, stop, status) ~ 1, subset=(trans==1),
            data = d_multistate1, dist = "exp")

flexsurv2 = flexsurvreg(Surv(start, stop, status) ~ position + age + 
              jump_height_max + match + t_prevmatch, subset=(trans==1),
            data = d_multistate1, dist = "exp")

# to obtain p-values
flexsurv2.res <- flexsurv2$res
flexsurv2.wald <- flexsurv2.res[,1]/flexsurv2.res[,4]
flexsurv2.p <- 2*pnorm(-abs(flexsurv2.wald))

options(scipen = 30)
enframe(flexsurv2.p)

# Cox looks like it has a better fit
flex_exp = flexsurvreg(Surv(start, stop, status) ~ trans,
                        data = d_multistate1, dist = "exp")
flex_wei = flexsurvreg(Surv(start, stop, status) ~ trans,
                       data = d_multistate1, dist = "weibull")
AIC(crcox1)
# predtimes
pred_times = seq(1, 300, by = 10)
mrexp = msfit.flexsurvreg(flex_exp, t = pred_times, trans = transmat)

plot(mrexp)


#-----------------------------------------Introducing the DLNM------------------------------
# function for calculating the q matrix (needed for DLNM) given the survival data in counting process form
# and the exposure history spread in wide format in a matrix
calc_q_matrix = function(d_counting_process, d_tl_hist_wide, id, exit){
  
  id = id
  exit = exit
  
  # for each individual, for each of these exit times, we will extract the exposure history 
  # for the given lag-time which we are interested in
  # This is called the Q-matrix. The Q-matrix should be nrow(dataspl) X 0:lag_max dimensions.
  q = map2(.x = id, 
           .y = exit, 
           ~exphist(d_tl_hist_wide[.x,], .y, c(lag_min, lag_max))) %>% 
    do.call("rbind", .)
  q
}




l_surv = (d_time_to_sympt %>% group_by(d_imp) %>% nest())$data

l_tl_hist = l_surv %>% map(. %>% select(id_event, jumps_n, stop))
l_tl_hist_spread_day = 
  l_tl_hist %>% map(. %>% pivot_wider(names_from = stop, values_from = jumps_n) %>% 
                      select(-id_event) %>% as.matrix)

# calc Q matrices
l_q_mat = map2(.x = l_surv,
               .y = l_tl_hist_spread_day, 
               ~calc_q_matrix(.x, .y, .x$id_event, .x$stop))

l_cb_dlnm = l_q_mat %>% map(~crossbasis(., lag=c(lag_min, lag_max), 
                                        argvar = list(fun="ns", knots = 3),
                                        arglag = list(fun="ns", knots = 3)))


crcox4 = coxph(Surv(start, stop, status) ~ position + age + l_cb_dlnm[[1]] +
                 jump_height_max + match + t_prevmatch + frailty(id_player), data = d_time_to_sympt)
AIC(crcox4)








# counting process method
tl_hist_spread_day = d_time_to_sympt %>% select(id_event, jumps_n, stop) %>% 
  pivot_wider(names_from = stop, values_from = jumps_n) %>% 
  select(-id_event) %>% as.matrix

calc_q_matrix(d_from1, tl_hist_spread_day, d_from1$id_player, d_from1$stop)

q_mat = tsModel::Lag(d_from1$jumps_n, lag_min:lag_max)

cb_dlnm = crossbasis(q_mat, lag=c(lag_min, lag_max), 
                      argvar = list(fun="ns", knots = c(50, 100, 150)),
                      arglag = list(fun="ns", knots = 3))


crcox3 = coxph(Surv(start, stop, status) ~ position + age + cb_dlnm +
                 jump_height_max + match + t_prevmatch + frailty(id_player), data = d_from1)
AIC(crcox3)


# find the Q matrix for all the individuals in the whole data.
l_tl_hist = l_multistate %>% 
            map(. %>% distinct(id_player, date, .keep_all = TRUE) %>%  
                select(id_player, jumps_n, stop))
l_tl_hist_spread_day = 
  l_tl_hist %>% map(. %>% pivot_wider(names_from = stop, values_from = jumps_n) %>% 
                      select(-id_player) %>% as.matrix)

d_onestate1 = d_multistate1 %>% distinct(id_player, date, .keep_all = TRUE)
d_q_mat = calc_q_matrix(d_onestate1, l_tl_hist_spread_day[[1]], d_onestate1$id_player, d_onestate1$stop)

# make the crossbasis
# based on the Q matrix
# using the whole data
cb_dlnm =  crossbasis(d_q_mat, lag=c(lag_min, lag_max), 
                                        argvar = list(fun="ns", knots = c(50, 100, 150)),
                                        arglag = list(fun="ns", knots = 3))

# for each potential transition
# we will find the rows in the crossbasis 
# that corresponds to the rows in the data.
# E.g. if a player is in state 1, they can potentially transition 1 to state 2
# if that player was in state 7 days ago, there was no potential for transition 1 to occur
# however, the jumping they did 7 days ago still contributes to their injury risk on the current day
# this is why we make the cross basis on the whole data first, and then subset to the 
# needed rows per transition
d_cb = tibble(cb_dlnm) %>% 
  mutate(id_player = d_onestate1$id_player,
         date = d_onestate1$date,
         id_player_date = paste0(id_player, " ", date))

d_trans1_ids = d_multistate1 %>% filter(trans == 1) %>%
  mutate(id_player_date = paste0(id_player, " ", date))

pos_data = which(d_cb$id_player_date %in% d_trans1_ids$id_player_date)
cb_dlnm_sliced = cb_dlnm[pos_data,]

d_test = d_trans1_ids %>% left_join(d_cb, by = c("id_player", "date"))

# add the regular covariates
crcox3 = coxph(Surv(start, stop, status) ~ strata(trans) + position + age + cb_dlnm +
                 jump_height_max + match + t_prevmatch + frailty(id_player), data = d_multistate1)
AIC(crcox3)

crcox4 = coxph(Surv(start, stop, status) ~ position + age + numeric(cb_dlnm_sliced) +
                 jump_height_max + match + t_prevmatch + frailty(id_player), 
               data = d_multistate1 %>% filter(trans == 1))
AIC(crcox4)


crcox4 = coxph(Surv(start, stop, status) ~ position + age + crossbss[,12] +
                 jump_height_max + match + t_prevmatch + frailty(id_player), 
               data = d_test %>% filter(trans == 1))
AIC(crcox4)


# OK, that didn't work
# try making a CB for each transition type instead.
trans_vec
d_multistate1

l_transitions  = trans_vec %>% map(.x = ., ~d_multistate1 %>% filter(trans == .x))

# find the Q matrix per transition data
l_tl_hist = l_transitions %>% 
  map(. %>% distinct(id_player, date, .keep_all = TRUE) %>%  
        select(id_player, jumps_n, stop))
l_tl_hist_spread_day = 
  l_tl_hist %>% map(. %>% pivot_wider(names_from = stop, values_from = jumps_n) %>% 
                      select(-id_player) %>% as.matrix)


calc_q_matrix(l_transitions[[1]], l_tl_hist_spread_day[[1]], 
              l_transitions[[1]]$id_player, l_transitions[[1]]$stop)

# calc Q matrices
l_q_mat = map2(.x = l_transitions,
               .y = l_tl_hist_spread_day, 
               ~calc_q_matrix(.x, .y, .x$id_player, .x$stop))

# make the crossbasis
l_cb_dlnm = l_q_mat %>% map(~crossbasis(., lag=c(lag_min, lag_max), 
                                        argvar = list(fun="ns", knots = 3),
                                        arglag = list(fun="ns", knots = 3)))


