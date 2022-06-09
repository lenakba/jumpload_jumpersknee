library(tidyverse) # data wrangling
library(dlnm) # distributed lag non-linear models
library(survival)
library(lubridate) # to manipulate dates
library(mstate) # for fitting multistate models
library(coxme) # much better maximization and optimalization for frailty models than the trad survival package

# so we don't have to deal with scientific notations
# and strings aren't automatically read as factors:
options(scipen = 30, 
        stringsAsFactors = FALSE)

data_folder = "D:\\phd\\jump load\\data\\"
d_jumpload = readRDS(paste0(data_folder, "d_jumpload_multimputed.rds"))

# define key columns
key_cols = c("date", "id_player", "id_team", "id_team_player", "id_season")
conf_cols = c("age", "jump_height_max", "position", 
              "match", "t_prevmatch", "jumps_n_weekly", "preseason", "jump_height_sum", "weight")


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
  select(d_imp, id_player, season, date, knee_total_filled, knee_state, jumps_n, all_of(conf_cols))

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
  group_by(d_imp, id_player, season) %>% 
  mutate(day = 1:n(),
         Fup = ifelse(change == 1, day, NA)) %>% fill(Fup, .direction = "up") %>% ungroup()

# find start and stop times
d_surv = d_selected %>% group_by(d_imp, id_player, season) %>% 
  rename(stop = day) %>% 
  mutate(start = lag(stop),
         start = ifelse(is.na(start), 0, start)) %>% ungroup()

# select again for easier analysis
d_surv = d_surv %>% 
  select(d_imp, id_player, season, date, start, stop, knee_state, Fup, jumps_n, all_of(conf_cols)) 
d_surv = d_surv %>% mutate(preseason = as.factor(preseason),
                  position = as.factor(position),
                  match = as.factor(match))

# fixme! better missing solution.
d_filled = d_surv %>% group_by(d_imp, id_player, season) %>% fill(knee_state, .direction = "downup") 
d_filled = d_filled %>% mutate(from = lag(knee_state)) %>% fill(from, .direction = "up")  %>% ungroup()

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
d_multistate = d_multistate %>% mutate(start = as.numeric(start),
                                       stop = as.numeric(stop),
                                       status = as.numeric(status))
# add combined id for dlnm
# it should identify each player
# each season within player
# each event within season
# the state the player is currently in (in that event in that season)
# the event of interest (per state the player can transition to)
d_multistate = d_multistate %>% mutate(id_dlnm = paste0(id_player, "-", season, "-", id_event, "-", from, "-", trans))

# function for calculating the q matrix (needed for DLNM) given the survival data in counting process form
# and the exposure history spread in wide format in a matrix
calc_q_matrix = function(d_tl_hist_wide, id, exit){
  
  id = id
  exit = exit
  
  # for each individual, for each of these exit times, we will extract the exposure history 
  # for the given lag-time which we are interested in
  # This is called the Q-matrix. The Q-matrix should be nrow(dataspl) X 0:lag_max dimensions.
  q = exit %>% map(., ~exphist(d_tl_hist_wide, ., c(lag_min, lag_max))) %>% 
    do.call("rbind", .)
  q
}

l_multistate = (d_multistate %>% group_by(d_imp) %>% nest())$data
d_multistate1 = d_multistate %>% filter(d_imp == 1)

l_tl_hist = l_multistate %>% map(. %>% select(id_player, season, id_dlnm, jumps_n, stop) %>% 
                                   arrange(stop, id_dlnm))
l_tl_hist_spread_day = 
  l_tl_hist %>% map(. %>% pivot_wider(names_from = stop, values_from = jumps_n)  %>% 
                           group_by(id_player, season) %>% 
                           fill(where(is.numeric), .direction = "downup") %>% ungroup() %>% 
                           select(-id_dlnm, -id_player, -season) %>% as.matrix)

# calc Q matrices
l_q_mat = map2(.x = l_tl_hist,
               .y = l_tl_hist_spread_day, 
               ~calc_q_matrix(.y, .x$id_dlnm, .x$stop))

# subjectively placed knots
# since the data is so skewed
# with sparse data >200 jumps on a day
# just check 
# ting = d_analysis %>% filter(d_imp == 1)
# hist(ting$jumps_n)
l_cb_dlnm = l_q_mat %>% map(~crossbasis(., lag=c(lag_min, lag_max), 
                                        argvar = list(fun="ns", knots = c(1, 100, 150)),
                                        arglag = list(fun="ns", knots = 3)))

# do the same for jump height
l_tl_hist_height = l_multistate %>% map(. %>% select(id_player, season, id_dlnm, jump_height_sum, stop) %>% 
                                   arrange(stop, id_dlnm))
l_tl_hist_spread_day_height = 
  l_tl_hist_height %>% map(. %>% pivot_wider(names_from = stop, values_from = jump_height_sum)  %>% 
                      group_by(id_player, season) %>% 
                      fill(where(is.numeric), .direction = "downup") %>% ungroup() %>% 
                      select(-id_dlnm, -id_player, -season) %>% as.matrix)

# calc Q matrices
l_q_mat_height = map2(.x = l_tl_hist_height,
               .y = l_tl_hist_spread_day_height, 
               ~calc_q_matrix(.y, .x$id_dlnm, .x$stop))

# subjectively placed knots
# since the data is so skewed
# with sparse data >200 jumps on a day
# just check 
# ting = d_analysis %>% filter(d_imp == 1)
# hist(ting$jump_height_sum)
l_cb_dlnm_height = l_q_mat_height %>% map(~crossbasis(., lag=c(lag_min, lag_max), 
                                        argvar = list(fun="ns", knots = c(500, 1000, 5000)),
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
  id_player = rep(1, 5),
  position = rep("Setter", 5),
  t_prevmatch = rep(6, 5)
)
mrcox2 = msfit(crcox2, newdata = d_preddate, trans = transmat)
plot(mrcox2)

# the cox.zph tests proportional hazards per covariate
cox.zph(crcox2)

# add the DLNM
# the coxph won't converge, whilst coxme will
cb = l_cb_dlnm[[1]]
cb_height = l_cb_dlnm_height[[1]]
cox4 = coxph(Surv(start, stop, status) ~ strata(trans) + position + age + cb +
                 jump_height_max + match + t_prevmatch + frailty(id_player), data = d_multistate1)
AIC(cox4)


cox_freq = coxme(Surv(start, stop, status) ~ strata(trans) + position + age + cb + 
                 jump_height_max + match + t_prevmatch + season + (1|id_player), 
                 data = d_multistate1, subset=(jumps_n!=0))
AIC(cox_freq)

cox_height = coxme(Surv(start, stop, status) ~ strata(trans) + position + age + cb_height + 
                   jump_height_max + match + weight + season + (1|id_player), 
                 data = d_multistate1, subset=(jumps_n!=0))
AIC(cox_height)

# predicted values
n_trans = max(transmat, na.rm = TRUE)
trans_vec = 1:n_trans
d_preddate =  tibble(
  strata = trans_vec,
  age = rep(30, 5),
  jump_height_max = rep(86, 5),
  match = as.factor(rep(0, 5)),
  id_player = rep(1, 5),
  position = rep("Setter", 5),
  t_prevmatch = rep(6, 5),
  cb = rep(75, 5)
)

msfit(object = cox4, newdata = d_preddate, trans = transmat)

mr_freq = msfit(cox_freq, newdata = d_preddate, trans = transmat)
plot(mr_freq)

plot(cox_freq)

# method to manually get the pooled results from coxme: https://github.com/amices/mice/issues/123
l_cox_freq = 
   map2(.x = l_multistate,
        .y = l_cb_dlnm,
        ~coxme(Surv(start, stop, status) ~ strata(trans) + position + age + .y +
                 jump_height_max + match + t_prevmatch + season + (1|id_player), data = .x, subset=jumps_n!=0))


cox_unadj = coxme(Surv(start, stop, status) ~ strata(trans) + l_cb_dlnm[[1]] + (1|id_player), data = d_multistate1)
AIC(cox_unadj)
summary(cox_unadj)

# flexsurv have royston parmar models if we are worried about proportional hazards
library(flexsurv)
flexsurv1 = flexsurvreg(Surv(start, stop, status) ~ cb, subset=(trans==1),
            data = d_multistate1, dist = "exp")

flexsurv2 = flexsurvreg(Surv(start, stop, status) ~ position + age + cb +
              jump_height_max + match + t_prevmatch, subset=(trans==1),
            data = d_multistate1, dist = "exp")

# to obtain p-values
flexsurv2.res <- flexsurv2$res
flexsurv2.wald <- flexsurv2.res[,1]/flexsurv2.res[,4]
flexsurv2.p <- 2*pnorm(-abs(flexsurv2.wald))

options(scipen = 30)
enframe(flexsurv2.p)

# Cox looks like it has a better fit
flex_exp = flexsurvreg(Surv(start, stop, status) ~ trans + position + age + cb +
                         jump_height_max + match + t_prevmatch,
                        data = d_multistate1, dist = "exp")
flex_wei = flexsurvreg(Surv(start, stop, status) ~ trans,
                       data = d_multistate1, dist = "weibull")
AIC(crcox1)
# predtimes
pred_times = seq(1, 300, by = 10)

n_trans = max(transmat, na.rm = TRUE)
trans_vec = 1:n_trans
d_preddate =  tibble(
  strata = trans_vec,
  age = rep(30, 5),
  jump_height_max = rep(86, 5),
  match = as.factor(rep(0, 5)),
  id_player = rep(1, 5),
  position = rep("Setter", 5),
  t_prevmatch = rep(6, 5),
  cb = rep(75, 5)
)

mrexp = msfit.flexsurvreg(flex_exp, t = pred_times, trans = transmat, newdata = d_preddate)
plot(mrexp)


#-------------------------------------include interval-censoring---------------------------

d_multistate_cens = d_multistate %>% mutate(stop_cens = ifelse(status == 1, stop+6, stop),
                        status_cens = ifelse(status == 1, 3, status))
d_multistate_cens1 = d_multistate_cens %>% filter(d_imp == 1)

l_multistate_cens = (d_multistate_cens %>% group_by(d_imp) %>% nest())$data
l_tl_hist_cens = l_multistate_cens %>% map(. %>% select(id_player, season, id_dlnm, jumps_n, stop_cens) %>% arrange(stop_cens, id_dlnm))
l_tl_hist_spread_day_cens = 
  l_tl_hist_cens %>% map(. %>% pivot_wider(names_from = stop_cens, values_from = jumps_n)  %>% 
                           group_by(id_player, season) %>% 
                           fill(where(is.numeric), .direction = "downup") %>% ungroup() %>% 
                           select(-id_dlnm, -id_player, -season) %>% as.matrix)
# calc Q matrices
l_q_mat = map2(.x = l_tl_hist_cens,
               .y = l_tl_hist_spread_day_cens, 
               ~calc_q_matrix(.y, .x$id_dlnm, .x$stop_cens))


# calc Q matrices
l_q_mat = map2(.x = l_tl_hist,
               .y = l_tl_hist_spread_day, 
               ~calc_q_matrix(.y, .x$id_dlnm, .x$stop))

l_q_mat_7lag = l_q_mat %>% map(., ~.[,-(1:7)])

d_multistate_cens1 %>% filter(id_player == 24) %>% View()


# subjectively placed knots
# since the data is so skewed
# with sparse data >200 jumps on a day
# just check 
# ting = d_analysis %>% filter(d_imp == 1)
# hist(ting$jumps_n)
lag_min_cens = 7
l_cb_dlnm = l_q_mat %>% map(~crossbasis(., lag=c(lag_min_cens, lag_max), 
                                        argvar = list(fun="ns", knots = c(50, 100, 150)),
                                        arglag = list(fun="ns", knots = 3)))

lag_min_cens = 7
l_cb_dlnm_7lag = l_q_mat_7lag %>% map(~crossbasis(., lag=c(lag_min_cens, lag_max), 
                                        argvar = list(fun="ns", knots = c(50, 100, 150)),
                                        arglag = list(fun="ns", knots = 3)))


survobject = Surv(d_multistate_cens1$start, 
                  d_multistate_cens1$stop_cens, 
                  d_multistate_cens1$status_cens, 
                  type = "interval")

icenReg::ic_sp(survobject ~ strata(trans) + position + age + season + l_cb_dlnm_7lag[[1]] +
                 jump_height_max + match + t_prevmatch, model = 'ph',
                bs_samples = 2, data = d_multistate_cens1)


survreg_cens = survreg(survobject ~ strata(trans) + position + age + l_cb_dlnm_7lag[[1]] +
                     jump_height_max + match + t_prevmatch , data = d_multistate_cens1)
AIC(survreg_cens)

#------------------------------------------Fewer strata---------------------------------------------

# we will include "worse" states in the symptomatic state

d_from_sympt = d_filled %>% 
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

d_from_sympt = bind_rows(d_from_sympt %>% filter(id_state == 2) %>% add_event_id(status),
                    d_from_sympt %>% filter(id_state == 3) %>% add_event_id(status))

d_from_worse = d_filled  %>% 
  filter(from == 3) %>% 
  group_by(d_imp, id_player) %>% 
  mutate(trans2 = ifelse(knee_state == 1, 1, 0),
         trans2 = ifelse(is.na(trans2), 0, trans2)) %>% ungroup() %>% 
  pivot_longer(cols = c("trans2"), names_to = "trans", values_to = "status") %>% 
  mutate(trans = str_replace_all(trans, "trans", ""),
         to = 1,
         id_state = 2,
         from = 2)

d_from_worse = d_from_worse %>% add_event_id(status)


# bind data
# congratulations, they are now ready for analysis
d_multistate_3trans = bind_rows(d_from1, d_from_sympt, d_from_worse) %>% arrange(d_imp, id_player, date)
d_multistate_3trans = d_multistate %>% mutate(start = as.numeric(start),
                                       stop = as.numeric(stop),
                                       status = as.numeric(status))
# add combined id for dlnm
# it should identify each player
# each season within player
# each event within season
# the state the player is currently in (in that event in that season)
# the event of interest (per state the player can transition to)
d_multistate_3trans = d_multistate_3trans %>% mutate(id_dlnm = paste0(id_player, "-", season, "-", id_event, "-", from, "-", trans))


fit_3trans = coxph(Surv(start, stop, status) ~ strata(trans) + position + age + 
                 jump_height_max + match + t_prevmatch + frailty(id_player), data = d_multistate_3trans)
AIC(fit_3trans)
summary(fit_3trans)


# interval censored
d_multistate_cens = d_multistate_3trans %>% mutate(stop_cens = ifelse(status == 1, stop+6, stop),
                                            status_cens = ifelse(status == 1, 3, status))
d_multistate_cens1 = d_multistate_cens %>% filter(d_imp == 1)

l_multistate_cens = (d_multistate_cens %>% group_by(d_imp) %>% nest())$data
l_tl_hist_cens = l_multistate_cens %>% map(. %>% select(id_dlnm, jumps_n, stop_cens) %>% arrange(id_dlnm, stop_cens))
l_tl_hist_spread_day_cens = 
  l_tl_hist_cens %>% map(. %>% pivot_wider(names_from = stop_cens, values_from = jumps_n) %>% 
                           select(-id_dlnm) %>% as.matrix)
# calc Q matrices
l_q_mat = map2(.x = l_tl_hist_cens,
               .y = l_tl_hist_spread_day_cens, 
               ~calc_q_matrix(.x, .y, .x$id_dlnm, .x$stop_cens))

# subjectively placed knots
# since the data is so skewed
# with sparse data >200 jumps on a day
# just check 
# ting = d_analysis %>% filter(d_imp == 1)
# hist(ting$jumps_n)
l_cb_dlnm = l_q_mat %>% map(~crossbasis(., lag=c(lag_min, lag_max), 
                                        argvar = list(fun="ns", knots = c(1, 100, 150)),
                                        arglag = list(fun="ns", knots = 3)))

survobject = Surv(d_multistate_cens1$start, 
                  d_multistate_cens1$stop_cens, 
                  d_multistate_cens1$status_cens, 
                  type = "interval")

icenReg::ic_sp(survobject ~ strata(trans) + position + age + season +
                 jump_height_max + match + t_prevmatch, model = 'ph',
               bs_samples = 2, data = d_multistate_cens1)


#--------------------------------------------Figures----------------------------------------------

l_cox_freq_mstate = 
  map2(.x = l_multistate,
       .y = l_cb_dlnm,
       ~coxme(Surv(start, stop, status) ~ strata(trans) + position + age + .y +
                jump_height_max + match + t_prevmatch + season + (1|id_player), 
              data = .x, 
              subset=(jumps_n!=0)))

l_cox_freq_risk = 
  map2(.x = l_multistate,
       .y = l_cb_dlnm,
       ~coxme(Surv(start, stop, status) ~ position + age + .y +
                jump_height_max + match + t_prevmatch + season + (1|id_player), 
              data = .x, 
              subset=((jumps_n!=0) & (trans == 1))))


l_cox_freq_improv = 
  map2(.x = l_multistate,
       .y = l_cb_dlnm,
       ~coxme(Surv(start, stop, status) ~ position + age + .y +
                jump_height_max + match + t_prevmatch + season + (1|id_player), 
              data = .x, 
              subset=(trans == 2)))

l_cox_freq_worse = 
  map2(.x = l_multistate,
       .y = l_cb_dlnm,
       ~coxme(Surv(start, stop, status) ~ position + age + .y +
                jump_height_max + match + t_prevmatch + season + (1|id_player), 
              data = .x, 
              subset=(trans == 3)))


library(lmisc) # loading local package for figure settings
# shared figure options
text_size = 14
ostrc_theme =  theme(panel.border = element_blank(), 
                     panel.background = element_blank(),
                     panel.grid = element_blank(),
                     axis.line = element_line(color = nih_distinct[4]),
                     strip.background = element_blank(),
                     strip.text.x = element_text(size = text_size, family="Trebuchet MS", colour="black", face = "bold", hjust = -0.01),
                     axis.ticks = element_line(color = nih_distinct[4]),
                     legend.position = "bottom")

# vector of tl values used in visualizations of predictions
predvalues = seq(min(d_analysis$jumps_n), 250, 10)
lag_seq = lag_min:lag_max 

# predict hazards
l_cp_preds_dlnm = 
  map2(.x = l_cox_freq_risk,
       .y = l_cb_dlnm,
       ~crosspred(.y, .x, at = predvalues, cen = 0, cumul = TRUE))
glimpse(l_cp_preds_dlnm)

# function for plucking the right matrix out of the crosspred list within the list of crosspred lists
pluck_mat = function(x, pos){pluck(l_cp_preds_dlnm, x, pos)}
# the crosspred list has changed
allRRfit = 9
d_preds_cumul1 = pluck_mat(1, allRRfit)
d_preds_cumul2 = pluck_mat(2, allRRfit)
d_preds_cumul3 = pluck_mat(3, allRRfit)
d_preds_cumul4 = pluck_mat(4, allRRfit)
d_preds_cumul5 = pluck_mat(5, allRRfit)
l_cumulRRfit = list(d_preds_cumul1, d_preds_cumul2, d_preds_cumul3, d_preds_cumul4, d_preds_cumul5)
# average across preds
mat_cumulRRfit = reduce(l_cumulRRfit, `+`) / length(l_cumulRRfit)

# conflow
allRRfit_low = 15
d_preds_cumullow1 = pluck_mat(1, allRRfit_low)
d_preds_cumullow2 = pluck_mat(2, allRRfit_low)
d_preds_cumullow3 = pluck_mat(3, allRRfit_low)
d_preds_cumullow4 = pluck_mat(4, allRRfit_low)
d_preds_cumullow5 = pluck_mat(5, allRRfit_low)
l_cumulRRfit_low = list(d_preds_cumullow1, d_preds_cumullow2, d_preds_cumullow3, d_preds_cumullow4, d_preds_cumullow5)
# average across preds
mat_cumulRRfit_low = reduce(l_cumulRRfit_low, `+`) / length(l_cumulRRfit_low)

# confhigh
allRRfit_high = 16
d_preds_cumulhigh1 = pluck_mat(1, allRRfit_high)
d_preds_cumulhigh2 = pluck_mat(2, allRRfit_high)
d_preds_cumulhigh3 = pluck_mat(3, allRRfit_high)
d_preds_cumulhigh4 = pluck_mat(4, allRRfit_high)
d_preds_cumulhigh5 = pluck_mat(5, allRRfit_high)
l_cumulRRfit_cumulhigh = list(d_preds_cumulhigh1, d_preds_cumulhigh2, d_preds_cumulhigh3, d_preds_cumulhigh4, d_preds_cumulhigh5)
# average across preds
mat_cumulRRfit_high = reduce(l_cumulRRfit_cumulhigh, `+`) / length(l_cumulRRfit_cumulhigh)

d_cumul = as_tibble(mat_cumulRRfit) %>% 
  mutate(jumps_n = predvalues, ci_low = mat_cumulRRfit_low, ci_high = mat_cumulRRfit_high)
plot_cumul = ggplot(d_cumul, aes(x = jumps_n, y = value, group = 1)) +
  geom_ribbon(aes(min = ci_low, max = ci_high), alpha = 0.3, fill = nih_distinct[1]) +
  geom_hline(yintercept = 1, alpha = 0.3, size = 1) +
  geom_line(size = 0.75, color = nih_distinct[4]) +
  theme_base(text_size) +
  ostrc_theme +
  xlab("N jumps daily") +
  ylab("Cumulative HR on Day 0") 

# 13 is matRRfit
matRRfit = 7
d_preds1 = pluck_mat(1, matRRfit)
d_preds2 = pluck_mat(2, matRRfit)
d_preds3 = pluck_mat(3, matRRfit)
d_preds4 = pluck_mat(4, matRRfit)
d_preds5 = pluck_mat(5, matRRfit)
l_matRRfit = list(d_preds1, d_preds2, d_preds3, d_preds4, d_preds5)
# average across preds
mat_matRRfit = reduce(l_matRRfit, `+`) / length(l_matRRfit)

# conflow
matRRfit_low = 13
d_preds_low1 = pluck_mat(1, matRRfit_low)
d_preds_low2 = pluck_mat(2, matRRfit_low)
d_preds_low3 = pluck_mat(3, matRRfit_low)
d_preds_low4 = pluck_mat(4, matRRfit_low)
d_preds_low5 = pluck_mat(5, matRRfit_low)
l_matRRfit_low = list(d_preds_low1, d_preds_low2, d_preds_low3, d_preds_low4, d_preds_low5)
# average across preds
mat_matRRfit_low = reduce(l_matRRfit_low, `+`) / length(l_matRRfit_low)

# confhigh
matRRfit_high = 14
d_preds_high1 = pluck_mat(1, matRRfit_high)
d_preds_high2 = pluck_mat(2, matRRfit_high)
d_preds_high3 = pluck_mat(3, matRRfit_high)
d_preds_high4 = pluck_mat(4, matRRfit_high)
d_preds_high5 = pluck_mat(5, matRRfit_high)
l_matRRfit_high = list(d_preds_high1, d_preds_high2, d_preds_high3, d_preds_high4, d_preds_high5)
# average across preds
mat_matRRfit_high = reduce(l_matRRfit_high, `+`) / length(l_matRRfit_high)

# lag-response curve for jumps 100
jumps_fixed = "100"
rownumber = which(rownames(mat_matRRfit)==jumps_fixed)
d_preds_per_lag = as_tibble(mat_matRRfit[rownumber,]) %>% 
  rename(coef = value) %>% 
  mutate(lag = 0:27,
         ci_low = mat_matRRfit_low[rownumber,],
         ci_high = mat_matRRfit_high[rownumber,])

cairo_pdf("figure2_3d.pdf", width = 12, height = 7)
persp(x = predvalues, y = lag_seq, mat_matRRfit, ticktype="detailed", 
      theta=230, ltheta=150, phi=40, lphi=30,
      ylab="Lag (Days)", zlab="HR", shade=0.75, 
      r=sqrt(3), d=5, cex.axis=1.2, cex.lab=1.2,
      border=grey(0.2), col = nih_distinct[1], 
      xlab = "N jumps", main = "3D plane of effects")
dev.off()

# exposure-response curve for lag 0
lag_fixed = "lag0"
colnumber = which(colnames(mat_matRRfit) == lag_fixed)
d_preds_per_jump = as_tibble(mat_matRRfit[,colnumber]) %>% 
  rename(coef = value) %>% 
  mutate(jumps_n = predvalues,
         ci_low = mat_matRRfit_low[,colnumber],
         ci_high = mat_matRRfit_high[,colnumber])

plot_dlnm2d1 = ggplot(d_preds_per_jump, aes(x = jumps_n, y = coef, group = 1)) +
  geom_hline(yintercept = 1, alpha = 0.3, size = 1) +
  geom_ribbon(aes(min = ci_low, max = ci_high), alpha = 0.3, fill = nih_distinct[1]) +
  geom_line(size = 0.75, color = nih_distinct[4]) +
  theme_base(text_size) +
  ostrc_theme +
  xlab("N jumps") +
  ylab("HR on Day 0")

lag_fixed = "lag15"
colnumber = which(colnames(mat_matRRfit) == lag_fixed)
d_preds_per_jump = as_tibble(mat_matRRfit[,colnumber]) %>% 
  rename(coef = value) %>% 
  mutate(jumps_n = predvalues,
         ci_low = mat_matRRfit_low[,colnumber],
         ci_high = mat_matRRfit_high[,colnumber])

plot_dlnm2d2 = ggplot(d_preds_per_jump, aes(x = jumps_n, y = coef, group = 1)) +
  geom_hline(yintercept = 1, alpha = 0.3, size = 1) +
  geom_ribbon(aes(min = ci_low, max = ci_high), alpha = 0.3, fill = nih_distinct[1]) +
  geom_line(size = 0.75, color = nih_distinct[4]) +
  theme_base(text_size) +
  ostrc_theme +
  xlab("N jumps") +
  ylab("HR on Day 15") 

lag_fixed = "lag27"
colnumber = which(colnames(mat_matRRfit) == lag_fixed)
d_preds_per_jump = as_tibble(mat_matRRfit[,colnumber]) %>% 
  rename(coef = value) %>% 
  mutate(jumps_n = predvalues,
         ci_low = mat_matRRfit_low[,colnumber],
         ci_high = mat_matRRfit_high[,colnumber])

plot_dlnm2d3 = ggplot(d_preds_per_jump, aes(x = jumps_n, y = coef, group = 1)) +
  geom_hline(yintercept = 1, alpha = 0.3, size = 1) +
  geom_ribbon(aes(min = ci_low, max = ci_high), alpha = 0.3, fill = nih_distinct[1]) +
  geom_line(size = 0.75, color = nih_distinct[4]) +
  theme_base(text_size) +
  ostrc_theme +
  xlab("N jumps") +
  ylab("HR on Day 27") 

cairo_pdf("figure1.pdf", width = 10, height = 8)
ggpubr::ggarrange(plot_cumul, plot_dlnm2d1, plot_dlnm2d2, plot_dlnm2d3, ncol = 2, nrow = 2, labels = c("A Cumulative effect", "B Risk on current day", "C Risk on 15th day", "D Risk on 27th day"))
dev.off()

#--------------------------------------------Figures height----------------------------------------------

l_cox_freq_mstate_h = 
  map2(.x = l_multistate,
       .y = l_cb_dlnm_height,
       ~coxme(Surv(start, stop, status) ~ strata(trans) + position + age + .y +
                jump_height_max + match + weight + season + (1|id_player), 
              data = .x, 
              subset=(jumps_n!=0)))

l_cox_freq_risk_h = 
  map2(.x = l_multistate,
       .y = l_cb_dlnm_height,
       ~coxme(Surv(start, stop, status) ~ position + age + .y +
                jump_height_max + match + weight + season + (1|id_player), 
              data = .x, 
              subset=((jumps_n!=0) & (trans == 1))))


l_cox_freq_improv_h = 
  map2(.x = l_multistate,
       .y = l_cb_dlnm_height,
       ~coxme(Surv(start, stop, status) ~ position + age + .y +
                jump_height_max + match + weight + season + (1|id_player), 
              data = .x, 
              subset=(trans == 2)))

l_cox_freq_worse_h = 
  map2(.x = l_multistate,
       .y = l_cb_dlnm_height,
       ~coxme(Surv(start, stop, status) ~ position + age + .y +
                jump_height_max + match + weight + season + (1|id_player), 
              data = .x, 
              subset=(trans == 3)))


library(lmisc) # loading local package for figure settings
# shared figure options
text_size = 14
ostrc_theme =  theme(panel.border = element_blank(), 
                     panel.background = element_blank(),
                     panel.grid = element_blank(),
                     axis.line = element_line(color = nih_distinct[4]),
                     strip.background = element_blank(),
                     strip.text.x = element_text(size = text_size, family="Trebuchet MS", colour="black", face = "bold", hjust = -0.01),
                     axis.ticks = element_line(color = nih_distinct[4]),
                     legend.position = "bottom")

# vector of tl values used in visualizations of predictions
predvalues_h = seq(min(d_analysis$jump_height_sum), 10000, 30)
lag_seq = lag_min:lag_max 

# predict hazards
l_cp_preds_dlnm_h = 
  map2(.x = l_cox_freq_risk_h,
       .y = l_cb_dlnm_height,
       ~crosspred(.y, .x, at = predvalues_h, cen = 0, cumul = TRUE))
glimpse(l_cp_preds_dlnm_h)

# function for plucking the right matrix out of the crosspred list within the list of crosspred lists
pluck_mat = function(x, pos){pluck(l_cp_preds_dlnm_h, x, pos)}
# the crosspred list has changed
allRRfit = 9
d_preds_cumul1 = pluck_mat(1, allRRfit)
d_preds_cumul2 = pluck_mat(2, allRRfit)
d_preds_cumul3 = pluck_mat(3, allRRfit)
d_preds_cumul4 = pluck_mat(4, allRRfit)
d_preds_cumul5 = pluck_mat(5, allRRfit)
l_cumulRRfit = list(d_preds_cumul1, d_preds_cumul2, d_preds_cumul3, d_preds_cumul4, d_preds_cumul5)
# average across preds
mat_cumulRRfit = reduce(l_cumulRRfit, `+`) / length(l_cumulRRfit)

# conflow
allRRfit_low = 15
d_preds_cumullow1 = pluck_mat(1, allRRfit_low)
d_preds_cumullow2 = pluck_mat(2, allRRfit_low)
d_preds_cumullow3 = pluck_mat(3, allRRfit_low)
d_preds_cumullow4 = pluck_mat(4, allRRfit_low)
d_preds_cumullow5 = pluck_mat(5, allRRfit_low)
l_cumulRRfit_low = list(d_preds_cumullow1, d_preds_cumullow2, d_preds_cumullow3, d_preds_cumullow4, d_preds_cumullow5)
# average across preds
mat_cumulRRfit_low = reduce(l_cumulRRfit_low, `+`) / length(l_cumulRRfit_low)

# confhigh
allRRfit_high = 16
d_preds_cumulhigh1 = pluck_mat(1, allRRfit_high)
d_preds_cumulhigh2 = pluck_mat(2, allRRfit_high)
d_preds_cumulhigh3 = pluck_mat(3, allRRfit_high)
d_preds_cumulhigh4 = pluck_mat(4, allRRfit_high)
d_preds_cumulhigh5 = pluck_mat(5, allRRfit_high)
l_cumulRRfit_cumulhigh = list(d_preds_cumulhigh1, d_preds_cumulhigh2, d_preds_cumulhigh3, d_preds_cumulhigh4, d_preds_cumulhigh5)
# average across preds
mat_cumulRRfit_high = reduce(l_cumulRRfit_cumulhigh, `+`) / length(l_cumulRRfit_cumulhigh)

d_cumul = as_tibble(mat_cumulRRfit) %>% 
  mutate(jump_height_sum = predvalues_h, ci_low = mat_cumulRRfit_low, ci_high = mat_cumulRRfit_high)
plot_cumul = ggplot(d_cumul, aes(x = jump_height_sum, y = value, group = 1)) +
  geom_ribbon(aes(min = ci_low, max = ci_high), alpha = 0.3, fill = nih_distinct[1]) +
  geom_hline(yintercept = 1, alpha = 0.3, size = 1) +
  geom_line(size = 0.75, color = nih_distinct[4]) +
  theme_base(text_size) +
  ostrc_theme +
  xlab("Daily sum of jump heights") +
  ylab("Cumulative HR on Day 0") 

# 13 is matRRfit
matRRfit = 7
d_preds1 = pluck_mat(1, matRRfit)
d_preds2 = pluck_mat(2, matRRfit)
d_preds3 = pluck_mat(3, matRRfit)
d_preds4 = pluck_mat(4, matRRfit)
d_preds5 = pluck_mat(5, matRRfit)
l_matRRfit = list(d_preds1, d_preds2, d_preds3, d_preds4, d_preds5)
# average across preds
mat_matRRfit = reduce(l_matRRfit, `+`) / length(l_matRRfit)

# conflow
matRRfit_low = 13
d_preds_low1 = pluck_mat(1, matRRfit_low)
d_preds_low2 = pluck_mat(2, matRRfit_low)
d_preds_low3 = pluck_mat(3, matRRfit_low)
d_preds_low4 = pluck_mat(4, matRRfit_low)
d_preds_low5 = pluck_mat(5, matRRfit_low)
l_matRRfit_low = list(d_preds_low1, d_preds_low2, d_preds_low3, d_preds_low4, d_preds_low5)
# average across preds
mat_matRRfit_low = reduce(l_matRRfit_low, `+`) / length(l_matRRfit_low)

# confhigh
matRRfit_high = 14
d_preds_high1 = pluck_mat(1, matRRfit_high)
d_preds_high2 = pluck_mat(2, matRRfit_high)
d_preds_high3 = pluck_mat(3, matRRfit_high)
d_preds_high4 = pluck_mat(4, matRRfit_high)
d_preds_high5 = pluck_mat(5, matRRfit_high)
l_matRRfit_high = list(d_preds_high1, d_preds_high2, d_preds_high3, d_preds_high4, d_preds_high5)
# average across preds
mat_matRRfit_high = reduce(l_matRRfit_high, `+`) / length(l_matRRfit_high)

# lag-response curve for jumps 100
jumps_fixed = "3000"
rownumber = which(rownames(mat_matRRfit)==jumps_fixed)
d_preds_per_lag = as_tibble(mat_matRRfit[rownumber,]) %>% 
  rename(coef = value) %>% 
  mutate(lag = 0:27,
         ci_low = mat_matRRfit_low[rownumber,],
         ci_high = mat_matRRfit_high[rownumber,])

cairo_pdf("figure2_3d.pdf", width = 12, height = 7)
persp(x = predvalues_h, y = lag_seq, mat_matRRfit, ticktype="detailed", 
      theta=230, ltheta=150, phi=40, lphi=30,
      ylab="Lag (Days)", zlab="HR", shade=0.75, 
      r=sqrt(3), d=5, cex.axis=1.2, cex.lab=1.2,
      border=grey(0.2), col = nih_distinct[1], 
      xlab = "Daily sum of jump height", main = "3D plane of effects")
dev.off()

# exposure-response curve for lag 0
lag_fixed = "lag0"
colnumber = which(colnames(mat_matRRfit) == lag_fixed)
d_preds_per_jump = as_tibble(mat_matRRfit[,colnumber]) %>% 
  rename(coef = value) %>% 
  mutate(jump_height_sum = predvalues_h,
         ci_low = mat_matRRfit_low[,colnumber],
         ci_high = mat_matRRfit_high[,colnumber])

plot_dlnm2d1 = ggplot(d_preds_per_jump, aes(x = jump_height_sum, y = coef, group = 1)) +
  geom_hline(yintercept = 1, alpha = 0.3, size = 1) +
  geom_ribbon(aes(min = ci_low, max = ci_high), alpha = 0.3, fill = nih_distinct[1]) +
  geom_line(size = 0.75, color = nih_distinct[4]) +
  theme_base(text_size) +
  ostrc_theme +
  xlab("Daily sum of jumps") +
  ylab("HR on Day 0")

lag_fixed = "lag15"
colnumber = which(colnames(mat_matRRfit) == lag_fixed)
d_preds_per_jump = as_tibble(mat_matRRfit[,colnumber]) %>% 
  rename(coef = value) %>% 
  mutate(jump_height_sum = predvalues_h,
         ci_low = mat_matRRfit_low[,colnumber],
         ci_high = mat_matRRfit_high[,colnumber])

plot_dlnm2d2 = ggplot(d_preds_per_jump, aes(x = jump_height_sum, y = coef, group = 1)) +
  geom_hline(yintercept = 1, alpha = 0.3, size = 1) +
  geom_ribbon(aes(min = ci_low, max = ci_high), alpha = 0.3, fill = nih_distinct[1]) +
  geom_line(size = 0.75, color = nih_distinct[4]) +
  theme_base(text_size) +
  ostrc_theme +
  xlab("Daily sum of jumps") +
  ylab("HR on Day 15") 

lag_fixed = "lag27"
colnumber = which(colnames(mat_matRRfit) == lag_fixed)
d_preds_per_jump = as_tibble(mat_matRRfit[,colnumber]) %>% 
  rename(coef = value) %>% 
  mutate(jump_height_sum = predvalues_h,
         ci_low = mat_matRRfit_low[,colnumber],
         ci_high = mat_matRRfit_high[,colnumber])

plot_dlnm2d3 = ggplot(d_preds_per_jump, aes(x = jump_height_sum, y = coef, group = 1)) +
  geom_hline(yintercept = 1, alpha = 0.3, size = 1) +
  geom_ribbon(aes(min = ci_low, max = ci_high), alpha = 0.3, fill = nih_distinct[1]) +
  geom_line(size = 0.75, color = nih_distinct[4]) +
  theme_base(text_size) +
  ostrc_theme +
  xlab("N jumps") +
  ylab("HR on Day 27") 

cairo_pdf("figure1.pdf", width = 10, height = 8)
ggpubr::ggarrange(plot_cumul, plot_dlnm2d1, plot_dlnm2d2, plot_dlnm2d3, ncol = 2, nrow = 2, labels = c("A Cumulative effect", "B Risk on current day", "C Risk on 15th day", "D Risk on 27th day"))
dev.off()
