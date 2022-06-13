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
              "match", "t_prevmatch", "jumps_n_weekly", "preseason", "jump_height_sum", "jump_height_sum_perc","weight")


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
         jump_height_sum_perc,
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

# make one variable with knee state
# where a substantial state is prioritized over symptomatic state
d_kneelevels = d_analysis %>% group_by(d_imp, id_player) %>% 
  mutate(knee_state = ifelse(inj_knee_subst_filled == 1, 3, inj_knee_filled+1)) %>% ungroup() 

# select variables we are going to use.
d_selected = d_kneelevels  %>% 
  select(d_imp, id_player, season, date, knee_state, jumps_n, all_of(conf_cols))

#---------------------------------------multistate model------------------------------------------

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
statenames = c("asymptomatic", "symptomatic", "substantial")
l_transitions = list(c(2, 3), 
                     c(1, 3), 
                     c(1, 2))
transmat = mstate::transMat(l_transitions, statenames)

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
  mutate(enter = lag(stop),
         enter = ifelse(is.na(enter), 0, enter)) %>% ungroup()

# select again for easier analysis
d_surv = d_surv %>% 
  select(d_imp, id_player, season, date, enter, stop, knee_state, Fup, jumps_n, all_of(conf_cols)) 
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
# state 1 is asymptomatic
# state 2 is symptomatic
# state 3 is substantial
# from 1 to 2 is transition 1
# from 1 to 3 is transition 2
# from 2 to 1 is transition 3
# from 2 to 3 is transition 4
# from 3 to 1 is transition 5
# from 3 to 2 is transition 6
d_from1 = d_filled  %>% 
  filter(from == 1) %>% 
  group_by(d_imp, id_player) %>% 
  mutate(trans1 = ifelse(knee_state == 2, 1, 0),
         trans1 = ifelse(is.na(trans1), 0, trans1),
         trans2 = ifelse(knee_state == 3, 1, 0),
         trans2 = ifelse(is.na(trans2), 0, trans2)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c("trans1", "trans2"), names_to = "trans", values_to = "status") %>% 
  mutate(trans = str_replace_all(trans, "trans", ""),
         to = ifelse(trans == 1, 1, 3),
         id_state = ifelse(to == 1, 2, 3))

d_from2 = bind_rows(d_from2 %>% filter(id_state == 2) %>% add_event_id(status),
                    d_from2 %>% filter(id_state == 3) %>% add_event_id(status))

d_from2 = d_filled  %>% 
  filter(from == 2) %>% 
  group_by(d_imp, id_player) %>% 
  mutate(trans3 = ifelse(knee_state == 1, 1, 0),
         trans3 = ifelse(is.na(trans3), 0, trans3),
         trans4 = ifelse(knee_state == 3, 1, 0),
         trans4 = ifelse(is.na(trans4), 0, trans4)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c("trans3", "trans4"), names_to = "trans", values_to = "status") %>% 
  mutate(trans = str_replace_all(trans, "trans", ""),
         to = ifelse(trans == 3, 1, 3),
         id_state = ifelse(to == 1, 2, 3))

d_from2 = bind_rows(d_from2 %>% filter(id_state == 2) %>% add_event_id(status),
                    d_from2 %>% filter(id_state == 3) %>% add_event_id(status))


d_from3 = d_filled  %>% 
  filter(from == 3) %>% 
  group_by(d_imp, id_player) %>% 
  mutate(trans5 = ifelse(knee_state == 1, 1, 0),
         trans5 = ifelse(is.na(trans5), 0, trans5),
         trans6 = ifelse(knee_state == 2, 1, 0),
         trans6 = ifelse(is.na(trans6), 0, trans6)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c("trans5", "trans6"), names_to = "trans", values_to = "status") %>% 
  mutate(trans = str_replace_all(trans, "trans", ""),
         to = ifelse(trans == 5, 1, 2),
         id_state = ifelse(to == 1, 4, 5))

d_from3 = bind_rows(d_from3 %>% filter(id_state == 4) %>% add_event_id(status),
                    d_from3 %>% filter(id_state == 5) %>% add_event_id(status))

# bind data
# congratulations, they are now ready for analysis
d_multistate = bind_rows(d_from1, d_from2, d_from3) %>% arrange(d_imp, id_player, date)
d_multistate = d_multistate %>% mutate(enter = as.numeric(enter),
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

cb = l_cb_dlnm[[1]]
cox_freq = coxph(Surv(enter, stop, status) ~ strata(trans) + position + age + cb +
               jump_height_max + match + t_prevmatch,  data = d_multistate1)
summary(cox_freq)
AIC(cox_freq)

# predicted values
n_trans = max(transmat, na.rm = TRUE)
trans_vec = 1:n_trans
d_preddate =  tibble(
  strata = trans_vec,
  age = rep(30, n_trans),
  jump_height_max = rep(86, n_trans),
  match = as.factor(rep(0, n_trans)),
  id_player = rep(1, n_trans),
  position = rep("Setter", n_trans),
  t_prevmatch = rep(6, n_trans),
  cb = cb[1:n_trans,]
)

ms_freq = msfit(object = cox_freq, newdata = d_preddate, trans = transmat)
d_mstate_preds = ms_freq$Haz %>% tibble()
plot(ms_freq, lwd = 2)

#----------------------------------Fewer states-------------------------------------------

# to look at the crosspred for DLNM we are going to look at fewer states
# from asymptomatic to any symptoms
# and from substantial to any other level
d_strata = d_kneelevels  %>% 
  select(d_imp, id_player, season, date, inj_knee_filled, jumps_n, all_of(conf_cols))

# fixme! better missing solution.
d_strata = d_strata %>% group_by(d_imp, id_player, season) %>% fill(inj_knee_filled, .direction = "downup") 
d_strata = d_strata %>% mutate(from = lag(inj_knee_filled)) %>% fill(from, .direction = "up")  %>% ungroup()

# find moments of transitions 
# find the number of days until the transition
d_strata = d_strata %>% 
  mutate(status = ifelse(lag(inj_knee_filled) == 0 & inj_knee_filled == 1, 1, 0),
         status = ifelse(is.na(status), 0, status)) %>% 
  group_by(d_imp, id_player, season) %>% 
  mutate(day = 1:n()) %>% 
   ungroup()

# find start and stop times
d_surv = d_strata %>% group_by(d_imp, id_player, season) %>% 
  rename(stop = day) %>% 
  mutate(enter = lag(stop),
         enter = ifelse(is.na(enter), 0, enter)) %>% ungroup()

# select again for easier analysis
d_surv = d_surv %>% 
  select(d_imp, id_player, season, date, enter, stop, status, inj_knee_filled, jumps_n, all_of(conf_cols)) 
d_surv = d_surv %>% mutate(preseason = as.factor(preseason),
                           position = as.factor(position),
                           match = as.factor(match))

# add event id to calc Q matrix
d_asympt = d_surv %>% add_event_id(status)
d_asympt = d_asympt %>% mutate(id_dlnm = paste0(id_player, "-", season, "-", id_event))

# calc Q matrix jump frequency
l_asympt = (d_asympt %>% group_by(d_imp) %>% nest())$data
d_asympt1 = d_asympt %>% filter(d_imp == 1)

l_tl_hist_asympt = l_asympt %>% map(. %>% select(id_player, season, id_dlnm, jumps_n, stop) %>% 
                                   arrange(stop, id_dlnm))
l_tl_hist_spread_day_asympt = 
  l_tl_hist_asympt %>% map(. %>% pivot_wider(names_from = stop, values_from = jumps_n)  %>% 
                      group_by(id_player, season) %>% 
                      fill(where(is.numeric), .direction = "downup") %>% ungroup() %>% 
                      select(-id_dlnm, -id_player, -season) %>% as.matrix)

# calc Q matrices
l_q_mat_asympt = map2(.x = l_tl_hist_asympt,
               .y = l_tl_hist_spread_day_asympt, 
               ~calc_q_matrix(.y, .x$id_dlnm, .x$stop))

# subjectively placed knots
# since the data is so skewed
# with sparse data >200 jumps on a day
# just check 
# ting = d_analysis %>% filter(d_imp == 1)
# hist(ting$jumps_n)
l_cb_asympt = l_q_mat_asympt %>% map(~crossbasis(., lag=c(lag_min, lag_max), 
                                        argvar = list(fun="ns", knots = c(10, 100, 150)),
                                        arglag = list(fun="poly", degree = 2)))

# same for jump height
# ting = d_analysis %>% filter(d_imp == 1)
# hist(ting$jump_height_sum_perc)
l_tl_hist_asympt_height = l_asympt %>% map(. %>% select(id_player, season, id_dlnm, jump_height_sum_perc, stop) %>% 
                                      arrange(stop, id_dlnm))
l_tl_hist_spread_day_asympt_height = 
  l_tl_hist_asympt_height %>% map(. %>% pivot_wider(names_from = stop, values_from = jump_height_sum_perc)  %>% 
                             group_by(id_player, season) %>% 
                             fill(where(is.numeric), .direction = "downup") %>% ungroup() %>% 
                             select(-id_dlnm, -id_player, -season) %>% as.matrix)

# calc Q matrices
l_q_mat_asympt_height = map2(.x = l_tl_hist_asympt_height,
                      .y = l_tl_hist_spread_day_asympt_height, 
                      ~calc_q_matrix(.y, .x$id_dlnm, .x$stop))

# subjectively placed knots
# since the data is so skewed
# with sparse data >200 jumps on a day
# just check 
# ting = d_analysis %>% filter(d_imp == 1)
# hist(ting$jumps_n)
l_cb_asympt_height = l_q_mat_asympt_height %>% map(~crossbasis(., lag=c(lag_min, lag_max), 
                                                 argvar = list(fun="ns", knots = c(20, 50, 80)),
                                                 arglag = list(fun="poly", degree = 2)))

cb_asympt = l_cb_asympt[[1]]
cox_asympt = coxme(Surv(enter, stop, status) ~ position + age + cb_asympt +
                   jump_height_max + match + t_prevmatch + (1|id_player),  data = d_asympt1, 
                   subset=(jumps_n!=0))
summary(cox_asympt)
AIC(cox_asympt)

l_cox_asympt = 
  map2(.x = l_asympt,
       .y = l_cb_asympt,
       ~coxme(Surv(enter, stop, status) ~ position + age + .y +
                jump_height_max + match + t_prevmatch + (1|id_player), 
              data = .x, 
              subset=(jumps_n!=0)))

AIC(l_cox_asympt[[1]])

cb_asympt_height = l_cb_asympt_height[[1]]
cox_asympt_height = coxme(Surv(enter, stop, status) ~ position + age + cb_asympt_height + 
                            match + weight + season + (1|id_player),  data = d_asympt1)

l_cox_asympt_height = 
  map2(.x = l_asympt,
       .y = l_cb_asympt_height,
       ~coxme(Surv(enter, stop, status) ~ position + age + .y + 
                match + weight + season + (1|id_player), 
              data = .x, 
              subset=(jumps_n!=0)))
summary(cox_asympt_height)
AIC(cox_asympt_height)

#----------------------------------figures-----------------------------------------------

library(lmisc) # loading local package for figure settings
# shared figure options
text_size = 14
ostrc_theme =  theme(panel.border = element_blank(), 
                     panel.background = element_blank(),
                     panel.grid = element_blank(),
                     axis.line = element_line(color = nih_distinct[4]),
                     strip.background = element_blank(),
                     strip.text.x = element_text(size = text_size, family="Trebuchet MS", colour="black", face = "bold", hjust = -0.01),
                     axis.ticks = element_line(color = nih_distinct[4]))

trans_names = c("Asymptomatic -> symptomatic",
  "Asymptomatic -> substantial",
  "Symptomatic -> asymptomatic",
  "Symptomatic -> substantial",
  "Substantial -> asymptomatic",
  "Substantial -> symptomatic")

col_vec = c(nih_distinct[1:4], "orchid", "darkgrey")
d_mstate_preds = d_mstate_preds %>% mutate(trans_fac = factor(trans, labels = trans_names, levels = 1:6))
text_size = 16
plot_cumhaz_transitions_freq = ggplot(d_mstate_preds, 
       aes(x = time, y = Haz, group = trans_fac, color = trans_fac)) +
  geom_step(size = 0.75) +
  ylab("Cumulative hazard") +
  xlab("Time") +
  scale_color_manual(values=col_vec) +
  theme_line(text_size) +
  ostrc_theme 

devEMF::emf("cumhaz_transitions_freq.emf", height = 6, width = 10)
plot_cumhaz_transitions_freq
dev.off()

# vector of tl values used in visualizations of predictions
lag_seq = lag_min:lag_max 
predvalues_freq = seq(min(d_analysis$jumps_n), 250, 10)
pred_values_height = seq(min(d_analysis$jump_height_sum_perc), max(d_analysis$jump_height_sum_perc), 10)

# predict hazards for jump frequency and jump height
l_cp_preds_asympt_freq = 
  map2(.x = l_cox_asympt,
       .y = l_cb_asympt,
       ~crosspred(.y, .x, at = predvalues_freq, cen = 0, cumul = TRUE))

l_cp_preds_asympt_height = 
  map2(.x = l_cox_asympt_height,
       .y = l_cb_asympt_height,
       ~crosspred(.y, .x, at = pred_values_height, cen = 0, cumul = TRUE))

# function for plucking the right matrix out of the crosspred list within the list of crosspred lists
pluck_mat = function(l_crosspred, x, pos){pluck(l_crosspred, x, pos)}
# function for fetching the predictions per model, than averaging the results
fetch_matrix = function(l_crosspred, all = TRUE, lag_fixed){
  
  # different position for cumulative effect and non-cumulative effect
  if(all){
    pos = 9
    pos_low = 15
    pos_high = 16
  } else {
    pos = 7
    pos_low = 13
    pos_high = 14
  }
  
  # obtain estimate
  d_preds1 = pluck_mat(l_crosspred, 1, pos)
  d_preds2 = pluck_mat(l_crosspred, 2, pos)
  d_preds3 = pluck_mat(l_crosspred, 3, pos)
  d_preds4 = pluck_mat(l_crosspred, 4, pos)
  d_preds5 = pluck_mat(l_crosspred, 5, pos)
  l_fit = list(d_preds1, d_preds2, d_preds3, d_preds4, d_preds5)
  # average across preds
  mat_fit = reduce(l_fit, `+`) / length(l_fit)
  
  # conflow
  d_preds_low1 = pluck_mat(l_crosspred, 1, pos_low)
  d_preds_low2 = pluck_mat(l_crosspred, 2, pos_low)
  d_preds_low3 = pluck_mat(l_crosspred, 3, pos_low)
  d_preds_low4 = pluck_mat(l_crosspred, 4, pos_low)
  d_preds_low5 = pluck_mat(l_crosspred, 5, pos_low)
  l_low = list(d_preds_low1, d_preds_low2, d_preds_low3, d_preds_low4, d_preds_low5)
  # average across preds
  mat_low = reduce(l_low, `+`) / length(l_low)
  
  # confhigh
  d_preds_high1 = pluck_mat(l_crosspred, 1, pos_high)
  d_preds_high2 = pluck_mat(l_crosspred, 2, pos_high)
  d_preds_high3 = pluck_mat(l_crosspred, 3, pos_high)
  d_preds_high4 = pluck_mat(l_crosspred, 4, pos_high)
  d_preds_high5 = pluck_mat(l_crosspred, 5, pos_high)
  l_high = list(d_preds_high1, d_preds_high2, d_preds_high3, d_preds_high4, d_preds_high5)
  # average across preds
  mat_high = reduce(l_high, `+`) / length(l_high)

  # return data
  if(all){
  d_pred = enframe(mat_fit, name = "predvalue")  %>% 
    mutate(ci_low = mat_low, ci_high = mat_high, predvalue = as.numeric(predvalue))  %>% 
    rename(coef = value)
  } else {
    colnumber = which(colnames(mat_fit) == lag_fixed)
    predvalues = enframe(mat_fit, name = "predvalue") %>% select(predvalue)
    d_pred = as_tibble(mat_fit[,colnumber]) %>% 
      rename(coef = value) %>% 
      mutate(predvalue = as.numeric(predvalues$predvalue),
             ci_low = mat_low[,colnumber],
             ci_high = mat_high[,colnumber])
  }
  d_pred
}

d_asympt_preds_freq_cumul = fetch_matrix(l_cp_preds_asympt_freq)
d_asympt_preds_freq_lag0 = fetch_matrix(l_cp_preds_asympt_freq, all = FALSE, "lag0")
d_asympt_preds_height_cumul = fetch_matrix(l_cp_preds_asympt_height)
d_asympt_preds_height_lag0 = fetch_matrix(l_cp_preds_asympt_height, all = FALSE, "lag0")

ggplot(d_asympt_preds_freq_cumul, aes(x = predvalue, y = coef, group = 1)) +
  geom_ribbon(aes(min = ci_low, max = ci_high), alpha = 0.3, fill = nih_distinct[1]) +
  geom_hline(yintercept = 1, alpha = 0.3, size = 1) +
  geom_line(size = 0.75, color = nih_distinct[4]) +
  theme_base(text_size) +
  ostrc_theme +
  xlab("Daily number of jumps") +
  ylab("Cumulative HR on Day 0") 


ggplot(d_asympt_preds_freq_lag0, aes(x = predvalue, y = coef, group = 1)) +
  geom_hline(yintercept = 1, alpha = 0.3, size = 1) +
  geom_ribbon(aes(min = ci_low, max = ci_high), alpha = 0.3, fill = nih_distinct[1]) +
  geom_line(size = 0.75, color = nih_distinct[4]) +
  theme_base(text_size) +
  ostrc_theme +
  xlab("Daily number of jumps") +
  ylab("HR on Day 0")

ggplot(d_asympt_preds_height_cumul, aes(x = predvalue, y = coef, group = 1)) +
  geom_ribbon(aes(min = ci_low, max = ci_high), alpha = 0.3, fill = nih_distinct[1]) +
  geom_hline(yintercept = 1, alpha = 0.3, size = 1) +
  geom_line(size = 0.75, color = nih_distinct[4]) +
  theme_base(text_size) +
  ostrc_theme +
  xlab("Daily sum of jump heights") +
  ylab("Cumulative HR on Day 0") 


ggplot(d_asympt_preds_height_lag0, aes(x = predvalue, y = coef, group = 1)) +
  geom_hline(yintercept = 1, alpha = 0.3, size = 1) +
  geom_ribbon(aes(min = ci_low, max = ci_high), alpha = 0.3, fill = nih_distinct[1]) +
  geom_line(size = 0.75, color = nih_distinct[4]) +
  theme_base(text_size) +
  ostrc_theme +
  xlab("Daily sum of jumps") +
  ylab("HR on Day 0")



