library(tidyverse) # data wrangling
library(dlnm) # distributed lag non-linear models
library(lme4)
library(lubridate) # to manipulate dates
library(splines) # natural splines

# so we don't have to deal with scientific notations
# and strings aren't automatically read as factors:
options(scipen = 30, 
        stringsAsFactors = FALSE)

data_folder = "O:\\Prosjekter\\Bache-Mathiesen-Biostatistikk\\Data\\volleyball\\"
d_jumpload = readRDS(paste0(data_folder, "d_jumpload_multimputed_daily.rds"))

# define key columns
key_cols = c("date", "id_player", "id_team", "id_team_player", "id_season")
conf_cols = c("age", "jump_height_max", "position", 
              "match", "t_prevmatch", "jumps_n_weekly", "jumps_height_weekly", "preseason", "jump_height_sum", 
              "jump_height_perc_sum","weight", "jumps_n")

# define the min and max lag
# we will lag the data, so lag 0
# corresponds to the last day before the OSTRC week
lag_min = 0
lag_max = 20

# select columns that may be useful in analyses
d_analysis = d_jumpload %>% 
  select(all_of(key_cols), 
         jump_height_sum,
         jumps_n,
         jumps_n_weekly,
         jumps_height_weekly,
         jump_height_max,
         jump_height_perc_sum,
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

# from asymptomatic to any symptoms
d_analysis_selected = d_analysis  %>% 
  select(all_of(key_cols), d_imp, id_player, season, date, inj_knee_filled, all_of(conf_cols))

# imputed missing outcome data - for now. This is only temprorary while we calculate DLNM.
# we will return the missing values later.
d_strata = d_analysis_selected %>% group_by(d_imp, id_player, season) %>% 
  fill(inj_knee_filled, .direction = "downup") 

# denote the day number up until end of player study period
d_strata = d_strata %>% 
  group_by(d_imp, id_player, season) %>% 
  mutate(day = 1:n()) %>% 
  ungroup()

# find start and stop times
d_surv = d_strata %>% group_by(d_imp, id_player, season) %>% 
  rename(stop = day) %>% 
  mutate(enter = lag(stop),
         enter = ifelse(is.na(enter), 0, enter)) %>% ungroup()

# make sure category data with number codes is treated as factors
d_surv = d_surv %>% mutate(preseason = as.factor(preseason),
                           position = as.factor(position))

# add event id so that we may calculate the Q matrix per event
# we also need variable that we may stratify on
# the intervals in which the player is at risk (i.e. not while they already have symptoms)
d_surv = d_surv %>% group_by(d_imp, id_player, season) %>% 
                    mutate(status = ifelse(lag(inj_knee_filled) == 0 & 
                                             inj_knee_filled == 1, 1, 0),
                           status = ifelse(is.na(status), 0, status)) %>% ungroup()
d_surv = d_surv %>% add_event_id(status)

d_surv = d_surv %>% mutate(id_dlnm = paste0(id_player, "-", season, "-", id_event),
                           inj_knee_filled_fixed = ifelse(status == 1, 2, inj_knee_filled))

# labelling intervals as being from asympt to sympt or sympt to asympt

# d_surv %>% group_by(d_imp, id_player) %>% 
#   mutate(interval_type = 
#                     case_when(status == 1 & inj_knee_filled == 1 & 
#                              lag(inj_knee_filled) == 0 ~ "asymptomatic",
#                              status == 1 & inj_knee_filled == 0 & 
#                              lag(inj_knee_filled) == 1 ~ "symptomatic",
#                              TRUE ~ NA_character_)) %>% 
#   filter(d_imp == 1, id_player == 1) %>% View()


# we have to remove intervals of pain before starting a new
# interval with week = 1
# but, the final week needs the weekly sum from the last date, not the first
# this is why we calculate the lead for this week
d_weekly = d_surv %>%
  select(d_imp, all_of(key_cols), id_event, id_dlnm, date, season, jumps_height_weekly, 
         jump_height_perc_sum, status, inj_knee_filled, all_of(conf_cols), stop) %>% 
  group_by(d_imp, id_player) %>%
  mutate(jumps_height_weekly_lead = lead(jumps_height_weekly, 6)) %>%
  ungroup() %>%
  filter(!(status == 0 & inj_knee_filled == 1)) %>% 
  group_by(d_imp, id_dlnm) %>% mutate(week = difftime(max(date)+7, date, units = "weeks"),
                                week = as.numeric(round(rev(week))),
                                day = 1:n()) %>% ungroup()

d_weekly = d_weekly %>% mutate(id_player = as.character(id_player))
d_weekly = d_weekly %>% select(-stop)


# model without DLNM

library(lme4) # for mixed models
fit1 = glmer(status ~ ns(week, 4) + ns(jumps_height_weekly, 3) + season + position + age + weight + (1|id_player), 
             data = d_weekly %>% filter(d_imp == 1),
             family=binomial(link="cloglog"))
parameters::parameters(fit1, exponentiate = TRUE)

AIC(fit1)


#------------------------------------- DLNM with the missing data method-----------------


# fixme!
# how should weekly load be assessed?
# jumps_height_weekly = ifelse(status == 1,
#                             jumps_height_weekly_lead, jumps_height_weekly)
d_weekly = d_surv %>%
  select(d_imp, all_of(key_cols), id_event, id_dlnm, date, season, jumps_height_weekly, 
         jump_height_perc_sum, status, inj_knee_filled, all_of(conf_cols), stop) %>% 
  group_by(d_imp, id_player) %>%
  mutate(jumps_height_weekly_lead = lead(jumps_height_weekly, 6)) %>%
  ungroup() 

d_weekly = d_weekly %>% mutate(id_dlnm2 = paste0(id_player, "-", season))
d_weekly = d_weekly %>% mutate(id_player = as.character(id_player))
d_weekly = d_weekly %>% select(-stop)

d_index = d_weekly %>% distinct(id_dlnm2) %>% mutate(index = 1:n())
d_weekly = d_weekly %>% left_join(d_index, by = "id_dlnm2") %>% select(-id_dlnm2) %>% rename(id_dlnm2 = index)


d_weekly = d_weekly %>%
  group_by(d_imp, id_dlnm2) %>% mutate(day = 1:n()) %>% ungroup()

# make DLNM cross basis
# since we have symptoms at the weekly level, and training at the daily level
# we will only consider the training that happened before that week
# at the daily level
d_weekly = d_weekly %>% 
  group_by(d_imp, id_dlnm2) %>% 
  mutate(daily_jump_lag = lag(jump_height_perc_sum),
         daily_jump_n_lag = lag(jumps_n),
         daily_jump_h_lag = lag(jump_height_sum)) %>% ungroup()

# remember now that 0 is the day before the symptoms-week
# we want to look 21 days into the past, that is day number 20
lag_min = 0
lag_max = 20

# find start and stop times
d_weekly = d_weekly %>% group_by(d_imp, id_dlnm2) %>% 
  rename(stop = day) %>% 
  mutate(enter = lag(stop),
         enter = ifelse(is.na(enter), 0, enter)) %>% ungroup()

l_weekly = (d_weekly %>% group_by(d_imp) %>% nest())$data
l_tl_hist = l_weekly %>% map(. %>% select(id_player, season, id_dlnm2, daily_jump_lag, stop) %>% 
                               arrange(id_dlnm2, stop))
l_tl_hist_spread_day = 
  l_tl_hist %>% map(. %>% pivot_wider(names_from = stop, values_from = daily_jump_lag)  %>% 
                      group_by(id_dlnm2) %>% 
                      fill(where(is.numeric), .direction = "downup") %>% ungroup() %>% 
                      select(-id_dlnm2, -id_player, -season) %>% as.matrix)

# for each individual, for each of these exit times, we will extract the exposure history 
# for the given lag-time which we are interested in
# This is called the Q-matrix. The Q-matrix should be nrow(dataspl) X 0:lag_max dimensions.
l_q_mat = list()
for(i in 1:length(l_tl_hist)){

  l_q_mat[[i]] = map2(.x = l_tl_hist[[i]]$id_dlnm2, 
              .y = l_tl_hist[[i]]$stop, 
              ~exphist(l_tl_hist_spread_day[[i]][.x,], .y, c(lag_min, lag_max))) %>% 
    do.call("rbind", .)
}

# subjectively placed knots
# since the data is so skewed
# hist(d_weekly$daily_jump_lag)
l_cb_dlnm = l_q_mat %>% map(~crossbasis(., lag=c(lag_min, lag_max), 
                                        argvar = list(fun="ns", knots = c(2500, 5000, 9000)),
                                        arglag = list(fun="poly", degree = 2)))

# to ensure that he countdown for number of weeks 
# starts from the first symptom-free week, 
# OR at the beginning of a season, given that the season starts symptom free
d_symptomfree_weeks = d_weekly %>% group_by(d_imp, id_dlnm) %>% 
  filter(!(status == 0 & inj_knee_filled == 1)) %>% 
  mutate(week = difftime(max(date)+7, date, units = "weeks"),
           week = as.numeric(round(rev(week))),
           day = 1:n()) %>% 
           ungroup() %>% 
           select(d_imp, id_dlnm, date, week)

d_weekly_j = d_weekly %>% left_join(d_symptomfree_weeks, by = c("d_imp", "id_dlnm", "date"))

# we imputed injuries to make it easier to calculate the DLNM
# but we won't impute the missing data in our final analysis
# we remove the imputed values
pos_missing = which(is.na(d_analysis_selected$inj_knee_filled))
d_weekly_j = d_weekly_j %>%
  mutate(status = case_when(row_number() %in% pos_missing ~ NA_real_, TRUE ~ status)) 

# now ensure that weekly values during symptom-weeks are missing data
# so these rows will automatically be removed in the analysis
d_weekly_j = d_weekly_j %>% mutate(jumps_height_weekly = 
                                 case_when(inj_knee_filled == 1 & 
                                             status == 0 ~ NA_real_,
                                                     TRUE ~ jumps_height_weekly),
                                 age = 
                                   case_when(inj_knee_filled == 1 & 
                                               status == 0 ~ NA_real_,
                                             TRUE ~ age))

cb_dlnm_1 = l_cb_dlnm[[1]]
fit2 = glmer(status ~ ns(week, 3) + ns(jumps_height_weekly, 3) + cb_dlnm_1 + 
               season + position + age + weight + (1|id_player), 
             data = d_weekly_j %>% filter(d_imp == 1),
             family=binomial(link="cloglog"))
parameters::parameters(fit2, exponentiate = TRUE)

fit3 = glmer(status ~ ns(week, 4) + cb_dlnm_1 + 
               season + position + age + weight + (1|id_player), 
             data = d_weekly_j %>% filter(d_imp == 1),
             family=binomial(link="cloglog"))
parameters::parameters(fit3, exponentiate = TRUE)

AIC(fit2)
AIC(fit3)


#---------------------------------------------- splitting jump frequency and jump height

l_tl_hist_n = l_weekly %>% map(. %>% select(id_player, season, id_dlnm2, daily_jump_n_lag, stop) %>% 
                               arrange(id_dlnm2, stop))
l_tl_hist_spread_day_n = 
  l_tl_hist_n %>% map(. %>% pivot_wider(names_from = stop, values_from = daily_jump_n_lag)  %>% 
                      group_by(id_dlnm2) %>% 
                      fill(where(is.numeric), .direction = "downup") %>% ungroup() %>% 
                      select(-id_dlnm2, -id_player, -season) %>% as.matrix)

# for each individual, for each of these exit times, we will extract the exposure history 
# for the given lag-time which we are interested in
# This is called the Q-matrix. The Q-matrix should be nrow(dataspl) X 0:lag_max dimensions.
l_q_mat_n = list()
for(i in 1:length(l_tl_hist_n)){
  
  l_q_mat_n[[i]] = map2(.x = l_tl_hist_n[[i]]$id_dlnm2, 
                      .y = l_tl_hist_n[[i]]$stop, 
                      ~exphist(l_tl_hist_spread_day_n[[i]][.x,], .y, c(lag_min, lag_max))) %>% 
    do.call("rbind", .)
}

# subjectively placed knots
# since the data is so skewed
# hist(d_weekly$daily_jump_n_lag)
l_cb_dlnm_n = l_q_mat_n %>% map(~crossbasis(., lag=c(lag_min, lag_max), 
                                        argvar = list(fun="ns", knots = c(50, 80, 120)),
                                        arglag = list(fun="poly", degree = 2)))


#---- height

l_tl_hist_h = l_weekly %>% map(. %>% select(id_player, season, id_dlnm2, daily_jump_h_lag, stop) %>% 
                                 arrange(id_dlnm2, stop))
l_tl_hist_spread_day_h = 
  l_tl_hist_h %>% map(. %>% pivot_wider(names_from = stop, values_from = daily_jump_h_lag)  %>% 
                        group_by(id_dlnm2) %>% 
                        fill(where(is.numeric), .direction = "downup") %>% ungroup() %>% 
                        select(-id_dlnm2, -id_player, -season) %>% as.matrix)

# for each individual, for each of these exit times, we will extract the exposure history 
# for the given lag-time which we are interested in
# This is called the Q-matrix. The Q-matrix should be nrow(dataspl) X 0:lag_max dimensions.
l_q_mat_h = list()
for(i in 1:length(l_tl_hist_h)){
  
  l_q_mat_h[[i]] = map2(.x = l_tl_hist_h[[i]]$id_dlnm2, 
                        .y = l_tl_hist_h[[i]]$stop, 
                        ~exphist(l_tl_hist_spread_day_h[[i]][.x,], .y, c(lag_min, lag_max))) %>% 
    do.call("rbind", .)
}

# subjectively placed knots
# since the data is so skewed
# hist(d_weekly$daily_jump_h_lag)
l_cb_dlnm_h = l_q_mat_h %>% map(~crossbasis(., lag=c(lag_min, lag_max), 
                                            argvar = list(fun="ns", knots = c(2500, 5000, 8000)),
                                            arglag = list(fun="poly", degree = 2)))

cb_dlnm_n_1 = l_cb_dlnm_n[[1]]
fit2 = glmer(status ~ ns(week, 3) + ns(jumps_height_weekly, 3) + cb_dlnm_n_1 + 
               season + position + age + (1|id_player), 
             data = d_weekly_j %>% filter(d_imp == 1),
             family=binomial(link="cloglog"))
parameters::parameters(fit2, exponentiate = TRUE)

cb_dlnm_h_1 = l_cb_dlnm_h[[1]]
fit2 = glmer(status ~ ns(week, 3) + ns(jumps_height_weekly, 3) + cb_dlnm_h_1 + 
               season + position + age + (1|id_player), 
             data = d_weekly_j %>% filter(d_imp == 1),
             family=binomial(link="cloglog"))
parameters::parameters(fit2, exponentiate = TRUE)


#-------------------------- run cloglog regression


# make DLNM cross basis
# since we have symptoms at the weekly level, and training at the daily level
# we will only consider the training that happened before that week
# at the daily level
d_weekly = d_weekly %>% 
  group_by(d_imp, id_dlnm) %>% 
  mutate(daily_jump_lag = lag(jump_height_perc_sum)) %>% ungroup()


d_weekly %>% filter(d_imp == 1, id_player == 1) %>% View()

# remember now that 0 is the day before the symptoms-week
# we want to look 35 days into the past, that is day number 34
lag_min = 0
lag_max = 34


# find start and stop times
d_weekly = d_weekly %>% group_by(d_imp, id_dlnm) %>% 
  rename(stop = day) %>% 
  mutate(enter = lag(stop),
         enter = ifelse(is.na(enter), 0, enter)) %>% ungroup()

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

l_weekly = (d_weekly %>% group_by(d_imp) %>% nest())$data
d_weekly = d_weekly %>% filter(d_imp == 1)

l_tl_hist = l_weekly %>% map(. %>% select(id_player, season, id_dlnm, daily_jump_lag, stop) %>% 
                                   arrange(id_dlnm, stop))
l_tl_hist_spread_day = 
  l_tl_hist %>% map(. %>% pivot_wider(names_from = stop, values_from = daily_jump_lag)  %>% 
                      group_by(id_dlnm) %>% 
                      fill(where(is.numeric), .direction = "downup") %>% ungroup() %>% 
                      select(-id_dlnm, -id_player, -season) %>% as.matrix)

# finding the Q matrix for DLNM
# Don't worry if there are any missing data (NA) in the past
# DLNM will model based on the data available
l_q_mat = map2(.x = l_tl_hist,
               .y = l_tl_hist_spread_day, 
               ~calc_q_matrix(.y, .x$id_dlnm, .x$stop))

# subjectively placed knots
# since the data is so skewed
# hist(d_weekly$daily_jump_lag)
l_cb_dlnm = l_q_mat %>% map(~crossbasis(., lag=c(lag_min, lag_max), 
                                        argvar = list(fun="ns", knots = c(2500, 5000, 9000)),
                                        arglag = list(fun="ns", knots = 3)))

library(lme4) # for mixed models
fit1 = glmer(status ~ ns(week, 4) + ns(jumps_height_weekly, 3) + season + position + age + (1|id_player), 
             data = d_weekly %>% filter(d_imp == 1),
             family=binomial(link="cloglog"))
parameters::parameters(fit1, exponentiate = TRUE)

AIC(fit1)

cb_dlnm_1 = l_cb_dlnm[[1]]
fit2 = glmer(status ~ ns(week, 4) + cb_dlnm_1 + season + position + age + (1|id_player), 
             data = d_weekly %>% filter(d_imp == 1),
             family=binomial(link="cloglog"))
parameters::parameters(fit2, exponentiate = TRUE)

AIC(fit2)





d_weekly_dist = d_weekly %>% group_by(d_imp, id_dlnm) %>% arrange(id_event, desc(date)) %>% 
  distinct(d_imp, id_dlnm, week, .keep_all = TRUE) %>% arrange(d_imp, id_dlnm, week) %>% 
  mutate(id_player = as.character(id_player),
         jumps_height_weekly = ifelse(status == 1,
                                      jumps_height_weekly_lead, jumps_height_weekly)) %>%
          ungroup()

#------------------------------------------- discrete time analysis
library(lme4)
fit1 = glmer(status ~ ns(week, 4) + ns(jumps_height_weekly, 3) + season + position + age + (1|id_player), 
             data = d_weekly_dist %>% filter(d_imp == 1),
             family=binomial(link="cloglog"))
parameters::parameters(fit1, exponentiate = TRUE)

AIC(fit1)


library(ggeffects)
library(sjPlot)
preds_jumph = ggpredict(
    fit1, "jumps_height_weekly [all]", 
    condition = c(age = 26.1, position = "Outside", id_player = "1", season = "2019/2020", week = 3),
    vcov.fun = "vcovCR", 
    vcov.type = "CR0", 
    vcov.args = list(id_player = unique((d_weekly_dist %>% filter(d_imp == 1))$id_player)),
    type = "re.zi") %>% as_tibble()
  
ggplot(preds_jumph, aes(x = x, y = predicted, group = 1)) + 
  geom_line()
  

preds_week = ggpredict(
  fit1, "week [all]", 
  condition = c(age = 26.1, position = "Outside", id_player = "1", season = "2019/2020", jumps_height_weekly = 2724),
  vcov.fun = "vcovCR", 
  vcov.type = "CR0", 
  vcov.args = list(id_player = unique((d_weekly_dist %>% filter(d_imp == 1))$id_player)),
  type = "re.zi") %>% as_tibble()

ggplot(preds_week, aes(x = x, y = predicted, group = 1)) + 
  geom_line()

#------------- With DLNM

d_weekly %>% select(d_imp, date, id_player, id_event, season, status, jump_height_perc_sum, jumps_height_weekly, week)


d_weekly_dist

















lag_min = 0
lag_max = 2


hist(d_weekly_dist$jumps_height_weekly)

# finding the Q matrix for DLNM
# Don't worry if there are any missing data (NA) in the past
# DLNM will model based on the data available
q_mat = tsModel::Lag((d_weekly_dist %>% filter(d_imp == 1))$jumps_height_weekly, lag_min:lag_max)

# calculate crossproducts for DLNM
# using subjectively chosen locations for knots
# based on the range of the data (hist(d_daily_1$exposure_daily))
cb_past_exposure = crossbasis(q_mat, lag=c(lag_min, lag_max), 
                              argvar = list(fun="ns", knots = 3),
                              arglag = list(fun="ns", knots = 3))



fit_chronic = glmer(status ~ ns(week, 4) +  season + position + age + (1|id_player), 
             data = d_weekly_dist %>% filter(d_imp == 1),
             family=binomial(link="cloglog"))
parameters::parameters(fit_chronic, exponentiate = TRUE)





#----------------------- to make survival graph?

general <- glm(EVENT ~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 - 1, family = "binomial")
fits <- c()
survivor.quad = 1
survivor.gen = 1
for (i in 1:9){
  constant = order0$coef[1]
  linear = order1$coef[1] + order1$coef[2]*i
  quadratic = order2$coef[1] + order2$coef[2]*i + order2$coef[3]*i**2
  cubic = order3$coef[1] + order3$coef[2]*i + order3$coef[3]*i**2 + order3$coef[4]*i**3
  hazard.quad = 1/(1 + exp(-quadratic));
  survivor.quad = (1 - hazard.quad)*survivor.quad;
  generalval = general$coef[i]
  hazard.gen = 1/(1 + exp(-generalval));
  survivor.gen = (1 - hazard.gen)*survivor.gen;
  z <- c(i, constant, linear, quadratic, cubic, generalval, hazard.quad, survivor.quad, hazard.gen, survivor.gen)
  fits <- rbind(fits, z)
}

par(mfrow=c(1,1))
plot(fits[,1], fits[,2], type = "l", lty = 1, col="black",
       + xlim = c(0,9), ylim = c(-6,0), xlab = "Years after hire", ylab = "Fitted logit(hazard)")
points(fits[,1], fits[,3], type = "l", lty = 2,col="green")
points(fits[,1], fits[,4], type = "l", lty = 3,col="blue")
points(fits[,1], fits[,5], type = "l", lty = 4,col="brown")
points(fits[,1], fits[,6], type = "l", lty = 5,col="red")
legend("bottomright", c("constant", "linear", "quadratic", "cubic",
                          + "general"), lty = c(1, 2, 3, 4, 5),col=c("black","green","blue","brown","red"))

# survival function
plot(fits[,1], fits[,8], type = "l", lty = 1, col="blue",
       + xlim = c(0,9), ylim = c(0,1), xlab = "Years after hire", ylab = "Fitted survival")
points(fits[,1], fits[,10], type = "l", lty = 2,col="red")
legend("bottomright", c("quadratic","general"), lty = c(1, 2),col=c("blue","red"))
