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
  select(d_imp, id_player, date, knee_total_filled, knee_state, jumps_n, all_of(conf_cols))

# we will find each event interval and give it an ID.
# this so we can find the follow up time per event
d_selected = d_selected %>% 
  mutate(diff_prev = knee_state-lag(knee_state),
         change_missing = ifelse(!is.na(knee_state) & is.na(diff_prev), 1, 0),
         change = ifelse(diff_prev != 0 | change_missing == 1, 1, 0),
         change = ifelse(is.na(change), 0, change)) %>% 
  group_by(d_imp, id_player) %>% 
  mutate(day = 1:n(),
         Fup = ifelse(change == 1, day, NA)) %>% fill(Fup, .direction = "up") %>% ungroup()  %>% 
  mutate(status = ifelse(is.na(knee_state), 0, knee_state),
         time = 1)



#-----------------------no idea where i am at this point

# find start and stop times
d_surv = d_selected %>% group_by(d_imp, id_player) %>% 
  rename(stop = day) %>% 
  mutate(start = lag(stop),
         start = ifelse(is.na(start), 0, start)) %>% ungroup()

d_surv = d_surv %>% 
  select(d_imp, id_player, date, start, stop, knee_state, Fup, jumps_n) 



# fixme! better missing solution.
d_test1 = d_surv %>% group_by(id_player) %>% fill(knee_state, .direction = "downup") %>% ungroup()


d_test2 = d_test1 %>% mutate(from = lag(knee_state))


d_from1 = d_test2  %>% 
  filter(from == 1) %>% 
  group_by(d_imp, id_player) %>% 
                  mutate(to = as.character(2),
                         status = ifelse(knee_state == 2, 1, 0),
                         status = ifelse(is.na(status), 0, status))

d_from2 = d_test2  %>% 
  filter(from == 2) %>% 
  group_by(d_imp, id_player) %>% 
  mutate(status1 = ifelse(knee_state == 1, 1, 0),
         status1 = ifelse(is.na(status1), 0, status1),
         status3 = ifelse(knee_state == 3, 1, 0),
         status3 = ifelse(is.na(status3), 0, status3)) %>% ungroup() %>% 
 pivot_longer(cols = c("status1", "status3"), names_to = "to", values_to = "status") %>% 
  mutate(to = str_replace_all(to, "status", ""))


d_from3 = d_test2  %>% 
  filter(from == 3) %>% 
  group_by(d_imp, id_player) %>% 
  mutate(status1 = ifelse(knee_state == 1, 1, 0),
         status1 = ifelse(is.na(status1), 0, status1),
         status2 = ifelse(knee_state == 2, 1, 0),
         status2 = ifelse(is.na(status2), 0, status2)) %>% ungroup() %>% 
  pivot_longer(cols = c("status1", "status2"), names_to = "to", values_to = "status") %>% 
  mutate(to = str_replace_all(to, "status", ""))

# data in multstate format
d_test3 = bind_rows(d_from1, d_from2, d_from3) %>% arrange(d_imp, id_player, date)


library(flexsurv)
statenames = c("asymptomatic", "symptomatic", "worsening")
l_transitions = list(c(2), 
                     c(1, 3), 
                     c(1, 2))
transmat = mstate::transMat(l_transitions, statenames)

n_trans = max(transmat, na.rm = TRUE)
trans_vec = 1:n_trans
trans_vec %>% map(.x = ., ~flexsurvreg(Surv(start, stop, status) ~ jumps_n,
                                       data = subset(d_test3, status == .x),
                                       dist = "exp"))



# msm2Surv method
d_test = d_selected %>% filter(d_imp == 1) %>% 
  select(id_player, day,  Fup, status = knee_state, age, jumps_n) %>% 
  mutate(status = ifelse(is.na(status), 0, status),
         time = 1)

statenames = c("asymptomatic", "symptomatic", "worsening")


q_transitions = rbind(c(1, 1, 0), 
                      c(1, 1, 1), 
                      c(1, 1, 1))
transmat = mstate::transMat(l_transitions, statenames)

msm::msm2Surv(d_test, subject = "id_player", time = "day", state = "status", q_transitions)

###  find event intervals method


d_selected %>% filter(d_imp == 1, id_player == 5) %>% select(knee_total_filled, knee_state, status, day, Fup)

# find start and stop times
d_surv = d_selected %>% group_by(d_imp, id_player) %>% 
  rename(stop = day) %>% 
  mutate(start = lag(stop),
         start = ifelse(is.na(start), 0, start)) %>% ungroup()

d_surv = d_surv %>% 
  mutate(status = ifelse(is.na(knee_state), 0, 1)) %>% 
  select(d_imp, id_player, date, start, stop, status, knee_state, Fup, jumps_n, all_of(conf_cols)) 



# we can also assume that we are taking the "time to next symptom week"
# if they have symptoms multiple weeks in a row, that is still considered 1 symptom week
# we can call this a jumper's knee episode
no_inj_players = d_selected %>% group_by(d_imp, id_player) %>% 
  summarise(sum = sum(knee_total_filled, na.rm = TRUE)) %>% 
  ungroup() %>% filter(sum == 0) %>% distinct(id_player) %>% pull()

# fetch those with injuries
d_inj_only = d_selected %>% filter(!id_player %in% no_inj_players)
ids = d_inj_only %>% distinct(id_player) %>% pull()

d1 = d_selected %>% 
  filter(id_player == 1) %>% 
  select(d_imp, id_player, date, knee_total_filled, knee_state, status, day, Fup, jumps_n) %>% 
  mutate(from_state = lag(knee_state),
         to_state1 = ifelse(from_state == 1, 2, NA),
         to_state2 = ifelse(from_state == 2, 1, NA),
         to_state3 = ifelse(from_state == 2, 3, NA)
         ) %>% View()



d_test = d_selected %>% 
  filter(id_player == 1) %>% 
  select(d_imp, id_player, date, knee_total_filled, knee_state, status, day, Fup, jumps_n) %>% 
  group_by(d_imp, id_player) %>% 
  mutate(from_state = lag(knee_state),
         from1to2 = case_when(from_state == 1 & knee_state == 2 ~ 1, 
                              TRUE ~ 0),
         from2to1 = case_when(from_state == 2 & knee_state == 1 ~ 1,
                              TRUE ~ 0),
         from2to3 = case_when(from_state == 2 & knee_state == 3 ~ 1,
                              TRUE ~ 0),
         from3to1 = case_when(from_state == 3 & knee_state == 1 ~ 1,
                              TRUE ~ 0),
         from3to2 = case_when(from_state == 3 & knee_state == 2 ~ 1,
                              TRUE ~ 0)
  ) %>% ungroup()


d_test = d_selected %>% 
  filter(id_player == 1) %>% 
  select(d_imp, id_player, date, knee_total_filled, knee_state, status, day, Fup, jumps_n) %>% 
  group_by(d_imp, id_player) %>% 
  mutate(from_state = lag(knee_state),
         to2 = case_when(knee_state == 2 ~ 1, 
                              TRUE ~ 0),
         to1 = case_when(knee_state == 1 ~ 1,
                              TRUE ~ 0),
         to3 = case_when(knee_state == 3 ~ 1,
                              TRUE ~ 0),
         from_state = ifelse(is.na(from_state), -1, from_state)
  ) %>% ungroup()
d_test %>% View()

d_test %>% pivot_longer(d_test, 
                        cols = c("to2", "to1", "to3"), 
                        names_to = "to", values_to = "status")


# find intervals
symptoms = d1$status
skips = lead(symptoms)-symptoms
pos_trans = which(skips != 0)+1

from = c(1, pos_trans)
to = c(pos_trans, length(symptoms))

d2 = slice(d1, from[1]:to[1])

d2


event_ids = 1:length(l_data1person)
l_data1person = map2(l_data1person, event_ids, ~.x %>% mutate(id_event = .y))



# slice number of times equal to number of intervals
l1mann = replicate(length(from_rows), d1, simplify = FALSE)
l_data1person = pmap(list(as.list(from_rows), as.list(to_rows), l1mann), 
                     function(x, y, z) slice(z, x:y))
event_ids = 1:length(l_data1person)
l_data1person = map2(l_data1person, event_ids, ~.x %>% mutate(id_event = .y))

# collect list of intervals to dataset
d_1person = l_data1person %>% bind_rows()
d_1person




# function to find event intervals and append them to a dataset
find_events = function(d, id){
  d1 = d %>% filter(id_player == id)
  
  # find intervals
  pos_symptoms = which(d1$inj_knee_filled == 1)
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
d_time_to_sympt = bind_rows(d_maindata, d_analysis %>% filter(id_player %in% no_inj_players)) %>% 
  mutate(id_event = ifelse(id_player %in% no_inj_players, 1, id_event)) %>% 
  rename(not_unique_id = id_event) 

d_eventid = d_time_to_sympt %>% 
  distinct(id_player, not_unique_id) %>% mutate(id_event = 1:n())
d_time_to_sympt = d_time_to_sympt %>% 
  left_join(d_eventid, by = c("id_player", "not_unique_id")) %>% 
  select(-not_unique_id)
















# find start and stop times
d_surv = d_selected %>% group_by(d_imp, id_player) %>% 
  rename(stop = day) %>% 
  mutate(start = lag(stop),
         start = ifelse(is.na(start), 0, start)) %>% ungroup()

d_surv = d_surv %>% 
  mutate(status = ifelse(is.na(knee_state), 0, 1)) %>% 
  select(d_imp, id_player, date, start, stop, status, knee_state, Fup, jumps_n, all_of(conf_cols)) 

library(flexsurv)
crexp = flexsurvreg(Surv(start, stop, status) ~ knee_state, data = d_surv,
                     dist = "exp")


statenames = c("asymptomatic", "symptomatic", "worsening")
l_transitions = list(c(2), 
                     c(1, 3), 
                     c(1, 2))
transmat = mstate::transMat(l_transitions, statenames)
tgrid = seq(0, 50, by = 1)
mrexp = msfit.flexsurvreg(crexp, t = tgrid, trans = transmat, tvar = "knee_state")




## Simple exponential model (reduces to Markov)

bexp <- flexsurvreg(Surv(years, status) ~ trans,
                    data=bosms3, dist="exp")
tmat <- rbind(c(NA,1,2),c(NA,NA,3),c(NA,NA,NA))
mexp <- msfit.flexsurvreg(bexp, t=seq(0,12,by=0.1),
                          trans=tmat, tvar="trans", variance=FALSE)




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




#-----------------------library(mstate)
# simple overuse injury transition matrix
transitions_test = list(c(3), 
                       c(3),
                       c(2, 4),
                       c())

statenames_test = c("startstate", "asymptomatic", "symptomatic", "worse")
transmat_test = transMat(transitions_test, statenames_test)

d_selected = d_selected %>% mutate(symptomatic = case_when(knee_state == 2 ~ 1, 
                                              is.na(knee_state) ~ 0,
                                              TRUE ~ 0),
                 asymptomatic = case_when(knee_state == 1 ~ 1, 
                                          is.na(knee_state) ~ 0,
                                          TRUE ~ 0),
                 worse = case_when(knee_state == 3 ~ 1, 
                                   is.na(knee_state) ~ 0,
                                   TRUE ~ 0))
# the asymptomatic state is more complicated, because the lack of injuries in the beginning of the time
# series will be considered under a "healthy" state
first_injes = d_selected %>% filter(!is.na(knee_state)) %>% 
  distinct(d_imp, id_player, .keep_all = TRUE) %>% 
  mutate(first_inj = 1) %>% 
  select(d_imp, id_player, date, first_inj) 

d_test = d_selected %>% left_join(first_injes, by = c("d_imp","id_player", "date")) %>% 
  group_by(d_imp, id_player) %>% fill(first_inj, .direction = "up") %>% 
  mutate(first_inj = ifelse(is.na(first_inj), 0, first_inj),
         first_inj = ifelse(!is.na(knee_state), 0, first_inj)) %>% ungroup() 

msprep(data = d_test, trans = transmat_test, 
       time = c(NA, rep("day", 4)), 
       status = c(NA, "healthy", "asymptomatic", "symptomatic", "worse"), 
       keep = c("id_player"))

# put states into their own variables
d_u19_states_temp = d_u19 %>% mutate(overuse = ifelse(injury == 1 & overuse == 2, 1, 0),
                                     acute = ifelse(injury == 1 & substantial == 0 & overuse == 1, 1, 0),
                                     acute_subst = ifelse(injury == 1 & substantial == 1 & overuse == 1, 1, 0)) %>% 
  select(p_id, date_training, load, overuse, acute, acute_subst, injury)

# the asymptomatic state is more complicated, becuase the lack of injuries in the beginning of the time
# series will be considered under a "healthy" state
first_injes = d_u19_states_temp %>% filter(injury == 1) %>% distinct(p_id, .keep_all = TRUE) %>% select(p_id, date_training, first_inj = injury)
d_u19_states = d_u19_states_temp %>% left_join(first_injes, by = c("p_id", "date_training")) %>% 
  group_by(p_id) %>% fill(first_inj, .direction = "up") %>% mutate(first_inj = ifelse(is.na(first_inj), 0, first_inj)) %>% ungroup()


d_u19_states = d_u19_states %>% mutate(asymptomatic = ifelse(first_inj == 0 | injury == 0, 1, 0),
                                       asymptomatic = ifelse(first_inj == 1 | injury == 1, 0, asymptomatic)) 

# add number of days of follow up per event
d_u19_states = d_u19_states %>% group_by(p_id) %>% mutate(day = 1:n()) %>% select(-date_training) %>% ungroup()
# d_u19_states = d_u19_states %>% filter(load <= 2000)
# fill in missing load values
d_u19_states = d_u19_states %>% group_by(p_id) %>% mutate(load = ifelse(is.na(load), mean(load, na.rm = TRUE), load)) %>% ungroup()

msprep(data = ebmt4, trans = tmat, 
       time = c(NA, "rec", "ae","recae", "rel", "srv"), 
       status = c(NA, "rec.s", "ae.s", "recae.s", "rel.s", "srv.s"), 
       keep = c("match", "proph", "year", "agecl"))

msprep(data = d_u19_states, trans = transmat_u19, 
       time = c(NA, rep("day", 3)), 
       status = c(NA, "overuse", "acute", "asymptomatic"), 
       keep = c("p_id", "load"))




library(mstate)
# transition matrix for illness-death model
tmat <- trans.illdeath()
# some data in wide format
tg <- data.frame(stt=rep(0,6),sts=rep(0,6),
                 illt=c(1,1,6,6,8,9),ills=c(1,0,1,1,0,1),
                 dt=c(5,1,9,7,8,12),ds=c(1,1,1,1,1,1),
                 x1=c(1,1,1,2,2,2),x2=c(6:1))
tg$x1 <- factor(tg$x1,labels=c("male","female"))
tg$patid <- factor(2:7,levels=1:8,labels=as.character(1:8))
# define time, status and covariates also as matrices
tt <- matrix(c(rep(NA,6),tg$illt,tg$dt),6,3)
st <- matrix(c(rep(NA,6),tg$ills,tg$ds),6,3)
keepmat <- data.frame(gender=tg$x1,age=tg$x2)
# data in long format using msprep
msprep(time=tt,status=st,trans=tmat,keep=as.matrix(keepmat))
msprep(time=c(NA,"illt","dt"),status=c(NA,"ills","ds"),data=tg,
       id="patid",keep=c("x1","x2"),trans=tmat)
# Patient no 5, 6 now start in state 2 at time t=4 and t=10
msprep(time=tt,status=st,trans=tmat,keep=keepmat,
       start=list(state=c(1,1,1,1,2,2),time=c(0,0,0,0,4,10)))