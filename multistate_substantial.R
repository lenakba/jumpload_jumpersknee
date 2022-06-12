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

# make one variable with knee state
# where a substantial state is prioritized over symptomatic state
d_kneelevels = d_analysis %>% group_by(d_imp, id_player) %>% 
  mutate(knee_state = ifelse(inj_knee_subst_filled == 1, 3, inj_knee_filled+1)) %>% ungroup() 

# fixme! handle missing data in a better way
# d_kneelevels = d_kneelevels %>% 
#   group_by(d_imp) %>% 
#   mutate(knee_state = case_when(is.na(knee_state) & knee_total_filled == 0 ~ 1,
#                                 is.na(knee_state) & knee_total_filled > 0 ~ 2,
#                                 TRUE ~ knee_state)
#   ) %>% ungroup()

# select variables we are going to use.
d_selected = d_kneelevels  %>% 
  select(d_imp, id_player, season, date, knee_state, jumps_n, all_of(conf_cols))
d_selected %>% select(id_player, knee_state) %>% View()

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

# state 1 is asymptomatic
# state 2 is symptomatic
# state 3 is substantial
# from 1 to 2 is transition 1
# from 1 to 3 is transition 2
# from 2 to 1 is transition 3
# from 2 to 3 is transition 4
# from 3 to 1 is transition 5
# from 3 to 2 is transition 6

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

