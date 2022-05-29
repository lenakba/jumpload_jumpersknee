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
# d_all = read_delim(paste0(data_folder, "d_volleyball.csv"), delim = ";", na = "")

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

# define states
# we can either take this problem through a 
# regular vs. substantial injury defition
# or going from one level (no matter what level)
# to something worse
# we will consider the "worsening" problem
statenames = c("asymptomatic", "symptomatic", "worsening", "improvement")
l_transitions = list(c(2), 
                     c(1, 3, 4), 
                     c(1, 4),
                     c(1, 3))

# We start by filling the values 6 days down.
# find which days the OSTRC-questionnaire actually pertains to
# since each answer is for the previous 6 days including the current day
d_ostrc_dates = d_analysis %>% select(d_imp, all_of(key_cols), Knee_Total) %>% filter(!is.na(Knee_Total))
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
  fill(Knee_Total, .direction = "down") %>% select(-date_last) %>% rename(knee_total_filled = Knee_Total)

d_analysis = d_analysis %>% left_join(d_ostrc_dates_valid, 
                            by = c("d_imp", "id_player", "id_team", "id_team_player", "id_season", "date"))


d_analysis %>% select(d_imp, id_player, Knee_Total)

# fixme! find better solution for handling missing responses
# fixme! maybe assume that 1 NA between two OSTRC intervals of 0 is 0?
d_analysis = d_analysis %>% mutate(knee_total_filled = ifelse(is.na(knee_total_filled), 0, knee_total_filled),
                                   inj_knee_filled = ifelse(is.na(inj_knee_filled), 0, inj_knee_filled))

# Find each interval, so that we can determine, for each,
# which state they are in (when they are not 0)
# and give each an ID

# find the events
d_events = d_analysis  %>% 
  group_by(d_imp) %>% 
  filter(inj_knee_filled == 1) %>% mutate(id_event = 1:n()) %>% 
  ungroup() %>% select(d_imp, id_player, date, id_event)

no_inj_players = d_analysis %>% group_by(id_player) %>% summarise(sum = sum(inj_knee_filled)) %>% 
  ungroup %>% filter(sum == 0) %>% pull(id_player)

# fetch those with injuries
d_inj_only = d_analysis %>% filter(!id_player %in% no_inj_players)
ids = d_inj_only %>% distinct(id_player) %>% pull()

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


d_event_ids = d_time_to_sympt %>% 
  left_join(d_eventid, by = c("id_player", "not_unique_id")) %>% 
  select(d_imp, id_player, date, id_event)

d_inj_struct = d_analysis %>% left_join(d_event_ids, by = c("d_imp", "id_player", "date")) %>% 
             group_by(d_imp, id_player) %>% 
             fill(id_event, .direction = "down")

d_kneelevels = d_inj_struct %>% group_by(d_imp, id_player, id_event) %>% mutate(knee_total_filled_lag = lag(knee_total_filled),
                  knee_total_diff = knee_total_filled - knee_total_filled_lag,
                  knee_state = case_when(Knee_Total >= 1 & knee_total_filled_lag == 0 ~ 1,
                                         knee_total_diff < 0 & Knee_Total != 0 ~ 3,
                                         knee_total_diff > 0 & Knee_Total != 0 ~ 2,
                                         Knee_Total == 0 ~ 0)) %>% ungroup() 

# put states into their own variables
d_u19_states_temp = d_u19 %>% mutate(knee_mild = ifelse(inj_knee_filled == 1 & overuse == 2, 1, 0),
                                     acute = ifelse(injury == 1 & substantial == 0 & overuse == 1, 1, 0),
                                     acute_subst = ifelse(injury == 1 & substantial == 1 & overuse == 1, 1, 0)) %>% 
  select(p_id, date_training, load, overuse, acute, acute_subst, injury)



d_time_to_sympt = d_time_to_sympt %>% arrange(d_imp, id_player, date) %>%
  group_by(d_imp, id_player, id_event) %>%
  mutate(Fup = n(), day = 1:n()) %>% ungroup()

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
