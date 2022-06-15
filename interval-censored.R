library(tidyverse) # data wrangling
library(dlnm) # distributed lag non-linear models
library(survival)
library(lubridate) # to manipulate dates
library(splines) # natural splines
library(icenReg) # interval-censored survival analysis

# so we don't have to deal with scientific notations
# and strings aren't automatically read as factors:
options(scipen = 30, 
        stringsAsFactors = FALSE)

data_folder = "D:\\phd\\jump load\\data\\"
d_jumpload = readRDS(paste0(data_folder, "d_jumpload_multimputed.rds"))

# define key columns
key_cols = c("date", "id_player", "id_team", "id_team_player", "id_season")
conf_cols = c("age", "jump_height_max", "position", 
              "match", "t_prevmatch", "jumps_n_weekly", "preseason", "jump_height_sum", 
              "jump_height_sum_perc","weight")


# define the min and max lag
# we will lag the data, so lag 0
# corresponds to the last day before the OSTRC week
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

# function for calculating the q matrix (needed for DLNM) given the survival data in counting process form
# and the exposure history spread in wide format in a matrix
calc_q_matrix = function(d_tl_hist_wide, id, exit, lag_min, lag_max){
  
  id = id
  exit = exit
  
  # for each individual, for each of these exit times, we will extract the exposure history 
  # for the given lag-time which we are interested in
  # This is called the Q-matrix. The Q-matrix should be nrow(dataspl) X 0:lag_max dimensions.
  q = exit %>% map(., ~exphist(d_tl_hist_wide, ., c(lag_min, lag_max))) %>% 
    do.call("rbind", .)
  q
}

# from asymptomatic to any symptoms
d_strata = d_analysis  %>% 
  select(d_imp, id_player, season, date, inj_knee_filled, jumps_n, all_of(conf_cols))

# fixme! better missing solution.
d_strata = d_strata %>% group_by(d_imp, id_player, season) %>% 
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
                           position = as.factor(position),
                           match = as.factor(match))

# add event id so that we may calculate the Q amtrix per event
# we also need variable that we may stratify on
# the intervals in which the player is at risk (i.e. not while they already have symptoms)
d_surv = d_surv %>% mutate(status = ifelse(lag(inj_knee_filled) == 0 & 
                                             inj_knee_filled == 1, 1, 0),
                             status = ifelse(is.na(status), 0, status))
d_surv = d_surv %>% add_event_id(status)
d_surv = d_surv %>% mutate(id_dlnm = paste0(id_player, "-", season, "-", id_event),
                               inj_knee_filled_fixed = ifelse(status == 1, 2, inj_knee_filled))

#-------------------------------------include interval-censoring---------------------------

# make sure overlapping days are not included in the data
d_cens = d_surv %>% group_by(d_imp, id_player, season) %>% 
                    mutate(stop_cens = ifelse(status == 1, stop+6, stop),
                           status_cens = ifelse(status == 1, 3, status),
                           lag_cens_6 = lag(status_cens, 6),
                           lag_cens_1 = lag(status_cens),
                           lag_cens_2 = lag(status_cens, 2),
                           lag_cens_3 = lag(status_cens, 3),
                           lag_cens_4 = lag(status_cens, 4),
                           lag_cens_5 = lag(status_cens, 5),
                           dupl = ifelse(lag_cens_6 == 3 | lag_cens_1 == 3 |
                                         lag_cens_2 == 3 | lag_cens_3 == 3 |
                                         lag_cens_4 == 3 | lag_cens_5 == 3, 1, 0),
                           dupl = ifelse(is.na(dupl), 0, dupl)) %>% 
  ungroup() %>% 
  select(-starts_with("lag"))

# make into list
d_cens1 = d_cens %>% filter(d_imp == 1)
l_cens = (d_cens %>% group_by(d_imp) %>% nest())$data

# we will lag the jump load data by 1 day to ensure we only look at 
# what happened before the week (the interval of uncertainty)
# The missing data is not included in the calculation of the past load, 
# as the event happens before it would be included
# we can therefore fill it with whatever 
# (the results do not change whether we choose to impute with the mean or 0, for instance)
l_hist_cens = l_cens %>% 
  map(. %>% select(id_player, season, id_dlnm, jumps_n, stop) %>% 
        arrange(id_dlnm) %>% 
        group_by(id_dlnm) %>% 
        mutate(jumps_n_lag = lag(jumps_n),
               jumps_n_lag = 
                 ifelse(is.na(jumps_n_lag), mean(jumps_n, na.rm = TRUE), jumps_n_lag)) %>% 
        ungroup() %>% 
        arrange(stop, id_dlnm) %>% select(-jumps_n)
  )

l_hist_spread_day_cens = 
  l_hist_cens %>% map(. %>% pivot_wider(names_from = stop, values_from = jumps_n_lag)  %>% 
                           group_by(id_player, season) %>% 
                           fill(where(is.numeric), .direction = "downup") %>% ungroup() %>% 
                           mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) %>% 
                           select(-id_dlnm, -id_player, -season) %>% as.matrix)

# calc Q matrices
l_q_mat_cens = map2(.x = l_hist_cens,
                    .y = l_hist_spread_day_cens, 
                    ~calc_q_matrix(.y, .x$id_dlnm, .x$stop, lag_min, lag_max))

# subjectively placed knots
# since the data is so skewed
# with sparse data >200 jumps on a day
# just check 
# ting = d_analysis %>% filter(d_imp == 1)
# hist(ting$jumps_n)
l_cb_cens = l_q_mat_cens %>% map(~crossbasis(., lag=c(lag_min, lag_max), 
                                                  argvar = list(fun="ns", knots = c(50, 100, 150)),
                                                  arglag = list(fun="poly", degree = 2)))

cb_cens = l_cb_cens[[1]]
d_cens1 = d_cens1 %>% 
  mutate(jumps_n_weekly = 
           ifelse(is.na(jumps_n_weekly), 
                  mean(jumps_n_weekly, na.rm = TRUE), jumps_n_weekly))

icen_fit = ic_par(Surv(enter, 
                       stop_cens, 
                       status_cens, 
                       type = "interval") ~ position + age + season + cb_cens +
                    jump_height_max + match + t_prevmatch + jumps_n_weekly, model = 'ph',
                  data = d_cens1)
summary(icen_fit)
class(icen_fit)
confint(icen_fit)

# removing the duplicated rows
l_cb_cens_nodupl = l_q_mat_cens %>% map(~crossbasis(., lag=c(lag_min, lag_max), 
                                             argvar = list(fun="ns", knots = c(50, 100, 150)),
                                             arglag = list(fun="poly", degree = 2)))

cb_cens_nodupl = l_cb_cens[[1]]
pos_dups = which(d_cens1$dupl==1)
d_cens_nodupl = d_cens1 %>% slice(-pos_dups)
cb_cens_nodupl = cb_cens_nodupl[-pos_dups,]

icen_fit_nodupl = ic_par(Surv(enter, stop_cens, status_cens, type = "interval") ~ 
      position + age + season + cb_cens_nodupl +
      jump_height_max + match + t_prevmatch + jumps_n_weekly, 
      model = 'ph', data = d_cens_nodupl)


ic_sp(Surv(enter, stop_cens, status_cens, type = "interval") ~ position + age + season + cb_cens_nodupl +
        jump_height_max + match + t_prevmatch + jumps_n_weekly, 
      data = d_cens_nodupl, model = "ph", bs_samples = 3)

