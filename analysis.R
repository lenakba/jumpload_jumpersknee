library(tidyverse) # data wrangling
library(dlnm) # distributed lag non-linear models
library(survival)

# so we don't have to deal with scientific notations
# and strings aren't automatically read as factors:
options(scipen = 30, 
        stringsAsFactors = FALSE)

data_folder = "D:\\phd\\jump load\\data\\"
d_jumpload = readRDS(paste0(data_folder, "d_jumpload_multimputed.rds"))

# number of injuries in the data
d_jumpload %>% filter(.imp == 1) %>% summarise(sum = sum(as.numeric(inj_knee), na.rm = TRUE))

# just testing that the data is valid
d_jumpload %>% filter(is.na(id_player))
any(duplicated(d_jumpload))

# define key columns
key_cols = c("date", "id_player", "id_team", "id_team_player", "id_season")
# d_all = read_delim(paste0(data_folder, "d_volleyball.csv"), delim = ";", na = "")

# find the min and max lag
# lag set at 4 weeks (28) as is often used in tl studies
# we will set at 6 weeks initially to explore the effect of time
# since injury is interval-censored, the first week cannot be included in the DLNM
# note that "0" is current day, and so 7 is the first day after the first 7 days.
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
                 starts_with("inj"), 
                 year, month_day, season, 
                 preseason,
                 age, weight, height, position,
                 session_type, match = Match, 
                 t_prevmatch, game_type, match_sets_n = MatchSets,
                 d_imp = .imp)

d_analysis = d_analysis %>% mutate_at(vars(starts_with("inj")), ~as.numeric(.))

# add number of days
d_analysis = d_analysis %>% arrange(d_imp, id_player, date)

# add the follow-up time to each injury
d_events = d_analysis  %>% 
  group_by(d_imp) %>% 
  filter(inj_knee_filled == 1) %>% mutate(id_event = 1:n()) %>% 
  ungroup() %>% select(d_imp, id_player, date, id_event)

# how to structure injuries
# multiple ways
# here we assume that each interval is new per player

# d_analysis = d_analysis %>% group_by(d_imp, id_player) %>%
#   mutate(Fup = ifelse(inj_knee_filled == 1, day, NA)) %>% 
#   fill(Fup, .direction = "up") %>% 
#   ungroup()

# d_analysis_all = d_analysis %>% arrange(d_imp, id_player, date) %>% 
#   left_join(d_events, by = c("d_imp", "id_player", "date")) %>%
#   group_by(d_imp, id_player) %>%
#   fill(id_event, .direction = "up") %>%
#   group_by(d_imp, id_player, id_event) %>%
#   mutate(Fup = n(), day = 1:n()) %>% ungroup()

# we can also assume that we are taking the "time to next symptom week"
# if they have symptoms multiple weeks in a row, that is still considered 1 symptom week
# we can call this a jumper's knee episode
d_analysis = d_analysis %>% mutate(inj_knee_filled = 
                                  ifelse(is.na(inj_knee_filled), 0, inj_knee_filled))

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
d_time_to_sympt = bind_rows(d_maindata, d_analysis %>% filter(id_player %in% no_inj_players))

# find follow up time per symptom episode, per player
d_events = d_time_to_sympt %>% 
  group_by(d_imp) %>% 
  filter(inj_knee == 1) %>% mutate(id_event = 1:n()) %>% 
  ungroup() %>% select(d_imp, id_player, date, id_event)

d_time_to_sympt = d_time_to_sympt %>% arrange(d_imp, id_player, date) %>% 
  left_join(d_events, by = c("d_imp", "id_player", "date")) %>%
  group_by(d_imp, id_player) %>%
  fill(id_event, .direction = "up") %>%
  group_by(d_imp, id_player, id_event) %>%
  mutate(Fup = n(), day = 1:n()) %>% ungroup()

#---------------------------------------- Preparing for DLNM

# function to arrange survival data in counting process form
counting_process_form = function(d_survival_sim){
  d_follow_up_times = d_survival_sim %>% distinct(Id, Fup)
  d_surv_lim = map2(.x = d_follow_up_times$Id,
                    .y = d_follow_up_times$Fup,
                    ~d_survival_sim %>% filter(Id == .x) %>% slice(.y)) %>% 
    bind_rows() %>% rename(event = Event, exit = Stop, id = Id)
  
  # extracting timepoints in which an event happened
  ftime = d_surv_lim %>% filter(event == 1) %>% distinct(exit) %>% arrange(exit) %>% pull()
  
  # arrange the survival data so that, for each individual, we have an interval of enter and exit times
  # for each of the exit times above, with the information of whether or not they were injured at that time
  # meaning we will have the same time intervals per participant
  d_counting_process = survSplit(Surv(exit, event)~., d_surv_lim, cut = ftime, start="enter") %>% arrange(id)
  d_counting_process %>% select(-Start, -Fup) %>% tibble() 
}

# function for calculating the q matrix (needed for DLNM) given the survival data in counting process form
# and the exposure history spread in wide format in a matrix
calc_q_matrix = function(d_counting_process, d_tl_hist_wide){
  # for each individual, for each of these exit times, we will extract the exposure history 
  # for the given lag-time which we are interested in
  # This is called the Q-matrix. The Q-matrix should be nrow(dataspl) X 0:lag_max dimensions.
  q = map2(.x = d_counting_process$id, 
           .y = d_counting_process$exit, 
           ~exphist(d_tl_hist_wide[.x,], .y, c(lag_min, lag_max))) %>% 
    do.call("rbind", .)
  q
}

# first, we analyze jump frequency
d_confounders_freq = d_time_to_sympt %>% filter(d_imp == 1) %>% 
  distinct(id_player, date, .keep_all = TRUE) %>% 
  select(id_player, id_event, day, date, age, jump_height_max, position, match, t_prevmatch, jumps_n_weekly) %>% 
  mutate(position  = factor(position),
         match = factor(match))

# temporary select to reduce the amount of memory during computation
d_selected = d_time_to_sympt %>% select(d_imp, id_player, id_event, date, jumps_n, inj_knee, day, Fup)
d_selected = d_selected %>% mutate(inj_knee = ifelse(is.na(inj_knee), 0, inj_knee))

# find start and stop times
d_surv = d_selected %>% group_by(d_imp, id_player) %>% 
                              rename(Stop = day, Id = id_event, Event = inj_knee) %>% 
                              mutate(Start = lag(Stop),
                                     Start = ifelse(is.na(Start), 0, Start)) %>% ungroup()

l_surv = (d_surv %>% group_by(d_imp) %>% nest())$data

# rearrange to counting process form
l_surv_cpform = l_surv %>% map(~counting_process_form(.) %>% mutate(id = as.numeric(id)))
l_surv_cpform = l_surv_cpform %>% map(. %>% as_tibble() %>% select(-date, -jumps_n))

# arrange the exposure history in wide format in a matrix
l_tl_hist = l_surv %>% map(. %>% select(Id, jumps_n, Stop))
l_tl_hist_spread_day = 
  l_tl_hist %>% map(. %>% pivot_wider(names_from = Stop, values_from = jumps_n) %>% 
                      select(-Id) %>% as.matrix)

# calc Q matrices
l_q_mat = map2(.x = l_surv_cpform,
               .y = l_tl_hist_spread_day, 
               ~calc_q_matrix(.x, .y))

# add confounders back to datasets
l_surv_cpform = l_surv_cpform %>% 
                map(.  %>% left_join(d_confounders_freq, 
                                     by = c("id_player", "id" = "id_event", "exit" = "day")))

# make the crossbasis
l_cb_dlnm = l_q_mat %>% map(~crossbasis(., lag=c(lag_min, lag_max), 
                                        argvar = list(fun="ns", knots = 3),
                                        arglag = list(fun="ns", knots = 3)))
# fit DLNM
l_fit_dlnm_nofrailty = map2(.x = l_surv_cpform,
                  .y = l_cb_dlnm,
                  ~coxph(Surv(enter, exit, event) ~ .y + position + age + jump_height_max + 
                           match + t_prevmatch + frailty(id),
                           data = .x, y = FALSE, ties = "efron"))




library(coxme)
# no frailty
l_fit_dlnm_nofrailty = map2(.x = l_surv_cpform,
                  .y = l_cb_dlnm,
                  ~coxph(Surv(enter, exit, event, type = "interval2") ~ .y + position + age + 
                           jump_height_max + match + t_prevmatch, data = .x))


# frailty
# method to manually get the pooled results from coxme: https://github.com/amices/mice/issues/123
l_fit_dlnm = map2(.x = l_surv_cpform,
                  .y = l_cb_dlnm,
                  ~coxme(Surv(enter, exit, event, type = "interval2") ~ .y + position + age + 
                           jump_height_max + match + t_prevmatch + (1 | id), data = .x))

AIC(l_fit_dlnm_nofrailty[[1]])
AIC(l_fit_dlnm[[1]])

class(l_fit_dlnm_nofrailty[[1]])
class(l_fit_dlnm[[1]])

l_fit_dlnm_nofrailty[[1]]

summary(l_fit_dlnm_nofrailty[[1]])

l_fit_dlnm %>% map(~as.mira(.)) %>% pool()

as.mira(l_fit_dlnm[[1]]) 
as.coxph(l_fit_dlnm[[1]])

library(mice)
library(broom)
library(broom.mixed)
library(eha)
library(ehahelper)
d_pooled = summary(l_fit_dlnm %>% mice::pool(), conf.int = TRUE, exponentiate = TRUE) %>% as_tibble() %>% mutate_if(is.numeric, ~round(.,3))



library(icenReg)
data(miceData)


#----------------------------------------------example using tooth24 dataset

# example from https://docs.ufpr.br/~giolo/CE063/Artigos/A4_Gomes%20et%20al%202009.pdf

load(paste0(data_folder,"tooth24.RData")) 

tooth24 %>% tibble()

tooth24$cens=with(tooth24,ifelse(right==999,0,3))
sur24b<-with(tooth24,Surv(left,right,cens,type="interval"))
dim(sur24b)


# 3 interval-censored (all our events are 3, since OSTRC is for a whole week)
d_inj_number = d_analysis %>% group_by(d_imp, id_player) %>% 
  filter(inj_knee_filled == 1) %>% mutate(inj_number = 1:n()) %>% 
  ungroup() %>% select(d_imp, id_player, date, inj_number)

d_analysis = d_analysis %>% 
  left_join(d_inj_number, by = c("d_imp", "id_player", "date"))

d_analysis = d_analysis %>% 
  group_by(d_imp, id_player) %>% 
  fill(inj_number, .direction = "up") %>% 
  mutate(event = case_when(is.na(inj_knee_filled) & (inj_number == 1) ~ 2, 
                           inj_knee_filled == 1 ~ 3,
                           is.na(inj_knee_filled) ~ 0,
                           TRUE ~ inj_knee_filled)) %>% 
  select(-inj_number) %>% 
  ungroup()


# fix injuries to be in survival data format
# 0 for right-censored
# 1 for event
# 2 left censored (any observation before the first OSTRC questionnaire)
# 3 interval-censored
d_inj_number = d_analysis %>% group_by(d_imp, id_player) %>% 
  filter(inj_knee_filled == 1) %>% mutate(inj_number = 1:n()) %>% 
  ungroup() %>% select(d_imp, id_player, date, inj_number)

d_analysis = d_analysis %>% 
  left_join(d_inj_number, by = c("d_imp", "id_player", "date"))

d_analysis = d_analysis %>% 
  group_by(d_imp, id_player) %>% 
  fill(inj_number, .direction = "up") %>% 
  mutate(inj_knee_event = case_when(is.na(inj_knee_filled) & (inj_number == 1) ~ 2,
                                    is.na(inj_knee_filled) ~ 0,
                                    TRUE ~ inj_knee_filled)) %>% 
  select(-inj_number) %>% 
  ungroup()

