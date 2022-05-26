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
lag_min = 7
lag_max = 41

# or consider first day of OSTRC week to be day of injury?
lag_min = 0

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

# first, we analyze jump frequency
d_confounders_freq = d_analysis %>% filter(d_imp == 1) %>% 
  distinct(id_player, date, .keep_all = TRUE) %>% 
  select(id_player, date, age, jump_height_max, position, match, t_prevmatch, jumps_n_weekly) %>% 
  mutate(position  = factor(position),
         match = factor(match))

# add number of days
d_analysis = d_analysis %>% arrange(d_imp, id_player, date) %>% 
  group_by(d_imp, id_player) %>% 
  mutate(day = 1:n())

d_analysis = d_analysis %>% group_by(d_imp, id_player) %>%
  mutate(Fup = ifelse(inj_knee == 1, day, NA)) %>% 
  fill(Fup, .direction = "up") %>% 
  ungroup()

#---------------------------------------- Preparing for DLNM

# function to arrange survival data in counting process form
counting_process_form = function(d_survival_sim){
  d_follow_up_times = d_survival_sim %>% distinct(Id, Fup, .keep_all = TRUE)
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
  d_counting_process %>% select(-Start, -Fup)
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

# temporary select to reduce the amount of memory during computation
d_selected = d_analysis %>% select(d_imp, id_player, date, jumps_n, inj_knee, day, Fup)

# find start and stop times
d_surv = d_selected %>% group_by(d_imp, id_player) %>% 
                              rename(Stop = day, Id = id_player, Event = inj_knee) %>% 
                              mutate(Start = lag(Stop),
                                     Start = ifelse(is.na(Start), 0, Start)) %>% ungroup()

l_surv = (d_surv %>% group_by(d_imp) %>% nest())$data


d_follow_up_times = l_surv[[1]] %>% distinct(Id, Fup, .keep_all = TRUE)
d_surv_lim = map2(.x = d_follow_up_times$Id,
                  .y = d_follow_up_times$Fup,
                  ~l_surv[[1]] %>% filter(Id == .x) %>% slice(.y)) %>% 
  bind_rows() %>% rename(event = Event, exit = Stop, id = Id)
# extracting timepoints in which an event happened
ftime = d_surv_lim %>% filter(event == 1) %>% distinct(exit) %>% arrange(exit) %>% pull()

# arrange the survival data so that, for each individual, we have an interval of enter and exit times
# for each of the exit times above, with the information of whether or not they were injured at that time
# meaning we will have the same time intervals per participant
d_counting_process = survSplit(Surv(exit, event)~., d_surv_lim, cut = ftime, start="enter") %>% arrange(id)
d_counting_process


# rearrange to counting process form
#d_surv_cpform = d_surv %>% group_by(d_imp) %>% counting_process_form(.) %>% mutate(id = as.numeric(id)) %>% ungroup()
l_surv_cpform =  l_surv %>% map(~counting_process_form(.) %>% mutate(id = as.numeric(id)))
l_surv_cpform = l_surv_cpform %>% map(. %>% as_tibble())

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
l_surv_cpform = l_surv_cpform %>% map(.  %>%
                                      left_join(d_confounders_freq, by = c("id" = "id_player", "date")))

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

median()
plot(survfit(f_sur~(total_usage > 5866.2),data=ff_usage))



library(coxme)
# no frailty
l_fit_dlnm_nofrailty = map2(.x = l_surv_cpform,
                  .y = l_cb_dlnm,
                  ~coxph(Surv(enter, exit, event) ~ .y + position + age + 
                           jump_height_max + match + t_prevmatch, data = .x))


# frailty
l_fit_dlnm = map2(.x = l_surv_cpform,
                  .y = l_cb_dlnm,
                  ~coxme(Surv(enter, exit, event) ~ .y + position + age + 
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
