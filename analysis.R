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

# arrange dates
d_analysis = d_analysis %>% arrange(d_imp, id_player, date)

# find the events
d_events = d_analysis  %>% 
  group_by(d_imp) %>% 
  filter(inj_knee == 1) %>% mutate(id_event = 1:n()) %>% 
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

# find follow up time per symptom episode, per player
# d_events = d_time_to_sympt %>% 
#   group_by(d_imp) %>% 
#   filter(inj_knee == 1) %>% mutate(id_event = 1:n()) %>% 
#   ungroup() %>% select(d_imp, id_player, date, id_event)
# 
d_time_to_sympt = d_time_to_sympt %>% arrange(d_imp, id_player, date) %>%
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
  select(id_player, id_event, day, date, age, jump_height_max, position, 
         match, t_prevmatch, jumps_n_weekly, preseason, jump_height_sum) %>% 
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

# add confounders back to datasets
l_surv_cpform = l_surv_cpform %>% 
  map(.  %>% left_join(d_confounders_freq, 
                       by = c("id_player", "id" = "id_event", "exit" = "day")))

# arrange the exposure history in wide format in a matrix
l_tl_hist = l_surv %>% map(. %>% select(Id, jumps_n, Stop))
l_tl_hist_spread_day = 
  l_tl_hist %>% map(. %>% pivot_wider(names_from = Stop, values_from = jumps_n) %>% 
                      select(-Id) %>% as.matrix)

# calc Q matrices
l_q_mat = map2(.x = l_surv_cpform,
               .y = l_tl_hist_spread_day, 
               ~calc_q_matrix(.x, .y))

# make the crossbasis
l_cb_dlnm = l_q_mat %>% map(~crossbasis(., lag=c(lag_min, lag_max), 
                                        argvar = list(fun="ns", knots = 3),
                                        arglag = list(fun="ns", knots = 3)))

library(coxme)
# no frailty
l_fit_dlnm_nofrailty = 
             map2(.x = l_surv_cpform,
                  .y = l_cb_dlnm,
                  ~coxph(Surv(enter, exit, event) ~ .y + position + age + 
                           jump_height_max + match + t_prevmatch, data = .x))


# frailty
# method to manually get the pooled results from coxme: https://github.com/amices/mice/issues/123
l_fit_dlnm = map2(.x = l_surv_cpform,
                  .y = l_cb_dlnm,
                  ~coxme(Surv(enter, exit, event) ~ .y + position + age + 
                           jump_height_max + match + t_prevmatch + (1 | id), data = .x))

AIC(l_fit_dlnm_nofrailty[[1]])
AIC(l_fit_dlnm[[1]])


l_fit_preseason = map2(.x = l_surv_cpform,
                  .y = l_cb_dlnm,
                  ~coxme(Surv(enter, exit, event) ~ preseason + (1 | id), data = .x))


l_fit_preseason_mediatoradj = map2(.x = l_surv_cpform,
                  .y = l_cb_dlnm,
                  ~coxme(Surv(enter, exit, event) ~ .y + preseason + jump_height_sum + t_prevmatch + (1 | id), data = .x))

summary(l_fit_preseason[[1]])

summary(l_fit_preseason_mediatoradj[[1]])


l_fit_tprevmatch = map2(.x = l_surv_cpform,
                       .y = l_cb_dlnm,
                       ~coxme(Surv(enter, exit, event) ~ t_prevmatch + (1 | id), data = .x))


#---------------------------------Figures 

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
predvalues = seq(min(d_analysis$jumps_n), max(d_analysis$jumps_n), 10)
lag_seq = lag_min:lag_max 

# predict hazards
l_cp_preds_dlnm = 
  map2(.x = l_fit_dlnm,
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
  xlab("N Jumps") +
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





library(mice)
library(broom)
library(broom.mixed)
library(eha)
library(ehahelper)
d_pooled = summary(l_fit_dlnm %>% mice::pool(), 
                   conf.int = TRUE, exponentiate = TRUE) %>% as_tibble() %>% 
                    mutate_if(is.numeric, ~round(.,3))



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

