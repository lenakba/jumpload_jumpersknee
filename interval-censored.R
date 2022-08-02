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

data_folder = "O:\\Prosjekter\\Bache-Mathiesen-Biostatistikk\\Data\\volleyball\\"
d_jumpload = readRDS(paste0(data_folder, "d_jumpload_multimputed.rds"))

# define key columns
key_cols = c("date", "id_player", "id_team", "id_team_player", "id_season")
conf_cols = c("age", "jump_height_max", "position", 
              "match", "t_prevmatch", "jumps_n_weekly", "jumps_height_weekly", "preseason", "jump_height_sum", 
              "jump_height_perc_sum","weight")

# define the min and max lag
# we will lag the data, so lag 0
# corresponds to the last day before the OSTRC week
lag_min = 0
lag_max = 20

# select columns that may be useful in analyses
d_analysis = d_jumpload %>% 
  select(all_of(key_cols), 
         jumps_n, 
         jumps_n_weekly,
         jump_height_sum,
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

# add event id so that we may calculate the Q matrix per event
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
                                                  argvar = list(fun="ns", knots = c(10, 100, 150)),
                                                  arglag = list(fun="poly", degree = 2)))

# do the same for jump height sum perc
l_hist_cens_height = l_cens %>% 
  map(. %>% select(id_player, season, id_dlnm, jump_height_perc_sum, stop) %>% 
        arrange(id_dlnm) %>% 
        group_by(id_dlnm) %>% 
        mutate(jump_height_perc_lag = lag(jump_height_perc_sum),
               jump_height_perc_lag = 
                 ifelse(is.na(jump_height_perc_lag), mean(jump_height_perc_sum, na.rm = TRUE), jump_height_perc_lag)) %>% 
        ungroup() %>% 
        arrange(stop, id_dlnm) %>% select(-jump_height_perc_sum)
  )

l_hist_spread_day_cens_height = 
  l_hist_cens_height %>% map(. %>% pivot_wider(names_from = stop, values_from = jump_height_perc_lag)  %>% 
                        group_by(id_player, season) %>% 
                        fill(where(is.numeric), .direction = "downup") %>% ungroup() %>% 
                        mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) %>% 
                        select(-id_dlnm, -id_player, -season) %>% as.matrix)

# calc Q matrices
l_q_mat_cens_height = map2(.x = l_hist_cens_height,
                    .y = l_hist_spread_day_cens_height, 
                    ~calc_q_matrix(.y, .x$id_dlnm, .x$stop, lag_min, lag_max))

# subjectively placed knots
# since the data is so skewed
# with sparse data >200 jumps on a day
# just check 
# ting = d_analysis %>% filter(d_imp == 1)
# hist(ting$jumps_n)
l_cb_cens_height = l_q_mat_cens_height %>% map(~crossbasis(., lag=c(lag_min, lag_max), 
                                             argvar = list(fun="ns", knots = c(10, 100, 150)),
                                             arglag = list(fun="poly", degree = 2)))

# fill the first 6 days that are missing weekly load
d_cens1 = d_cens1 %>% 
  mutate(jumps_n_weekly = 
           ifelse(is.na(jumps_n_weekly), 
                  mean(jumps_n_weekly, na.rm = TRUE), jumps_n_weekly),
         jumps_height_weekly = 
           ifelse(is.na(jumps_height_weekly), 
                  mean(jumps_height_weekly, na.rm = TRUE), jumps_height_weekly))

cb_cens = l_cb_cens[[1]]
icen_fit = ic_par(Surv(enter, 
                       stop_cens, 
                       status_cens, 
                       type = "interval") ~ position + age + season + cb_cens +
                    jump_height_max + jumps_n_weekly, model = 'ph',
                  data = d_cens1)
summary(icen_fit)

icen_fit_interaction = ic_par(Surv(enter, 
                       stop_cens, 
                       status_cens, 
                       type = "interval") ~ jumps_n_weekly + cb_cens + jumps_n_weekly*cb_cens, model = 'ph',
                  data = d_cens1)

conf_icen = exp(confint(icen_fit))
icen_summary = summary(icen_fit)
d_icenfit = as_tibble(icen_summary$summaryParameters)
d_icenfit = d_icenfit %>% mutate(vars = names(icen_fit$coefficients), 
                                 ci_low = conf_icen[,1], ci_high = conf_icen[,2])

write_excel_csv(d_icenfit, "icen_fit.csv", delim = ";", na = "")

# make figures based on preds

pred_seq = seq(0, 250, 10)
n_preds = length(pred_seq)

pred_mat = data.frame()
for(i in pred_seq){
  vec = rep(i, 21)
  pred_mat = rbind(vec, pred_mat)
}
pred_mat = as.matrix(pred_mat)

qmat_example = l_q_mat_cens[[1]][1:n_preds,]
qmat_example[1:n_preds,] = pred_mat

cb_example = crossbasis(qmat_example, lag=c(lag_min, lag_max), 
           argvar = list(fun="ns", knots = c(10, 100, 150)),
           arglag = list(fun="poly", degree = 2))

d_preddata =  tibble(
  age = rep(30, n_preds),
  jump_height_max = rep(86, n_preds),
  match = as.factor(rep(0, n_preds)),
  id_player = rep(1, n_preds),
  position = rep("Setter", n_preds),
  cb_cens = cb_example,
  t_prevmatch = rep(6, n_preds),
  season = rep("2017/2018", n_preds),
  jumps_n_weekly = rep(360, n_preds)
)

preds = predict(icen_fit, newdata = d_preddata, type = "lp")
d_preds = bind_cols(jumps_n = rev(pred_seq), hr = preds)

library(lmisc) # loading local package for figure settings
# shared figure options
text_size = 14
ostrc_theme = theme(panel.border = element_blank(), 
                     panel.background = element_blank(),
                     panel.grid = element_blank(),
                     axis.line = element_line(color = nih_distinct[4]),
                     strip.background = element_blank(),
                     strip.text.x = element_text(size = text_size, family="Trebuchet MS", colour="black", face = "bold", hjust = -0.01),
                     axis.ticks = element_line(color = nih_distinct[4]))

devEMF::emf("cumhaz_freq_icenfit.emf", height = 4, width = 6)
ggplot(d_preds, aes( x = jumps_n, y = hr)) +
  geom_line()  +
  geom_line(size = 0.75, color = nih_distinct[4]) +
  theme_line(text_size) + 
  xlab("Daily jumps the previous 7 to 28 days") +
  ylab("Cumulative HR") +
  ostrc_theme 
dev.off()

#------------------- model for height
cb_cens_height = l_cb_cens_height[[1]]
icen_fit = ic_par(Surv(enter, 
                       stop_cens, 
                       status_cens, 
                       type = "interval") ~ position + age + season + cb_cens_height + +
                    jumps_height_weekly + weight, model = 'ph',
                  data = d_cens1)
summary(icen_fit)

icen_fit_interaction = ic_par(Surv(enter, 
                                   stop_cens, 
                                   status_cens, 
                                   type = "interval") ~ jumps_height_weekly + cb_cens_height + jumps_height_weekly*cb_cens_height, model = 'ph',
                              data = d_cens1)
summary(icen_fit_interaction)
#-----------------------------------EWMA method
library(slider)
# we need to calculate EWMA on the time before the censored injury interval
d_cens1 = d_cens1 %>% mutate(jumps_n_lag = lag(jumps_n, 7),
                             jumps_n_lag = ifelse(is.na(jumps_n_lag), mean(jumps_n_lag, na.rm = TRUE), jumps_n_lag))

# function for calculating exponentially waited moving averages
# using similar syntax as the RA-function
# an exponential smoothing ratio of 2/(n+1)
# same as in williams et al. 2016
ewma = function(x, n_days){
  TTR::EMA(x, n = n_days, wilder = FALSE)
}

# function calculates ewma on a sliding window of 21 days
slide_ewma = function(x){
  l = slide(x, ~ewma(., lag_max+1), .before = lag_max, step = 1, .complete = TRUE) %>% map(last)
  l = compact(l)
  l = unlist(l)
  l
}

# function to nest the exposure history data by each individual, 
# and run a user-specified function on each of their datasets in the list
function_on_list = function(d, FUN = NULL, day_start){
  nested_list = d %>% group_by(id_player) %>% nest()
  nested_list$data = nested_list$data %>% map(., ~FUN(.$jumps_n_lag))
  l_unnest = unnest(nested_list, cols = c(data)) %>% group_by(id_player) %>% 
    mutate(day = (lag_max+1):((n()) + (lag_max))) %>% ungroup()
  l_unnest
}

d_ewma = function_on_list(d_cens1, slide_ewma, lag_max+1) %>% rename(jumps_n_ewma = data)

# remove the first 21 rows for comparability of the AIC, 
# which requires the same sample size for all models

# fixme! per person?
d_cens1_mods = d_cens1 %>% filter(stop >= lag_max+1)
d_cens1_mods = d_cens1_mods %>% left_join(d_ewma, by = c("id_player", "stop" = "day"))

d_cens1_mods %>% filter(is.na(jumps_n_ewma)) %>% select(id_player, date, jumps_n_lag, jumps_n_ewma)


d_cens1_mods %>% filter(id_player == 19) %>% select(id_player, date, jumps_n_lag, jumps_n_ewma) %>% View()

icen_fit = ic_par(Surv(enter, 
                       stop_cens, 
                       status_cens, 
                       type = "interval") ~ jumps_n_weekly + 
                    jumps_n_ewma + 
                    jumps_n_weekly*jumps_n_ewma, model = 'ph',
                  data = d_cens1_mods)
summary(icen_fit)

conf_icen = exp(confint(icen_fit))
icen_summary = summary(icen_fit)
d_icenfit = as_tibble(icen_summary$summaryParameters)
d_icenfit = d_icenfit %>% mutate(vars = names(icen_fit$coefficients), 
                                 ci_low = conf_icen[,1], ci_high = conf_icen[,2])





fit_ewma = glm(injury ~ jumps_n_weekly + 
                 jumps_n_ewma + 
                 jumps_n_weekly*jumps_n_ewma, 
               family = "binomial", data = d_cens1)

# wald is only for faster computation
# use bootstrap for final model
parameters::parameters(fit_ewma, ci_method="wald", exponentiate = TRUE)

