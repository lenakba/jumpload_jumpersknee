library(tidyverse) # data wrangling
library(dlnm) # distributed lag non-linear models
library(lubridate) # to manipulate dates
library(splines) # natural splines
library(lme4) # for mixed models
library(merTools) # to pool fits with Ruben's rules on mixed models

# so we don't have to deal with scientific notations
# and strings aren't automatically read as factors:
options(scipen = 30, 
        stringsAsFactors = FALSE)

data_folder = "O:\\Prosjekter\\Bache-Mathiesen-Biostatistikk\\Data\\volleyball\\"
d_jumpload = readRDS(paste0(data_folder, "d_jumpload_multimputed_daily10.rds"))

# we want the setter to be the reference value
# when we later assess it as an independent variable in a model
d_jumpload = d_jumpload %>% 
  mutate(position_num = case_when(position == "Setter" ~ 0,
                                  position == "Opposite" ~ 1,
                                  position == "Outside" ~ 2,
                                  position == "Middle" ~ 3),
         position_num = factor(position_num))

# objects with 
# key columns
# adjustment variables (conf = confounders)
# jump load variables
key_cols = c("date", "id_player", "id_team", "id_team_player", "id_season")
conf_cols = c("age", "position_num", "weight")
jump_cols = c("jump_height_sum", "jumps_n", "jumps_n_weekly", 
              "jumps_height_weekly", "jump_height_max", "jump_height_perc_sum", "load_index_KE")

# define the min and max lag
# we will lag the data, so lag 0
# corresponds to the last day before the OSTRC week
lag_min = 0
lag_max = 20

# select columns that may be useful
d_analysis = d_jumpload %>% 
  dplyr::select(all_of(key_cols), 
                all_of(jump_cols),
                starts_with("knee"),
                starts_with("inj"), 
                year, month_day, season, 
                preseason,
                height, all_of(conf_cols),
                session_type, match = Match, 
                t_prevmatch, game_type, match_sets_n = MatchSets,
                d_imp = .imp)

d_analysis = d_analysis %>% mutate_at(vars(starts_with("inj"), starts_with("knee")), ~as.numeric(.))

# function to find event intervals and append them to a dataset
# here, status = 0 is an event, it is recovery to an asymptomatic state
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
    dplyr::select(-not_unique_id)
  d_time_to_sympt
}

# from any symptoms to asymptomatic
d_analysis_selected = d_analysis  %>% 
  dplyr::select(all_of(key_cols), d_imp, id_player, season, date, inj_knee_filled, 
                all_of(conf_cols), 
                all_of(jump_cols))

d_analysis_selected = d_analysis_selected %>% mutate(inj_knee_unfilled = inj_knee_filled)
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

# add event id to separate intervals
# the intervals in which the player has symptoms, and "risks" becoming symptom-free
d_surv = d_surv %>% group_by(d_imp, id_player, season) %>% 
  mutate(status = ifelse(lag(inj_knee_filled) == 1 & 
                           inj_knee_filled == 0, 1, 0),
         status = ifelse(is.na(status), 0, status)) %>% ungroup()
d_surv = d_surv %>% add_event_id(status)

# but also make sure that a new interval starts at the start of each season
# (id_event does not take this into account)
# confusingly, I called this id_dlnm, to be used in the DLNM calculation later
# however, we decided to include training load data from the previous interval 
# (if it was in the same season) in the DLNM calculation, 
# and this wasn't used for DLNM after all
# We still use id_dlnm later on to delineate each individual interval for other reasons
d_surv = d_surv %>% mutate(id_dlnm = paste0(id_player, "-", season, "-", id_event),
                           inj_knee_filled_fixed = ifelse(status == 0, 2, inj_knee_filled))

# make a variable that is 0 if the previous interval was in the previous season
# and 1 if the previous interval was a bout of symptoms
d_first_interval_season = d_surv %>%  
  group_by(id_player) %>% 
  arrange(id_season, id_event) %>% 
  distinct(id_season, .keep_all = TRUE) %>% 
  dplyr::select(id_player, id_dlnm) %>% 
  arrange(id_player, id_dlnm) %>% 
  mutate(prev_symptoms = 0) %>% ungroup()

d_surv = d_surv %>% 
  left_join(d_first_interval_season, by = c("id_player", "id_dlnm")) %>% 
  mutate(prev_symptoms = ifelse(is.na(prev_symptoms), 1, prev_symptoms))

#--------------------------------------- calculating EWMA
nsub = nrow(d_surv %>% distinct(id_player))

# function for calculating exponentially waited moving averages
# an exponential smoothing ratio of 2/(n+1)
# same as in williams et al. 2016
ewma = function(x, n_days){
  TTR::EMA(x, n = n_days, wilder = FALSE)
}

library(slider)
# function calculates ewma on a sliding window of 21 days (lag_max + 1)
slide_ewma = function(x){
  l = slide(x, ~ewma(., lag_max+1), .before = lag_max, step = 1, .complete = TRUE) %>% map(last)
  l = compact(l)
  l = unlist(l)
  l
}

# function to nest the exposure history data by each individual, 
# and run a user-specified function on each of their datasets in the list
function_on_list = function(d, FUN = NULL){
  nested_list = d %>% group_by(d_imp, id_player, season) %>% nest()
  nested_list$data = nested_list$data %>% map(., ~FUN(.$jump_height_perc_sum_lag7))
  l_unnest = unnest(nested_list, cols = c(data)) %>% group_by(d_imp, id_player, season) %>% 
    mutate(day = lag_max+1:n()) %>% ungroup()
  l_unnest
}


d_surv = d_surv %>% mutate(jumps_height_weekly_lag = lag(jumps_height_weekly),
                           jumps_height_weekly_lag = ifelse(is.na(jumps_height_weekly_lag), 
                                                            mean(jumps_height_weekly_lag, na.rm = TRUE), jumps_height_weekly_lag)
)


nested_list = d_surv %>% group_by(d_imp, id_player) %>% nest()
nested_list$data = nested_list$data %>% map(., ~slide_ewma(.$jumps_height_weekly_lag))
l_unnest = unnest(nested_list, cols = c(data)) %>% group_by(d_imp, id_player) %>% 
  mutate(day = lag_max+1:n()) %>% ungroup()
d_ewma = l_unnest %>% rename(jump_load_ewma = data)

# attach to surv data
d_surv = d_surv %>% left_join(d_ewma, by = c("d_imp", "id_player", "stop" = "day"))

# we have to remove intervals of symptom-free before starting a new
# interval with week = 1
# but, the final week needs the weekly sum from the last date, not the first
# this is why we calculate the lead for this week
d_weekly = d_surv %>%
  dplyr::select(d_imp, all_of(key_cols), 
                id_event, id_dlnm, date, season, 
                jumps_height_weekly, jump_height_perc_sum, jump_load_ewma, 
                status, inj_knee_filled, prev_symptoms, 
                all_of(conf_cols), stop, inj_knee_unfilled) %>% 
  group_by(d_imp, id_player) %>%
  mutate(jumps_height_weekly_lead = lead(jumps_height_weekly, 6)) %>%
  ungroup() %>%
  filter(!(status == 0 & inj_knee_filled == 0)) %>% 
  group_by(d_imp, id_dlnm) %>% mutate(week = difftime(max(date)+7, date, units = "weeks"),
                                      week = as.numeric(round(rev(week))),
                                      day = 1:n()) %>% ungroup()

d_weekly = d_weekly %>% mutate(id_player = as.character(id_player))
d_weekly = d_weekly %>% dplyr::select(-stop)

# we imputed injuries to make it easier to calculate the DLNM on training load
# (though we ran EWMA instead)
# but we won't impute the missing data in our final analysis
# we remove the imputed values here
d_weekly_unimputed = d_weekly %>%
  mutate(status = case_when(is.na(inj_knee_unfilled) ~ NA_real_, TRUE ~ status)) 
# model without DLNM

# run all fits on the 5 imputed datasets and pool results with Ruben's rules
l_nested = d_weekly_unimputed %>% group_by(d_imp) %>% nest()
l_fits = l_nested$data %>% map(., ~glmer(status ~ ns(week, 4) + ns(jumps_height_weekly, 3) + 
                                           jump_load_ewma + 
                                           position_num + age + weight + (1|id_player), 
                                         data = .,
                                         family=binomial(link="cloglog")))

fit_pooled = summary(mice::pool(l_fits), conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate_if(., is.numeric, ~round(., 3))
write_excel_csv(fit_pooled, paste0("symptomatic_to_asymptomatic_ewma.csv"), delim = ";", na = "")

#------------------------------------------ Figures

library(lme4) # for mixed models
fit1 = glmer(status ~ ns(week, 4) + ns(jumps_height_weekly, 3) + jump_load_ewma + 
               position_num + age + weight + (1|id_player), 
             data = d_weekly_unimputed %>% filter(d_imp == 1),
             family = binomial(link="cloglog"))
parameters::parameters(fit1, exponentiate = TRUE)
AIC(fit1)

# find distributions
hist((d_weekly_unimputed %>% filter(d_imp==1))$jumps_height_weekly)
dense = density((d_weekly_unimputed %>% filter(d_imp==1))$jumps_height_weekly, na.rm = TRUE)
d_dense = bind_cols(x = dense$x, predicted = dense$y*1000) %>% filter(x > 0)

hist((d_weekly_unimputed %>% filter(d_imp==1))$week)
dense_week = density((d_weekly_unimputed %>% filter(d_imp==1))$week, na.rm = TRUE)
d_dense_week = bind_cols(x = dense_week$x, predicted = dense_week$y)

# make figures showing the risk per level of jump load
# and for each number of weeks
library(ggeffects)
library(sjPlot)
preds_jumph = ggpredict(
  fit1, "jumps_height_weekly [all]", 
  condition = c(age = 26.1, position = "Outside", id_player = "1", week = 3),
  vcov.fun = "vcovCR", 
  vcov.type = "CR0", 
  vcov.args = list(id_player = unique((d_weekly_dist %>% filter(d_imp == 1))$id_player)),
  type = "re.zi") %>% as_tibble()

preds_week = ggpredict(
  fit1, "week [all]", 
  condition = c(age = 26.1, position = "Outside", id_player = "1", jumps_height_weekly = 2724),
  vcov.fun = "vcovCR", 
  vcov.type = "CR0", 
  vcov.args = list(id_player = unique((d_weekly_dist %>% filter(d_imp == 1))$id_player)),
  type = "re.zi") %>% as_tibble()

# what was the EWMA reference value?
d_weekly_unimputed %>% filter(d_imp == 1) %>% summarise(mean(jump_load_ewma, na.rm = TRUE))


library(lmisc)
text_size = 16
ostrc_theme =  theme(panel.border = element_blank(), 
                     panel.background = element_blank(),
                     panel.grid = element_blank(),
                     axis.line = element_line(color = nih_distinct[4]),
                     strip.background = element_blank(),
                     strip.text.x = element_text(size = text_size, 
                                                 family="Trebuchet MS", 
                                                 colour="black", 
                                                 face = "bold", hjust = -0.01),
                     axis.ticks = element_line(color = nih_distinct[4]))

plot_load = ggplot(preds_jumph, aes(x = x, y = predicted, group = 1)) + 
  geom_area(data = d_dense, alpha = 0.3, fill = nih_distinct[1]) +
  geom_line(size = 0.8) +
  theme_line(text_size) +
  ostrc_theme +
  xlab("Weekly jump load (arb. u)") +
  ylab("Probability of asymptomatic") +
  #scale_y_continuous(labels = axis_percent, limits = c(NA, 0.12)) +
  scale_y_continuous(labels = axis_percent) +
  theme(
    plot.margin = margin(0.5, 0, 0.2, 0, "cm")
  )

plot_weeks = ggplot(preds_week, aes(x = x, y = predicted, group = 1)) + 
  geom_area(data = d_dense_week, alpha = 0.3, fill = nih_distinct[1]) +
  geom_line(size = 0.8) + 
  theme_line(text_size) +
  ostrc_theme +
  xlab("Number of weeks with symptoms") +
  ylab("Probability of asymptomatic") +
  scale_y_continuous(labels = axis_percent, limits = c(NA, 0.12)) +
  theme(
    plot.margin = margin(0.5, 0.2, 0.2, 0, "cm")
  )

library(devEMF)
emf("figure4_predicted_probs_asymptomatic.emf", height = 4, width = 12)
ggpubr::ggarrange(plot_load, plot_weeks, labels = "AUTO")
dev.off()

cairo_pdf("figure4_predicted_probs_asymptomatic.pdf", width = 12, height = 4)
ggpubr::ggarrange(plot_load, plot_weeks, labels = "AUTO")
dev.off()

png("figure4_predicted_probs_asymptomatic.png", width = 12, height = 4, unit = "in", res = 600)
ggpubr::ggarrange(plot_load, plot_weeks, labels = "AUTO")
dev.off()
