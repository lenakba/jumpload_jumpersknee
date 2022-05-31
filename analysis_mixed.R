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
d_analysis = d_analysis %>% arrange(d_imp, id_player, date) %>%
  group_by(d_imp, id_player) %>%
  mutate(time = 1:n()) %>% ungroup()

d_selected = d_analysis %>% select(d_imp, id_player, date, time, jumps_n, inj_knee_filled,
                                   age, jump_height_max, position, match, t_prevmatch)
l_selected = (d_selected %>% group_by(d_imp) %>% nest())$data

# matrix of exposure histories
l_q_mat = l_selected %>% map(., ~tsModel::Lag(.$jumps_n, lag_min:lag_max))

# obtain the crossbasis
l_cb_dlnm =  l_q_mat %>% map(~crossbasis(., lag=c(lag_min, lag_max), 
                            argvar = list(fun="ns", knots = 3),
                            arglag = list(fun="ns", knots = 3)))

# -------perform mixed model

library(lme4)
# no frailty
l_fit = 
  map2(.x = l_selected,
       .y = l_cb_dlnm,
       ~glmer(inj_knee_filled ~ .y + position + age + 
                jump_height_max + match + t_prevmatch + ns(time, 3) + (1|id_player), 
              data = .x, family = "binomial"))

library(broom.mixed)
fit_pooled = summary(mice::pool(l_fit), conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate_if(., is.numeric, ~round(., 3))

#-----------------------------------------------figures

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
  map2(.x = l_fit,
       .y = l_cb_dlnm,
       ~crosspred(.y, .x, at = predvalues, cen = 0, cumul = TRUE))
glimpse(l_cp_preds_dlnm)

# function for plucking the right matrix out of the crosspred list within the list of crosspred lists
pluck_mat = function(x, pos){pluck(l_cp_preds_dlnm, x, pos)}
# the crosspred list has changed
allRRfit = 16
d_preds_cumul1 = pluck_mat(1, allRRfit)
d_preds_cumul2 = pluck_mat(2, allRRfit)
d_preds_cumul3 = pluck_mat(3, allRRfit)
d_preds_cumul4 = pluck_mat(4, allRRfit)
d_preds_cumul5 = pluck_mat(5, allRRfit)
l_cumulRRfit = list(d_preds_cumul1, d_preds_cumul2, d_preds_cumul3, d_preds_cumul4, d_preds_cumul5)
# average across preds
mat_cumulRRfit = reduce(l_cumulRRfit, `+`) / length(l_cumulRRfit)

# conflow
allRRfit_low = 17
d_preds_cumullow1 = pluck_mat(1, allRRfit_low)
d_preds_cumullow2 = pluck_mat(2, allRRfit_low)
d_preds_cumullow3 = pluck_mat(3, allRRfit_low)
d_preds_cumullow4 = pluck_mat(4, allRRfit_low)
d_preds_cumullow5 = pluck_mat(5, allRRfit_low)
l_cumulRRfit_low = list(d_preds_cumullow1, d_preds_cumullow2, d_preds_cumullow3, d_preds_cumullow4, d_preds_cumullow5)
# average across preds
mat_cumulRRfit_low = reduce(l_cumulRRfit_low, `+`) / length(l_cumulRRfit_low)

# confhigh
allRRfit_high = 18
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
ggplot(d_cumul, aes(x = jumps_n, y = value, group = 1)) +
  geom_ribbon(aes(min = ci_low, max = ci_high), alpha = 0.3, fill = nih_distinct[1]) +
  geom_hline(yintercept = 1, alpha = 0.3, size = 1) +
  geom_line(size = 0.75, color = nih_distinct[4]) +
  theme_base(text_size) +
  ostrc_theme +
  xlab("N Jumps") +
  ylab("Cumulative OR on Day 0")


# 13 is matRRfit
matRRfit = 13
d_preds1 = pluck_mat(1, matRRfit)
d_preds2 = pluck_mat(2, matRRfit)
d_preds3 = pluck_mat(3, matRRfit)
d_preds4 = pluck_mat(4, matRRfit)
d_preds5 = pluck_mat(5, matRRfit)
l_matRRfit = list(d_preds1, d_preds2, d_preds3, d_preds4, d_preds5)
# average across preds
mat_matRRfit = reduce(l_matRRfit, `+`) / length(l_matRRfit)

# conflow
matRRfit_low = 14
d_preds_low1 = pluck_mat(1, matRRfit_low)
d_preds_low2 = pluck_mat(2, matRRfit_low)
d_preds_low3 = pluck_mat(3, matRRfit_low)
d_preds_low4 = pluck_mat(4, matRRfit_low)
d_preds_low5 = pluck_mat(5, matRRfit_low)
l_matRRfit_low = list(d_preds_low1, d_preds_low2, d_preds_low3, d_preds_low4, d_preds_low5)
# average across preds
mat_matRRfit_low = reduce(l_matRRfit_low, `+`) / length(l_matRRfit_low)

# confhigh
matRRfit_high = 15
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

persp(x = predvalues, y = lag_seq, mat_matRRfit, ticktype="detailed", 
      theta=230, ltheta=150, phi=40, lphi=30,
      ylab="Lag (Days)", zlab="HR", shade=0.75, 
      r=sqrt(3), d=5, cex.axis=1.2, cex.lab=1.2,
      border=grey(0.2), col = nih_distinct[1], 
      xlab = "N jumps", main = "3D plane of effects")

# exposure-response curve for lag 0
lag_fixed = "lag0"
colnumber = which(colnames(mat_matRRfit) == lag_fixed)
d_preds_per_jump = as_tibble(mat_matRRfit[,colnumber]) %>% 
  rename(coef = value) %>% 
  mutate(jumps_n = predvalues,
         ci_low = mat_matRRfit_low[,colnumber],
         ci_high = mat_matRRfit_high[,colnumber])

ggplot(d_preds_per_jump, aes(x = jumps_n, y = coef, group = 1)) +
  geom_hline(yintercept = 1, alpha = 0.3, size = 1) +
  geom_ribbon(aes(min = ci_low, max = ci_high), alpha = 0.3, fill = nih_distinct[1]) +
  geom_line(size = 0.75, color = nih_distinct[4]) +
  theme_base(text_size) +
  ostrc_theme +
  xlab("N jumps") +
  ylab("OR on Day 0")

lag_fixed = "lag15"
colnumber = which(colnames(mat_matRRfit) == lag_fixed)
d_preds_per_jump = as_tibble(mat_matRRfit[,colnumber]) %>% 
  rename(coef = value) %>% 
  mutate(jumps_n = predvalues,
         ci_low = mat_matRRfit_low[,colnumber],
         ci_high = mat_matRRfit_high[,colnumber])

ggplot(d_preds_per_jump, aes(x = jumps_n, y = coef, group = 1)) +
  geom_hline(yintercept = 1, alpha = 0.3, size = 1) +
  geom_ribbon(aes(min = ci_low, max = ci_high), alpha = 0.3, fill = nih_distinct[1]) +
  geom_line(size = 0.75, color = nih_distinct[4]) +
  theme_base(text_size) +
  ostrc_theme +
  xlab("N jumps") +
  ylab("OR on Day 15") 

lag_fixed = "lag27"
colnumber = which(colnames(mat_matRRfit) == lag_fixed)
d_preds_per_jump = as_tibble(mat_matRRfit[,colnumber]) %>% 
  rename(coef = value) %>% 
  mutate(jumps_n = predvalues,
         ci_low = mat_matRRfit_low[,colnumber],
         ci_high = mat_matRRfit_high[,colnumber])

ggplot(d_preds_per_jump, aes(x = jumps_n, y = coef, group = 1)) +
  geom_hline(yintercept = 1, alpha = 0.3, size = 1) +
  geom_ribbon(aes(min = ci_low, max = ci_high), alpha = 0.3, fill = nih_distinct[1]) +
  geom_line(size = 0.75, color = nih_distinct[4]) +
  theme_base(text_size) +
  ostrc_theme +
  xlab("N jumps") +
  ylab("OR on Day 27") 