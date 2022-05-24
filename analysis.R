library(tidyverse)
library(dlnm)
library(slider)

data_folder = "D:\\phd\\jump load\\data\\"

d_all = read_delim(paste0(data_folder, "d_volleyball.csv"), delim = ";", na = "")
key_cols = c("date", "id_player", "id_team", "id_team_player", "id_season")

# fixme! use mean imputation for the missing weights, for now
d_all = d_all %>% mutate(weight = ifelse(is.na(weight), mean(weight), weight))

#------------------------------weekly training load sums-------------------

# First, we calculate weekly sums of number of jumps and jump heights
# This is because the OSTRC injury questionnaires were collected weekly.
# This means we cannot separate the effect of a single day in the current/acute week 
# from other days, even though the jumps were collected daily.
# The biggest limitation is that the current day of training has special properties
# compared to past training, also past training in the current week.
# We can still consider the effects of past daily training load before the current week.

# function to calculate sums across a user-specified window size
slide_sum = function(x, window_size, complete){
  l = slide(x, ~sum(.), .before = 6, step = window_size, .complete = complete)
  vector = unlist(l)
  v_calc = vector[seq(1, length(vector), window_size)]
  v_calc
}

# make window object to designated length (7 days)
window_7 = 7

# First step is to test that our automated function 
# for calculating weekly sums (slide_sum) calculates correctly. 
example_player = d_all %>% filter(id_player == 1) %>% select(date, jumps_n, id_player, inj_knee)

# manual calculation of first 3 weekly sums
v_correct = c(
  example_player %>% slice(1:7) %>% summarize(the_sum = sum(jumps_n)) %>% pull(the_sum),
  example_player %>% slice(8:14) %>% summarize(the_sum = sum(jumps_n)) %>% pull(the_sum),
  example_player %>% slice(15:21) %>% summarize(the_sum = sum(jumps_n)) %>% pull(the_sum)
)

# function above used to calculate weekly sums
test_loads = (example_player %>% pull(jumps_n))[1:(window_7*3)]
v_calc = slide_sum(test_loads, window_7, TRUE)

# test that the output from the function equals the manual calculation
testthat::expect_equal(v_correct, v_calc)

# Next step is to perform the calculation on the data
# We nest on each player in the data so that weekly sum-calculation
# does not slide over different players
window = 1
nested_list = d_all %>% group_by(id_player) %>% nest()
nested_list$data = nested_list$data %>% map(., ~slide_sum(.$jumps_n, window, FALSE))
d_weekly_load = unnest(nested_list, cols = c(data)) %>% 
                ungroup() %>% mutate(index = 1:n()) %>% 
                rename(jumps_n_weekly = data)

d_all = d_all %>% mutate(index = 1:n()) %>% left_join(d_weekly_load, by = c("id_player", "index"))

# repeat for jump height
nested_list = d_all %>% group_by(id_player) %>% nest()
nested_list$data = nested_list$data %>% map(., ~slide_sum(.$jump_height_sum, window, FALSE))
d_weekly_load = unnest(nested_list, cols = c(data)) %>% 
  ungroup() %>% mutate(index = 1:n()) %>% 
  rename(jumps_height_weekly = data)

d_all = d_all %>% mutate(index = 1:n()) %>% left_join(d_weekly_load, by = c("id_player", "index"))
d_all = d_all %>% mutate(preseason = ifelse(season_phase == "Preseason", 1, 0))
d_analysis = d_all %>% 
             select(key_cols, 
                 jumps_n, 
                 jumps_n_weekly,
                 jump_height_sum,
                 jumps_height_weekly,
                 jump_height_max,
                 TotalDailyKELoadIndex,
                 starts_with("inj"), 
                 year, month_day, season, 
                 preseason,
                 match = Match, t_prevmatch,
                 age, position, inj_bl, 
                 session_type,
                 weight)

# fill OSTRC questionnaires up so that they pertain for a whole week
# d_all = d_all %>% 
#   fill(starts_with("knee"), 
#        starts_with("Shoulder"), 
#        starts_with("LowBack"), 
#        .direction = "up")


library(icenReg)
data(miceData)
