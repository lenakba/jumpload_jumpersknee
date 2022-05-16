library(tidyverse)
library(dlnm)
library(slider)

data_folder = "D:\\phd\\jump load\\data\\"

d_all = read_delim(paste0(data_folder, "d_volleyball.csv"), delim = ";", na = "")

#------------------------------weekly training load sums-------------------

# First, we calculate weekly sums of number of jumps and jump heights
# This is because the OSTRC injury questionnaires were collected weekly.
# This means we cannot separate the effect of a single day in the current/acute week 
# from other days, even though the jumps were collected daily.
# The biggest limitation is that the current day of training has special properties
# compared to past training, also past training in the current week.
# We can still consider the effects of past daily training load before the current week.

# function to calculate sums across a user-specified window size
slide_sum = function(x, window_size){
  l = slide(x, ~sum(.), .before = 6, step = window_size, .complete =TRUE)
  vector = unlist(l)
  v_calc = vector[seq(1, length(vector), window_size)]
  v_calc
}

# make window object to designated length (7 days)
window = 7

# First step is to test that our automated function 
# for calculating weekly sums (slide_sum) calculates correctly. 
example_player = d_all %>% filter(PlayerID == 1) %>% select(Date, jumps_n, PlayerID)

# manual calculation of first 3 weekly sums
v_correct = c(
  example_player %>% slice(1:7) %>% summarize(the_sum = sum(jumps_n)) %>% pull(the_sum),
  example_player %>% slice(8:14) %>% summarize(the_sum = sum(jumps_n)) %>% pull(the_sum),
  example_player %>% slice(15:21) %>% summarize(the_sum = sum(jumps_n)) %>% pull(the_sum)
)

# function above used to calculate weekly sums
test_loads = (example_player %>% pull(jumps_n))[1:21]
v_calc = slide_sum(test_loads, window)

# test that the output from the function equals the manual calculation
testthat::expect_equal(v_correct, v_calc)

# Next step is to perform the calculation on the data
# Fetch dates in which the OSTRC was collected
d_all = d_all %>% mutate(ostrc_day = ifelse(is.na(Knee_Total), 0, 1))
d_ostrc_dates = d_all %>% group_by(PlayerID) %>% filter(ostrc_day == 1) %>% pull(Date)

# We nest on each player in the data so that weekly sum-calculation
# does not slide over different players
nested_list = d_all %>% group_by(PlayerID) %>% nest()
l_weekly_sums = nested_list$data %>% map(., ~slide_sum(.$jumps_n, window))

# we combine the sums to the original data
# but we only include the dates where OSTRC was collected
nested_list$data %>% 
  map(. %>% filter(Date %in% d_ostrc_dates) %>% select(Date)) %>% 
  map2(.x = ., .y = l_weekly_sums, ~.x %>% mutate(jumps_n_weekly = .y))


slide_sum(example_dude$jumps_n)







l_weekly_sums[[1]]


(nested_list$data %>% 
  map(. %>% filter(Date %in% d_ostrc_dates) %>% select(Date)))[[1]]


?map2

nested_list$data = nested_list$data %>% map(., ~slide_sum(.$jumps_n))


d_weekly %>% select(Date, jumps_n, jumps_n_weekly, PlayerID) %>% filter(PlayerID == 1, !is.na(jumps_n_weekly))



d_unnest = unnest(nested_list, cols = c(data)) %>% mutate(index = 1:n()) %>% ungroup()

d_unnest %>% filter(PlayerID == 2)









d_weekly_load = d_ostrc_dates %>% 
  left_join(d_unnest, by = c("PlayerID", "index")) %>% 
  rename(jumps_n_weekly = data) %>% 
  select(-index)
d_weekly = d_all %>% left_join(d_weekly_load, by = c("PlayerID", "Date"))

# test that the weekly sums are aligned with the weekly OSTRC questionnaire in the data
d_weekly %>% select(Date, PlayerID, jumps_n, jumps_n_weekly, inj_knee) %>% filter(!is.na(inj_knee)) %>% filter(is.na(jumps_n_weekly))



d_weekly %>% select(Date, PlayerID, jumps_n, jumps_n_weekly, inj_knee) %>% filter(PlayerID == 2)

# load data summed per week, but with past load data available


# injury data structered in intervals
d_all %>% select(Date, starts_with("inj"))

# fill OSTRC questionnaires up so that they pertain for a whole week
d_all = d_all %>% 
  fill(starts_with("knee"), 
       starts_with("Shoulder"), 
       starts_with("LowBack"), 
       .direction = "up")


library(icenReg)
data(miceData)
