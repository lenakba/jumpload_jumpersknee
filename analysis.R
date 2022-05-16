library(tidyverse)
library(dlnm)
library(slider)

data_folder = "D:\\phd\\jump load\\data\\"

d_all = read_delim(paste0(data_folder, "d_volleyball.csv"), delim = ";", na = "")


d_all %>% select(starts_with("knee"))

# calculate weekly sums 
slide_sum = function(x){
  l = slide(x, ~sum(.), .before = 6, step = 7, .complete =TRUE)
  l = unlist(l)
  l
}

nested_list = d_all %>% group_by(PlayerID) %>% nest()
nested_list$data = nested_list$data %>% map(., ~slide_sum(.$jumps_n))
d_unnest = unnest(nested_list, cols = c(data)) %>% ungroup()  %>% mutate(index = 1:n())

# fetch dates in which the OSTRC was collected
d_all = d_all %>% mutate(ostrc_day = ifelse(is.na(Knee_Total), 0, 1))
d_ostrc_dates = d_all %>% group_by(PlayerID) %>% filter(ostrc_day == 1) %>% mutate(index = 1:n()) %>% select(PlayerID, Date, index)

d_weekly_load = d_ostrc_dates %>% 
  left_join(d_unnest, by = c("PlayerID", "index")) %>% 
  rename(jumps_n_weekly = data) %>% 
  select(-index)
d_weekly = d_all %>% left_join(d_weekly_load, by = c("PlayerID", "Date"))

# check that its correct
d_weekly %>% select(Date, PlayerID, jumps_n, jumps_n_weekly)

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
