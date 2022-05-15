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

nested_list_date = d_all %>% group_by(PlayerID) %>% nest()
nested_list_date$data = nested_list_date$data %>% 
                   map(. %>% slice(-(1:6)) %>% 
                       slice(which(row_number() %% 7 == 1)))
d_unnest_date = unnest(nested_list_date, cols = c(data)) %>% ungroup() %>%
  select(PlayerID, Date) %>% mutate(index = 1:n())

d_weekly_load = d_unnest_date %>% left_join(d_unnest, by = c("PlayerID", "index")) %>% rename(weekly_jump_n = data) %>% select(-index)

d_weekly = d_all %>% left_join(d_weekly_load, by = c("PlayerID", "Date"))


first_sum = 1 525  
slide_sum(ting$jumps_n)

l = slide(ting$jumps_n, ~sum(.), .before = 6, step = 7, .complete = TRUE)
unlist(l)  



# load data summed per week, but with past load data available
d_all 

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
