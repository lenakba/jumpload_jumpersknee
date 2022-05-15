library(tidyverse)
library(readxlsb)

data_folder = "D:\\phd\\jump load\\data\\"

d_all = read_xlsb(paste0(data_folder, "Final Data Set_Daily_Weekly_Pre.xlsb"), "FDS_Daily_Pre_All", na = "", skip = 4, locale = locale(date_format = "%d.%m.%Y", encoding = "UTF-8"))
d_all = d_all %>% tibble()

# need to convert integer to date, excel origin date is 1899-12-30
d_all$Date = as.Date(d_all$Date,format="%Y-%m-%d", origin = "1899-12-30")

# fix so "-" means NA
d_all = d_all %>%mutate_if(is.character, funs(ifelse(. == "-", is.na(.), .)))

# add match variable and fix missing session-types
d_all = d_all %>%mutate(Match = as.character(ifelse(SessionType == "match", 1, 0)))
d_all = d_all %>%mutate(SessionType = ifelse(is.na(SessionType), "no volleyball", SessionType))

# calculate sum of jump heights
d_all = d_all %>%mutate(jump_height_sum = jump_height_avg_cm*jumps_n)

# fill OSTRC questionnaires up so that they eprtain for a whole week
d_all = d_all %>% 
  fill(starts_with("knee"), 
       starts_with("Shoulder"), 
       starts_with("LowBack"), 
       .direction = "up")

# only questions 2 and 3 pertain to substantial injuries, 
# and only if they answered reply number 3 or 4
# our data is structured for severity scores, 
# and have the options 0, 8, 17, 25, equaling response number 1, 2, 3, or 4
d_all = d_all %>% mutate(inj_knee = ifelse(Knee_1 >= 1| Knee_2 >= 1 | Knee_3 >= 1 | Knee_4 >= 1, 1, 0),
                 inj_knee_subst = ifelse(Knee_2 >= 17 | Knee_3 >= 17, 1, 0))


# write csv to read in other scripts
# write .csv
# write_delim is preferable, but write_excel_csv is required for excel to understand
# that the file encoding is UTF-8
# write_excel_csv(d_all, paste0(data_folder, "d_volleyball.csv"), delim = ";", na = "")


# checking that no jump height sums are unrealistically extreme
d_jump_height_test = d_all %>% select(Date, PlayerID, jumps_n, jump_height_sum) %>% 
                               mutate(max_height_possible = jumps_n * 120,
                               within_limits_no = ifelse(jump_height_sum <= max_height_possible, 0, 1))
n_errors = d_jump_height_test %>% summarise(n_errors = sum(within_limits_no == 1, na.rm = TRUE))

# look at histograms. Any negative values or other abnormalities?
hist(d_all$jumps_n)
hist(d_all$jump_height_sum)
hist(d_all$age)
hist(d_all$TotalDailyKELoadIndex)

d_all %>% count(Team)
d_all %>% count(Position)
d_all %>% count(SessionType)
d_all %>% count(Season.Phase)
d_all %>% count(MatchParticipation) 
  
# trends. Does there seem to be a season effect?
# only looking at days that players play volleyball (match or training)
d_mean_day = d_all %>% filter(SessionType != "no volleyball") %>% 
              group_by(Date) %>% summarise(mean_jumps = mean(jumps_n), 
                                           mean_height = mean(jump_height_sum, na.rm = TRUE))


ggplot(d_mean_day, aes(x = Date, y = mean_jumps)) +
  geom_line()

ggplot(d_mean_day, aes(x = Date, y = mean_height)) +
  geom_line()

# do matches have higher/more frequent than other training?
d_mean_day_match = d_all %>% 
                    filter(SessionType != "no volleyball") %>% group_by(Date, Match) %>% 
                    summarise(mean_jumps = mean(jumps_n), 
                              mean_height = mean(jump_height_sum, na.rm = TRUE))


ggplot(d_mean_day_match, aes(x = Date, y = mean_jumps, group = Match)) +
  geom_line()


d_mean_match = d_all %>% 
  filter(SessionType != "no volleyball") %>% group_by(Match) %>% 
  summarise(mean_jumps = mean(jumps_n), 
            mean_height = mean(jump_height_sum, na.rm = TRUE)) %>% ungroup()

# fewer and lower jumping during match? 
ggplot(d_mean_match, aes(y = Match, x = mean_jumps)) +
  ggstance::geom_barh(stat = "identity", color = "black")

ggplot(d_mean_match, aes(y = Match, x = mean_height)) +
  ggstance::geom_barh(stat = "identity", color = "black")

# what happens if we remove preseason?

d_mean_match = d_all %>% 
  filter(SessionType != "no volleyball", Season.Phase != "Preseason") %>% group_by(Match) %>% 
  summarise(mean_jumps = mean(jumps_n), 
            mean_height = mean(jump_height_sum, na.rm = TRUE)) %>% ungroup()

# fewer and lower jumping during match?
# effect not as extreme
ggplot(d_mean_match, aes(y = Match, x = mean_jumps)) +
  ggstance::geom_barh(stat = "identity", color = "black")

ggplot(d_mean_match, aes(y = Match, x = mean_height)) +
  ggstance::geom_barh(stat = "identity", color = "black")

