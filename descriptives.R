library(tidyverse)
library(readxl)

data_folder = "O:\\Prosjekter\\Bache-Mathiesen-Biostatistikk\\Data\\volleyball\\"
key_cols = c("date", "id_player", "id_team", "id_team_player", "id_season")

# read datasets
d_full = read_delim(paste0(data_folder,"d_volleyball.csv"), delim = ";", na = "")
d_daily = read_delim(paste0(data_folder,"d_jump_daily.csv"), delim = ";", na = "")

#------------------------------------------ injury descriptives

# number of knee complaint cases
d_full %>% 
  arrange(desc(inj_knee)) %>% 
  distinct(date, id_player, .keep_all = TRUE) %>% summarise(sum(inj_knee == 1, na.rm = TRUE),
                                                            sum(inj_knee == 0, na.rm = TRUE))

# how many players have symptoms?
d_knee_perplayer = d_full %>% count(id_player, inj_knee) 

d_knee_perplayer %>% group_by(id_player) %>% 
  summarise(injured = sum(inj_knee == 1, na.rm = TRUE)) %>% 
  count(injured)

# number of players who had complaints for more than 1 week
d_knee_perplayer %>% filter(inj_knee == 1) %>% summarise(sum(n>=2))

n_weeks_perplayer = d_knee_perplayer %>% filter(!is.na(inj_knee)) %>% group_by(PlayerID) %>% summarise(denom = sum(n))

mean_weeks_complaints = d_knee_perplayer %>% 
  filter(inj_knee == 1) %>% 
  left_join(n_weeks_perplayer, by = "PlayerID") %>% 
  mutate(prop = n/denom, mean_prop = mean(prop))

# how many substantial injuries
inj_knee_subst_filled

d_full %>% select(inj_knee_subst)

d_full %>% 
  arrange(desc(inj_knee_subst)) %>% 
  distinct(date, id_player, .keep_all = TRUE) %>% 
  summarise(sum(inj_knee_subst == 1, na.rm = TRUE),
            sum(inj_knee_subst == 0, na.rm = TRUE))


# how many our missing the OSTRC questionnaire?
d_ostrc_completed = d_all %>% select(starts_with("completed")) %>% filter(!is.na(completed_team_ostrc))
nrow(d_ostrc_completed)

# missing on completed variable means that the response was missing
d_ostrc_completed %>% filter(is.na(completed_today_Knee_OSTRC))

# only questions 2 and 3 pertain to substantial injuries, 
# and only if they answered reply number 3 or 4
# our data is structured for severity scores, 
# and have the options 0, 8, 17, 25, equaling response number 1, 2, 3, or 4

# number of days with jumper's knee
d_all %>% summarise(sum(inj_knee == 1, na.rm = TRUE))

#------------------------------------------------jump load descriptives
d_daily_nomissing = d_daily %>% filter(!is.na(jumps_n), session_type != "no volleyball")

# function for calculating mean, median etc. of anything
calc_descs = function(d, var){
  var = enquo(var)
  
  d %>% summarise(mean = mean(!!var, na.rm = TRUE), 
                  sd = sd(!!var, na.rm = TRUE),
                  median = median(!!var, na.rm = TRUE), 
                  iqr = IQR(!!var, na.rm = TRUE),
                  max = max(!!var, na.rm = TRUE))
}

calc_descs(d_daily_nomissing, jump_height_max) 
calc_descs(d_daily_nomissing, jumps_n) 
calc_descs(d_daily_nomissing, jump_height_sum) 
calc_descs(d_daily_nomissing, jump_height_perc_sum)

d_daily_nomissing %>% group_by(season_phase) %>% calc_descs(., jumps_n)
d_daily_nomissing %>% group_by(preseason) %>% calc_descs(., jumps_n)
d_daily_nomissing %>% group_by(Match) %>% calc_descs(., jumps_n)
d_daily_nomissing %>% filter(season_phase != "Preseason") %>% group_by(Match) %>% calc_descs(., jumps_n)

#------------------------------------------------player characteristics

d_player = d_daily %>% distinct(id_player, .keep_all = TRUE)

# number of players
nrow(d_player)

# number of player-seasons
d_daily %>% distinct(id_player, season) %>% nrow()

# 49, 36, 51, 48, 20 missing weight?
d_player %>% select(id_player, weight) %>% tail()
# 20 missing height?
d_player %>% select(id_player, height) %>% tail()

calc_descs(d_player, age)
calc_descs(d_player, weight)
calc_descs(d_player, height)
calc_descs(d_player, jump_height_max)

#----counts
d_player %>% select(position)
d_player %>% count(position)

#------------------------------- jump load figures

d_match_jumps = d_daily %>% 
  select(match = Match, jumps_n) %>% 
  filter(jumps_n != 0) %>% 
  mutate(match_name = ifelse(match == 1, "Match", "Training"))

# sample size
sample_size = d_match_jumps %>% group_by(match) %>% summarize(num=n())

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

# Plot
plot_jumps_match_v_training = d_match_jumps %>%
  left_join(sample_size, by = "match") %>%
  mutate(myaxis = paste0(match_name, "\n", "n=", num)) %>%
  ggplot(aes(x=myaxis, y=jumps_n, fill=match_name)) +
  geom_violin(width=1.4, alpha = 0.8) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  theme_base(text_size) +
  ostrc_theme +
  ylab("n daily\njumps") +
  xlab("") +
  theme(legend.position = "none",
        axis.title.y=element_text(angle=0)) +
  scale_fill_manual(values = c(lmisc::nih_distinct[4], lmisc::nih_distinct[1]
                                 ))

png("plot_n_jumps.png", width = 8, height = 5, unit = "in", res = 600)
plot_jumps_match_v_training
dev.off()

cairo_pdf("plot_n_jumps.pdf", width = 8, height = 5)
plot_jumps_match_v_training
dev.off()
