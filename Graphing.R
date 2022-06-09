# Hit/FA Graph -------------------------------------------------------------
# creates, displays, and saves a graph of hit rate, false alarm rate, and trial count across conditions
# Currently done off the Summary spreadsheet and NOT calculated from the actual data.
# Only trial count is sanity checked against the real data.

Hit_summary_by_day %>%
  filter(!(ID %in% HL_not_done)) %>%
  filter(Condition != "Recovery") %>%
  filter(Condition != "Recovery 2") %>%
  gather(variable, value, Hit, FA) %>% #print
  ggplot(aes(x = Stim, y = value)) +
  geom_boxplot(aes(fill = Condition)) +
  labs(x = "Go Stim Type",
       y = "% Rate") +
  facet_wrap(.~ interaction(fct_relevel(variable, "Hit", "FA"), fct_relevel(BG_Intensity, "NA","30","50")),
             scales = "free_y", ncol = 2) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 14, color = "black"),   # size of x-axis title
    axis.title.y = element_text(size = 14, color = "black"),   # size of y-axis title
    axis.text.x = element_text(size = 12, color = "black"),    # size of x-axis text
    axis.text.y = element_text(size = 12, color = "black"),    # size of y-axis text
    strip.text.x = element_text(size = 14, color = "black"), # size of facet titles
    legend.position = "right"                                 # hide legend
  )

ggsave("Descriptive_trials_FA_hit.jpg",
       plot = last_plot(), # or an explicit ggplot object name
       path = ProjectFolder,
       width = 600, height = 1200, units = "px", dpi = 100)

# Plotting vars -----------------------------------------------------------

Noise = "NA" #("NA", "PNK", "WN")
BG_dB = "NA"  #("NA", "30", "50)

BG = case_when(Noise == "PNK" ~ "Pink Background Noise",
               Noise == "WN" ~ "White Background Noise",
               Noise == "NA" ~ "No Background Noise")

# Should potentially be updated to something like this
# BG=case_when(BG_Type == "NA" & BG_Intensity == "NA" ~ "Quiet",
#              BG_Type == "PNK" & BG_Intensity == "30" ~ "30dB Pink Noise Background",
#              BG_Type == "PNK" & BG_Intensity == "50" ~ "50dB Pink Noise Background",
#              BG_Type == "WN" & BG_Intensity == "50" ~ "50dB White Noise Background",
#              TRUE ~ "ISSUE")

# Rxn Plot ----------------------------------------------------------------
# Plot by animal pre & post & group

Rxn_overall %>%
  filter(`Inten (dB)` != -100) %>%
  filter(Condition != "Recovery") %>%
  filter(Condition != "Recovery 2") %>%
  filter(Condition != "Post 2nd Exposure") %>%
  filter(!(ID %in% HL_not_done)) %>%
  # filter(`Freq (kHz)` != "0") %>%
  filter(Noise == BG_Type & BG_Intensity == BG_dB) %>%
  # filter(Sex == "Male") %>%
  ggplot(aes(x = `Inten (dB)`, y = mean)) +
  # geom_line(aes(color = Condition, linetype = ID))+
  # geom_point(aes(color = Condition, fill = ID))+
  # geom_point(aes(group = ID), color = "grey70")+
  # geom_line(aes(group = ID, color = Condition))+
  stat_summary(aes(color = Condition, group = Condition),
               fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(1)) +
  stat_summary(aes(color = Condition, group = Condition),
               fun = mean,
               geom = "point", position = position_dodge(1), size = 3) +
  stat_summary(aes(color = Condition, group = Condition), fun = mean, geom = "line") +
  labs(title = paste(BG, "at", BG_dB, "dB"),
       x = "Intensity (dB)",
       y = "Reaction time (ms, mean +/- SE)") +
  facet_wrap(~ `Freq (kHz)`, scales = "free") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = "grey80")
    # panel.grid.minor.x = element_line(color = "grey80"))
  )

ggsave(paste0("Condition_", Noise, BG_dB, ".jpg"),
       plot = last_plot(), # or an explicit ggplot object name
       path = ProjectFolder,
       width = 1200, height = 600, units = "px", dpi = 100)


# lines = BG & facets = Condition
Rxn_overall %>%
  # group_by(ID) %>%
  filter(`Inten (dB)` != -100) %>%
  # filter(Condition != "Recovery") %>%
  filter(Condition != "Recovery 2") %>%
  filter(Condition != "Post 2nd Exposure") %>%
  filter(!(ID %in% HL_not_done)) %>%
  # filter(`Freq (kHz)` == "0") %>%
  # filter(`Freq (kHz)` == "4") %>%
  # filter(`Freq (kHz)` == "8") %>%
  # filter(`Freq (kHz)` == "16") %>%
  filter(`Freq (kHz)` == "32") %>%
  ggplot(aes(x = `Inten (dB)`, y = mean)) +
  # geom_line(aes(color = BG_Intensity, linetype = ID))+
  # geom_point(aes(color = BG_Intensity, fill = ID))+
  # geom_point(aes(group = ID), color = "grey70")+
  # geom_line(aes(group = ID, color = BG_Intensity))+
  stat_summary(aes(color = BG_Intensity, group = BG_Intensity),
               fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(1)) +
  stat_summary(aes(color = BG_Intensity, group = BG_Intensity),
               fun = mean,
               geom = "point", position = position_dodge(1), size = 3) +
  stat_summary(aes(color = BG_Intensity, group = BG_Intensity), fun = mean, geom = "line", ) +
  labs(title = "32kHz",
       x = "Intensity (dB)",
       y = "Reaction time (ms, mean +/- SE)") +
  facet_wrap(~ `Condition`, scale = "free") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = "grey80")
    # panel.grid.minor.x = element_line(color = "grey80"))
  )

# Individuals
Rxn_overall %>%
  filter(`Inten (dB)` != -100) %>%
  filter(Condition != "Recovery") %>%
  filter(Condition != "Recovery 2") %>%
  filter(Condition != "Post 2nd Exposure") %>%
  filter(!(ID %in% HL_not_done)) %>%
  filter(`Freq (kHz)` != "0") %>%
  # filter(ID == "Green 1") %>%
  filter(Noise == BG_Type & BG_Intensity == BG_dB) %>%
  ggplot(aes(x = `Inten (dB)`, y = mean)) +
  geom_line(aes(color = ID, linetype = Condition)) +
  geom_point(aes(color = ID, shape = Condition)) +
  labs(title = paste(BG, "at", BG_dB, "dB"),
       x = "Intensity (dB)",
       y = "Reaction time (ms)") +
  facet_wrap(~ `Freq (kHz)`, scale = "free") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = "grey80")
    # panel.grid.minor.x = element_line(color = "grey80"))
  )


# ggsave(paste0("Individuals_", Noise, BG_dB, ".jpg"),
#        plot = last_plot(), # or an explicit ggplot object name
#        path = ProjectFolder,
#        width = 1200, height = 600, units = "px", dpi = 100)

# Rxn Change plot ---------------------------------------------------------

Rxn_change %>%
  filter(`Inten (dB)` != -100) %>%
  filter(Condition != "Recovery 2") %>%
  filter(Condition != "Post 2nd Exposure") %>%
  filter(!(ID %in% HL_not_done)) %>%
  filter(`Freq (kHz)` != "0") %>%
  # filter(Noise == BG_Type & BG_Intensity == BG_dB) %>%
  ggplot(aes(x = `Inten (dB)`, y = mean)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1.2) +
  stat_summary(aes(color = Sex, linetype = BG_Intensity, group = interaction(Sex, BG_Intensity)),
               fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(1)) +
  stat_summary(aes(color = Sex, shape = BG_Intensity, group = interaction(Sex, BG_Intensity)),
               fun = mean,
               geom = "point", position = position_dodge(1), size = 3) +
  stat_summary(aes(color = Sex, linetype = BG_Intensity, group = interaction(Sex, BG_Intensity)),
               fun = mean, geom = "line") +
  labs(title = "Change in Reaction time",
       x = "Intensity (dB)",
       y = "Reaction time (ms, mean +/- SE)") +
  facet_wrap(~ `Freq (kHz)`, scale = "free") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = "grey80")
    # panel.grid.minor.x = element_line(color = "grey80"))
  )

ggsave("Rxn_Change.jpg",
       plot = last_plot(), # or an explicit ggplot object name
       path = ProjectFolder,
       width = 1200, height = 600, units = "px", dpi = 100)
