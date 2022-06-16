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

# Psychoacuostic Graph -----------------------------------------------------


# Calculate standard error (SE) like standard deviation (SD)
se <- function(x, ...) {sqrt(var(x, ...)/length(x))}

n_fun <- function(x){
  # print(x)
  return(data.frame(y = mean(x), label = paste0("n = ",length(x))))
}


# Change in d' by BG ------------------------------------------------------
# Overview (Change by Background)

dprime %>%
  filter(Condition == "dprime_change") %>%
  left_join(Avg_TH, by = c("Type", "BG_Type", "BG_Intensity")) %>%
  ggplot(aes(x = dB, y = dprime, color = Type, group = Type)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1.2) +
  # Add vline for each average threshold
  geom_vline(aes(xintercept = TH, color = Type), linetype = "longdash", size = 1, show.legend = FALSE) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(1)) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(1), size = 3) +
  stat_summary(fun = mean, geom = "line") +
  # stat_summary(fun.data = n_fun, geom = "text", vjust = 5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-1, 91), breaks = c(10,30,50,70,90)) +
  labs(x = "Intensity (dB)",
       y = "Change in d' following HHL (+/- SE)",
       color = "Go Frequency") +
  facet_wrap(~ BG, scales = "fixed", nrow = 3, strip.position = "top") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = "grey80")
    # panel.grid.minor.x = element_line(color = "grey80"))
  )

ggsave("Psychoacoustic Change by BG.jpg",
       plot = last_plot(), # or an explicit ggplot object name
       path = ProjectFolder,
       width = 1200, height = 1000, units = "px", dpi = 125)



# d' for BBN --------------------------------------------------------------
# BBN Graph (overall)

TH_data %>%
  select(ID, Sex, Condition, Type, BG_Type, BG_Intensity, `Dur (ms)`, dprime, dB) %>%
  filter(!(ID %in% HL_not_done)) %>%
  filter(Condition != "Recovery 2") %>%
  # filter(Condition != "Post 2nd Exposure") %>%
  filter(Stim == "BBN") %>%
  # filter(Sex == "Male") %>%
  ggplot(aes(x = dB, y = dprime)) +
  stat_summary(aes(color = Condition, group = Condition),
               fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(1)) +
  stat_summary(aes(color = Condition, group = Condition),
               fun = mean,
               geom = "point", position = position_dodge(1), size = 3) +
  stat_summary(aes(color = Condition, group = Condition),
               fun = mean, geom = "line") +
  facet_wrap(~ Type, scale = "free") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = "grey80")
    # panel.grid.minor.x = element_line(color = "grey80"))
  )

ggsave("Psychoacoustic_BBN.jpg",
       plot = last_plot(), # or an explicit ggplot object name
       path = ProjectFolder,
       width = 1200, height = 600, units = "px", dpi = 100)

# d' for BBN by Sex -------------------------------------------------------
# BBN Graph (by sex)
#
# TH_data %>%
#   select(ID, Sex, Condition, Type, BG_Type, BG_Intensity, `Dur (ms)`, dprime, dB) %>%
#   filter(!(ID %in% HL_not_done)) %>%
#   # filter(Condition != "Recovery") %>%
#   filter(Condition != "Recovery 2") %>%
#   # filter(Condition != "Post 2nd Exposure") %>%
#   filter(Stim == "BBN") %>%
#   # filter(Sex == "Male") %>%
#   ggplot(aes(x = dB, y = dprime)) +
#   stat_summary(aes(color = Condition, linetype = Sex, group = interaction(Condition, Sex)),
#                fun = mean,
#                fun.min = function(x) mean(x) - se(x),
#                fun.max = function(x) mean(x) + se(x),
#                geom = "errorbar", width = 1, position = position_dodge(1)) +
#   stat_summary(aes(color = Condition, shape = Sex, group = interaction(Condition, Sex)),
#                fun = mean,
#                geom = "point", position = position_dodge(1), size = 3) +
#   stat_summary(aes(color = Condition, linetype = Sex, group = interaction(Condition, Sex)),
#                fun = mean, geom = "line") +
#   facet_wrap(~ Type, scale = "free",) +
#   theme_classic() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     panel.grid.major.x = element_line(color = "grey80")
#     # panel.grid.minor.x = element_line(color = "grey80"))
#   )
#
#
# ggsave("Psychoacoustic_BBN_by_Sex.jpg",
#        plot = last_plot(), # or an explicit ggplot object name
#        path = ProjectFolder,
#        width = 1200, height = 600, units = "px", dpi = 100)



# d' for Tones by BG ----------------------------------------------------------
# 4-32 Graph (Overall by BG)
TH_data %>%
  select(ID, Sex, Condition, Type, BG_Type, BG_Intensity, `Dur (ms)`, dprime, dB) %>%
  filter(!(ID %in% HL_not_done)) %>%
  # filter(Condition != "Recovery") %>%
  filter(Condition != "Recovery 2") %>%
  # filter(Condition != "Post 2nd Exposure") %>%
  filter(Stim == "tone") %>%
  # filter(Sex == "Male") %>%
  ggplot(aes(x = dB, y = dprime)) +
  stat_summary(aes(color = Condition, group = Condition),
               fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(1)) +
  stat_summary(aes(color = Condition, group = Condition),
               fun = mean,
               geom = "point", position = position_dodge(1), size = 3) +
  stat_summary(aes(color = Condition, group = Condition),
               fun = mean, geom = "line") +
  facet_wrap(~ BG_Intensity, scale = "free", nrow = 3, strip.position = "top") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = "grey80")
    # panel.grid.minor.x = element_line(color = "grey80"))
  )

ggsave("Psychoacoustic_tone.jpg",
       plot = last_plot(), # or an explicit ggplot object name
       path = ProjectFolder,
       width = 1200, height = 600, units = "px", dpi = 100)


# d' for Tones @ 50dB PNK by kHz ------------------------------------------
# 4-32 Graph (By Frequency & BG)

TH_data %>%
  select(ID, Sex, Condition, Type, BG_Type, BG_Intensity, `Dur (ms)`, dprime, dB) %>%
  filter(!(ID %in% HL_not_done)) %>%
  # filter(Condition != "Recovery") %>%
  filter(Condition != "Recovery 2") %>%
  filter(Condition != "Post 2nd Exposure") %>%
  filter(Stim == "tone") %>%
  filter(BG_Intensity == "50") %>%
  ggplot(aes(x = dB, y = dprime)) +
  geom_hline(yintercept = 1.5, linetype = "dashed", size = 1.2) +
  stat_summary(aes(color = Condition, group = Condition),
               fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(1)) +
  stat_summary(aes(color = Condition, group = Condition),
               fun = mean,
               geom = "point", position = position_dodge(1), size = 3) +
  stat_summary(aes(color = Condition, group = Condition),
               fun = mean, geom = "line") +
  facet_wrap(~ BG_Intensity + fct_relevel(Type, "4kHz", "8kHz", "16kHz", "32kHz"), scale = "free") +
  xlim(20, 61) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = "grey80")
    # panel.grid.minor.x = element_line(color = "grey80"))
  )

ggsave("Psychoacoustic_tone_by_kHz.jpg",
       plot = last_plot(), # or an explicit ggplot object name
       path = ProjectFolder,
       width = 2400, height = 1200, units = "px", dpi = 100)


# d' for Tones by kHz and Sex ---------------------------------------------
# 4-32 Graph (By Frequency and Sex)
#
# TH_data %>%
#   select(ID, Sex, Condition, Type, BG_Type, BG_Intensity, `Dur (ms)`, dprime, dB) %>%
#   filter(!(ID %in% HL_not_done)) %>%
#   # filter(Condition != "Recovery") %>%
#   filter(Condition != "Recovery 2") %>%
#   filter(Condition != "Post 2nd Exposure") %>%
#   filter(Stim == "tone") %>%
#   # filter(Sex == "Male") %>%
#   ggplot(aes(x = dB, y = dprime)) +
#   stat_summary(aes(color = Condition, linetype = Sex, group = interaction(Condition, Sex)),
#                fun = mean,
#                fun.min = function(x) mean(x) - se(x),
#                fun.max = function(x) mean(x) + se(x),
#                geom = "errorbar", width = 1, position = position_dodge(1)) +
#   stat_summary(aes(color = Condition, shape = Sex, group = interaction(Condition, Sex)),
#                fun = mean,
#                geom = "point", position = position_dodge(1), size = 3) +
#   stat_summary(aes(color = Condition, linetype = Sex, group = interaction(Condition, Sex)),
#                fun = mean, geom = "line") +
#   facet_wrap(~ fct_relevel(Type, "4kHz", "8kHz", "16kHz", "32kHz"), scale = "free") +
#   theme_classic() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     panel.grid.major.x = element_line(color = "grey80")
#     # panel.grid.minor.x = element_line(color = "grey80"))
#   )
#
# ggsave("Psychoacoustic_tone_by_Sex.jpg",
#        plot = last_plot(), # or an explicit ggplot object name
#        path = ProjectFolder,
#        width = 2400, height = 1200, units = "px", dpi = 100)
