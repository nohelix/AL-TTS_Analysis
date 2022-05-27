
# Notes -------------------------------------------------------------------

# Package loading ---------------------------------------------------------

# data loading/manipulation
library(readxl); library(tidyverse); library(magrittr); library(dplyr); library(tidyr);

# Analysis
library(psych); library(psycho); library(lme4); library(lmerTest);

# Data visualization
library(ggplot2); library(forcats);

# Exporting data
library(writexl)

# Phase Day numbers -------------------------------------------------------

TTS_Data %>%
  filter(Condition  == "Post HHL") %>%
  filter(!(ID %in% HL_not_done)) %>%
  filter(ID == "Orange 3") %>%
  group_by(ID, Condition, Duration, Phase, Frequency, BG_Type, BG_Intensity) %>%
  summarise(Days = n(), Trials = sum(Trials, na.rm = TRUE))

TTS_Data %>%
  # filter(Condition %in% c("Baseline", "Post HHL")) %>%
  filter(Condition  == "Post HHL") %>%
  filter(!(ID %in% HL_not_done)) %>%
  filter(ID == "Orange 3") %>%
  group_by(ID, Condition, Duration, Frequency, Intensity, BG_Type, BG_Intensity) %>%
  summarise(Days = n(), Trials = sum(Trials, na.rm = TRUE))


# Threshold filtering -----------------------------------------------------

TH %>%
  filter(`Dur (ms)` != "100") %>% 
  filter(Condition %in% c("Baseline")) %>%
  filter(!(ID %in% HL_not_done)) %>% 
  filter(BG_Intensity %in% c("NA", "50")) %>% 
  # write.table("clipboard", sep="\t", row.names=FALSE) %>%
  View

# Hit/FA Rate -------------------------------------------------------------

TTS_Data %>%
  filter(!(ID %in% HL_not_done)) %>% 
  # Group by ID, experiment type, etc
  group_by(ID, Sex, Condition, Frequency, BG_Type, BG_Intensity) %>%
  # Summarize for each individual
  summarise(count = n_distinct(Date),
            Trials = mean(Trials, na.rm = T),
            Hit = mean(Hit, na.rm = T),
            FA = mean(FA, na.rm = T)) %>%
  ungroup() %>%    
  # filter(Condition != "Recovery") %>%
  filter(Condition != "Recovery 2") %>%
  filter(Condition != "Post 2nd Exposure") %>% #View
  # filter(Frequency == "BBN") %>%
  # filter(Frequency == "4-32kHz") %>%
  gather(variable, value, Trials, Hit, FA) %>% #print
  ggplot(aes(x = fct_relevel(BG_Intensity, "NA","30","50"), y = value)) +
    geom_boxplot(aes(fill = Condition)) +
    geom_point(data = . %>% filter(ID == "Orange 3"), 
               aes(group = Condition), shape = 10,
               position = position_dodge(width = 0.75), size = 3) +
    labs(title = "BBN & 4-32kHz",
         x = "Background Intensity (dB)") +
    facet_wrap(.~ fct_relevel(variable, "Trials", "Hit", "FA"), scales = "free_y") +
    theme_classic() +
    theme(
      axis.title.x = element_text(size = 14, color = "black"),   # size of x-axis title
      axis.title.y = element_text(size = 14, color = "black"),   # size of y-axis title
      axis.text.x = element_text(size = 12, color = "black"),    # size of x-axis text
      axis.text.y = element_text(size = 12, color = "black"),    # size of y-axis text
      strip.text.x = element_text(size = 14, color = "black"), # size of facet titles
      legend.position = "right"                                 # hide legend
    )

# Psychoacustic Graph -----------------------------------------------------


# Calculate standard error (SE) like standard deviation (SD)
se <- function(x, ...) {sqrt(var(x, ...)/length(x))}

n_fun <- function(x){
  # print(x)
  return(data.frame(y = mean(x), label = paste0("n = ",length(x))))
} 


#General (Change by Background)
dprime %>%
  filter(Condition == "dprime_change") %>%
  left_join(Avg_TH, by = c("Type", "BG_Type", "BG_Intensity")) %>%
  # filter(Sex == "Male") %>%
  ggplot(aes(x = dB, y = dprime, color = Type, group = Type)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1.2) +
  # Add vline for each average threshold
  geom_vline(aes(xintercept = TH, color = Type), linetype = "longdash", size = 1, show.legend = FALSE) +
  # geom_text(aes(label=..count..), stat = "count", hjust=0, vjust=0) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(1)) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(1), size = 3) +
  stat_summary(fun = mean, geom = "line") +
  geom_line(data = . %>% filter(ID == "Orange 3"), linetype = "dotdash", size = 1) +
  geom_point(data = . %>% filter(ID == "Orange 3"), shape = 13, color = "black", size = 3) +
  # stat_summary(fun.data = n_fun, geom = "text", vjust = 5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-1, 91), breaks = c(10,30,50,70,90)) +
  labs(x = "Intensity (dB)",
       y = "Change in d' following HHL (+/- SE)",
       color = "Go Frequency") +
  facet_wrap(~ BG, scale = "fixed", nrow = 3, strip.position = "top") +
  theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(colour = "grey80")
    # panel.grid.minor.x = element_line(colour = "grey80"))
  )
