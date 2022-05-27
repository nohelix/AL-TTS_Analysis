
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
  filter(Condition %in% c("Baseline", "Post HHL")) %>%
  filter(!(ID %in% HL_not_done)) %>%
  group_by(ID, Condition, Duration, Phase, Frequency, BG_Type, BG_Intensity) %>%
  summarise(Days = n(), Trials = sum(Trials, na.rm = TRUE))

TTS_Data %>%
  filter(Condition %in% c("Baseline", "Post HHL")) %>%
  filter(!(ID %in% HL_not_done)) %>%
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


