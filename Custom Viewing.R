
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

# TH %>%
#   filter(`Dur (ms)` != "100") %>%
#   filter(Condition %in% c("Baseline")) %>%
#   filter(!(ID %in% HL_not_done)) %>%
#   filter(BG_Intensity %in% c("NA", "50")) %>%
#   # write.table("clipboard", sep="\t", row.names=FALSE) %>%
#   View
