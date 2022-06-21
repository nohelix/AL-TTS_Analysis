# NOTES -------------------------------------------------------------------
# Much of this is hardcoded based on the file structure where we store the
# Auerbach lab uses, the 'summary' workbook, and sheet layout. Altering these
# WILL BREAK this script.

# The individual files should be organized in to folders by day, with the raw
# data CSV's being in another /export subfolder. This is determined by the
# current Behavior_Analysis matlab script.


# Package loading ---------------------------------------------------------

# data loading/manipulation
library(readxl); library(tidyverse); library(magrittr); library(dplyr); library(tidyr);

# Analysis
library(psych); library(psycho); library(lme4); library(lmerTest);

# Data visualization
library(ggplot2); library(forcats);

# Exporting data
library(writexl);


# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Global Variables --------------------------------------------------------

# The folder where the 'summary' worksheet Noise_TTS_Gp1_Green-Orange.xlsx is:
MainFolder = 'C:/Users/Noelle/Box/Behavior Lab/Projects (Behavior)/TTS'

# Should be greater than the maximum lines in the longest sheet of 'summary'
range = "A3:X600"

ProjectFolder = 'C:/Users/Noelle/Box/Auerbach Lab (Personal)/TTS Analysis'

HL_not_done = c("Orange 4", "Orange 5", "Green 2", "Green 3")

min_blocks = 5

TH_cutoff = 1.5

# Working directory -------------------------------------------------------
setwd(MainFolder)


# Clean Environment & Import Data -----------------------------------------
# errors, post ABRs and 'maintenance' days are automatically removed
source("~/GitHub/AL-TTS_Analysis/importing.R")


# Dataset Picker ----------------------------------------------------------
# Options: Data_all, Data_no1st, Data_trimmedBlocks
# Data_all is every valid trial
# Data_no1st has only the 1st full block removed to account for warm up
# Data_trimmedBlocks also removes days that don't meet the minimum (5 blocks) &
#       blocks exceeding the max of the worst rat. This accounts for exhaustion.

Analysis_data = Data_no1st %>%
  filter(!(ID %in% HL_not_done))

# TODO: add ability to filter based on dB range



# Analysis ----------------------------------------------------------------
# Graph Hit, FA and Trial Count from summary data
# calculate hearing threshold (from all data) and remove any trials below hearing level.
# ISSUE: Plots don't show.

source("~/GitHub/AL-TTS_Analysis/analysis.R")

