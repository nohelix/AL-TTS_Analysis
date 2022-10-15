
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
  # filter(Condition %in% c("Baseline", "Post HHL", "Post 2nd Exposure")) %>%
  filter(Condition  == "Post HHL") %>%
  filter(Duration == "50ms") %>%
  filter(ID == "Orange 4") %>%
  group_by(ID, Condition, Duration, Frequency, Intensity, BG_Type, BG_Intensity) %>%
  summarise(Days = n(), Trials = sum(Trials, na.rm = TRUE)) %>%
  arrange(Days)

# Mixed vs. Alone BBN duration rxn ----------------------------------------

Data_no1st %>%
  filter(Type == 1 & Response == "Hit") %>%
  filter(`Inten (dB)` != -100) %>%
  filter(Stim == "BBN" & Condition == "Baseline") %>%
  # Filter by TH table so that only reaction times above thresholds are included
  left_join(TTS_Data %>%
              filter(Frequency == "BBN" & Condition == "Baseline") %>%
              select(Date, ID, Intensity, Duration) %>%
              rename(Range = Intensity),
            by = c("Date", "ID")) %>%
  group_by(ID, Sex, Condition, Stim, Duration, `Dur (ms)`, `Inten (dB)`) %>%
  filter(ID %in% c("Green 11", "Green 12", "Orange 11", "Orange 12")) %>%
  summarise(Rxn = mean(`R Time (ms)`)) %>%
  mutate(Duration = fct_recode(Duration, Alone = "50ms", Alone = "100ms", Alone = "300ms", Mixed = "50-300ms"),
         `Dur (ms)` = as.factor(`Dur (ms)`),
         `Dur (ms)` = fct_relevel(`Dur (ms)`, c("50", "100", "300"))) %>%
  filter(`Inten (dB)` >= 30 & `Inten (dB)` <= 80 &!(`Inten (dB)` %in% c(25, 35, 45))) %>%
  ggplot(aes(x = `Inten (dB)`, y = Rxn, color = `Dur (ms)`, group = `Dur (ms)`)) +
      stat_summary(fun = mean,
                   fun.min = function(x) mean(x) - se(x),
                   fun.max = function(x) mean(x) + se(x),
                   geom = "errorbar", width = 3) +
      stat_summary(fun = mean, geom = "point", size = 3) +
      stat_summary(fun = mean, geom = "line") +
      # scale_x_continuous(limits = c(28, 92), breaks = c(30, 50, 70, 90)) +
      labs(x = "Sound Intensity (dB)",
           y = "Reaction time (ms)") +
      facet_wrap(~ Duration, nrow = 2) +
      theme_classic() +
      theme(
        text = element_text(size = 12),
        panel.grid.major.x = element_line(color = "white")
      )

Test.df = Data_no1st %>%
  filter(Type == 1 & Response == "Hit") %>%
  filter(`Inten (dB)` != -100) %>%
  filter(Stim == "BBN" & Condition == "Baseline") %>%
  # Filter by TH table so that only reaction times above thresholds are included
  left_join(TTS_Data %>%
              filter(Frequency == "BBN" & Condition == "Baseline") %>%
              select(Date, ID, Intensity, Duration) %>%
              rename(Range = Intensity),
            by = c("Date", "ID")) %>%
  group_by(ID, Sex, Condition, Stim, Duration, `Dur (ms)`, `Inten (dB)`) %>%
  summarise(Rxn = mean(`R Time (ms)`)) %>%
  mutate(Duration = fct_recode(Duration, Alone = "50ms", Alone = "100ms", Alone = "300ms", Mixed = "50-300ms"),
         `Dur (ms)` = as.factor(`Dur (ms)`),
         `Dur (ms)` = fct_relevel(`Dur (ms)`, c("50", "100", "300"))) %>%
  filter(`Inten (dB)` >= 30 & `Inten (dB)` <= 80 &!(`Inten (dB)` %in% c(25, 35, 45))) %>%
  filter(ID %in% c("Orange 11", "Orange 12", "Green 11", "Green 12") & Duration == "Alone") %>%
  rename(Intensity = `Inten (dB)`,
         Dur = `Dur (ms)`)

Test.aov = aov(LambertW::Gaussianize(Test.df$Rxn) ~ Intensity * Dur, data = Test.df)
shapiro.test(Test.aov$residuals)

summary(Test.aov)

TukeyHSD(Test.aov, "Dur") %>% broom::tidy() %>% mutate(sig = gtools::stars.pval(adj.p.value))

# Missing raw files -------------------------------------------------------
File_list_verify %>%
  filter(is.na(FileName_complete)) %>%
  # .mat files from 9-22-2021 and earlier are in the old format and can not be exported currently
  filter(Date > "2021-9-22") %>%
  filter(Date < Sys.Date()) %>%
  arrange(Date) %>%
  View

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
  filter(Condition != "Recovery") %>%
  filter(Condition != "Post 2nd Exposure") %>% #View
  # filter(Frequency == "BBN") %>%
  # filter(Frequency == "4-32kHz") %>%
  gather(variable, value, Trials, Hit, FA) %>% #print
  ggplot(aes(x = fct_relevel(BG_Intensity, "NA","30","50"), y = value)) +
    geom_boxplot(aes(fill = Condition)) +
    # geom_point(data = . %>% filter(ID == "Orange 3"),
    #            aes(group = Condition), shape = 10,
    #            position = position_dodge(width = 0.75), size = 3) +
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
