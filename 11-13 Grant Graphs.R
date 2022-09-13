# Package loading ---------------------------------------------------------

# data loading/manipulation
library(readxl); library(tidyverse); library(magrittr); library(dplyr); library(tidyr);

# Data visualization
library(ggplot2); library(forcats); library(gtools); library(ggpmisc)

# Working directory -------------------------------------------------------
ProjectFolder = "C:/Users/Noelle/Box/ABR recordings/ABR Results/Noelle/11-13kHz HHL/"


# Select Graphing Data ----------------------------------------------------
# Can be summarized or not

To_Graph = read_excel("C:/Users/Noelle/Box/ABR recordings/ABR Results/Noelle/11-13kHz HHL/Summary.xlsx")

To_Graph = To_Graph %>%
            mutate(Condition = fct_relevel(Condition, c("Baseline", "Hearing Loss", "1 day", "1 week")),
                   Freq = fct_recode(as.factor(Freq), BBN = "0"),
                   Freq = fct_relevel(Freq, c("4", "8", "BBN", "16", "32")))

To_Graph %>%
  group_by(Condition, Ear, Freq) %>%
  summarise(n = length(unique(ID)), ID = paste(unique(ID), collapse = ", ")) %>%
  mutate(Freq = fct_relevel(Freq, c("4", "8", "16", "32", "BBN"))) %>%
  print


# RMS Grant Graph ---------------------------------------------------------------
# Signal-to-Noise ratio for BBN, significant for 70-90dB

To_Graph  %>%
  ggplot(aes(x = Inten, y = RMS, color = Condition, group = Condition)) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 3) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line") +
  scale_x_continuous(limits = c(10, 100), breaks = c(20, 40, 60, 80, 100)) +
  labs(x = "Sound Intensity (dB)",
       y = "Signal-to-Noise Ratio (RMS)") +
  facet_wrap( ~ Freq, nrow = 2) +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    panel.grid.major.x = element_line(color = "white"),
    legend.position = c(0.8, 0.2)
  )

ggsave("11-13kHz_HHL_RMS.jpg",
       plot = last_plot(), # or an explicit ggplot object name
       path = ProjectFolder)

# RMS ANOVA -------------------------------------------------------------------
RMS.aov <- aov(RMS ~ Condition * Inten * Freq,
               data = To_Graph %>%
                        group_by(ID, Freq, Inten, Condition) %>%
                        summarise(RMS = mean(RMS))
               )

shapiro.test(RMS.aov$residuals)$p.value

summary(RMS.aov)
# TukeyHSD(RMS.aov)
# TukeyHSD(RMS.aov)$`Condition:dB` %>%
#   as_tibble(.name_repair = "unique", rownames = "Comparison") %>%
#   filter(grepl("WT:.*?-Het:.*?|Het:.*?-WT:.*?", Comparison)) %>%
#   mutate(dB1 = gsub("^.*?WT:(\\d+).*?$","\\1", Comparison),
#          dB2 = gsub("^.*?Het:(\\d+).*?$","\\1", Comparison)) %>%
#   filter(dB1 == dB2) %>%
#   mutate(sig = stars.pval(`p adj`))


# W1 Grant Graph ---------------------------------------------------------------
# Wave 1 Amplitude for BBN to match Fmr1 KO result.

To_Graph  %>%
  # filter(Type == "BBN") %>%
  ggplot(aes(x = Inten, y = `W1 amp (uV)`, color = Condition, group = Condition)) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 3, position = position_dodge(1)) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(1), size = 3) +
  stat_summary(fun = mean, geom = "line") +
  scale_x_continuous(limits = c(10, 100), breaks = c(20, 40, 60, 80, 100)) +
  labs(x = "Sound Intensity (dB)",
       y = expression("Wave 1 Amplitued (\u00b5V)")) +
  facet_wrap( ~ Freq, nrow = 2) +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    panel.grid.major.x = element_line(color = "white"),
    legend.position = c(0.9, 0.2)
  )

ggsave("11-13kHz_HHL_W1lat.jpg",
       plot = last_plot(), # or an explicit ggplot object name
       path = ProjectFolder)

# W1 Amp ANOVA -------------------------------------------------------------------
W1amp.aov <- aov(W1 ~ Condition * Freq * Inten,
                 data = To_Graph %>%
                   group_by(ID, Freq, Inten, Condition) %>%
                   summarise(W1 = mean(`W1 amp (uV)`))
)

is_parametric = shapiro.test(W1amp.aov$residuals)$p.value > 0.05

if (is_parametric == TRUE) {writeLines("Normal data proced with ANOVA")} else
{writeLines(paste("Non-parametric data so use Kruskal followed by Dunn testing. \nShapiro Test: p =", shapiro.test(W1amp.aov$residuals)$p.value %>% round(digits = 3)))}

# summary(W1amp.aov)

# 80dB Graph ---------------------------------------------------------------

To_Graph_Table <-
To_Graph %>%
  mutate(Freq = fct_relevel(Freq, c("4", "8", "16", "32", "BBN"))) %>%
  filter(Inten == "80") %>%
  filter(Condition %in% c("Baseline", "1 week")) %>%
  group_by(Freq) %>%
  summarise(p = wilcox.test(`W1 amp (uV)` ~ Condition,
                            exact = FALSE,
                            alternative = "greater")$p.value %>% round(digits = 3)) %>%
  mutate(BH = p.adjust(p, method = "BH") %>% round(digits = 3),
         Sig = stars.pval(`BH`))

# wilcox.test(`W1 amp (uV)` ~ Condition,
#             exact = FALSE, correct = FALSE,
#             subset = Condition %in% c("Baseline", "1 week"),
#             data = filter(To_Graph, Inten == "80")
# )


To_Graph  %>%
  filter(Inten == "80") %>%
  mutate(Freq = fct_relevel(Freq, c("4", "8", "16", "32", "BBN"))) %>%
  ggplot(aes(x = Condition, y = `W1 amp (uV)`, color = Freq, group = Freq)) +
    stat_summary(fun = mean,
                 fun.min = function(x) mean(x) - se(x),
                 fun.max = function(x) mean(x) + se(x),
                 geom = "errorbar", width = 0.1, position = position_dodge(0.03)) +
    stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge(0.03)) +
    stat_summary(fun = mean, geom = "line", position = position_dodge(0.03)) +
    labs(x = "",
         y = expression("Wave 1 Amplitued (\u00b5V)"),
         color = "Frequency",
         title = "Wave 1 Amplitude at 80dB") +
    annotate(geom = 'table',
             x = 4.5, y = 0.1,
             label = list(To_Graph_Table)) +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      panel.grid.major.x = element_line(color = "white"),
      legend.position = c(0.9, 0.8)
    )

ggsave("11-13kHz_HHL_80dB.jpg",
       plot = last_plot(), # or an explicit ggplot object name
       path = ProjectFolder)
