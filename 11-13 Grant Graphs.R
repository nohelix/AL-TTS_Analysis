# Package loading ---------------------------------------------------------

# data loading/manipulation
library(readxl); library(tidyverse); library(magrittr); library(dplyr); library(tidyr);

# Data visualization
library(ggplot2); library(forcats);

# Select Graphing Data ----------------------------------------------------
# Can be summarized or not

To_Graph = read_excel("C:/Users/Noelle/Box/ABR recordings/ABR Results/Noelle/11-13kHz HHL/Summary.xlsx")

To_Graph = To_Graph %>%
            mutate(Condition = fct_relevel(Condition, c("Baseline", "Hearing Loss", "1 day", "1 week")),
                   Freq = fct_recode(as.factor(Freq), BBN = "0"),
                   Freq = fct_relevel(Freq, c("4", "8", "BBN", "16", "32")))


# RMS Grant Graph ---------------------------------------------------------------
# Signal-to-Noise ratio for BBN, significant for 70-90dB

To_Graph  %>%
  ggplot(aes(x = Inten, y = RMS, color = Condition, group = Condition)) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 0.5) +
  stat_summary(fun = mean, geom = "point", size = 2) +
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

ggsave("R01_RMS.jpg",
       plot = last_plot(), # or an explicit ggplot object name
       path = ProjectFolder,
       width = 250, height = 250, units = "px", dpi = 100)

# RMS ANOVA -------------------------------------------------------------------
RMS.aov <- aov(RMS ~ Condition * Inten * Freq, data = To_Graph)
summary(RMS.aov)
TukeyHSD(RMS.aov)
TukeyHSD(RMS.aov)$`Condition:dB` %>%
  as_tibble(.name_repair = "unique", rownames = "Comparison") %>%
  filter(grepl("WT:.*?-Het:.*?|Het:.*?-WT:.*?", Comparison)) %>%
  mutate(dB1 = gsub("^.*?WT:(\\d+).*?$","\\1", Comparison),
         dB2 = gsub("^.*?Het:(\\d+).*?$","\\1", Comparison)) %>%
  filter(dB1 == dB2) %>%
  mutate(sig = stars.pval(`p adj`))


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
       path = ProjectFolder,
       width = 250, height = 250, units = "px", dpi = 100)

# W1 Amp ANOVA -------------------------------------------------------------------
W1amp.aov <- aov(`W1 amp (uV)` ~ Condition * Freq * Inten, data = To_Graph)

is_parametric = shapiro.test(Rxn.aov$residuals)$p.value > 0.05

if (is_parametric == TRUE) {writeLines("Normal data proced with ANOVA")} else
{writeLines(paste("Non-parametric data so use Kruskal followed by Dunn testing. \nShapiro Test: p =", shapiro.test(Rxn.aov$residuals)$p.value %>% round(digits = 3)))}

# summary(W1amp.aov)
kruskal.test(`W1 amp (uV)` ~ interaction(Condition, Freq, Inten), data = To_Graph)

pairwise.wilcox.test(To_Graph$Condition, To_Graph$Inten,
                     p.adjust.method = "BH")

W1amp.postHoc <-
  FSA::dunnTest(`W1 amp (uV)` ~ interaction(Condition, Freq, Inten),
                data = To_Graph,
                method = "bonf")

W1amp.postHoc$res %>%
  as_tibble() %>%
  select(-P.unadj) %>%
  mutate(Sig = gtools::stars.pval(P.adj),
         Alone = str_split_fixed(.$Comparison, ' - ', 2)[,1],
         Mixed = str_split_fixed(.$Comparison, ' - ', 2)[,2]) %>%
  filter(Alone %in% c("Alone.300", "Alone.100", "Alone.50") & Mixed %in% c("Mix.300", "Mix.100", "Mix.50", "Rotating.300", "Rotating.100", "Rotating.50")) %>%
  mutate(Alone = str_extract(Alone, "[:digit:]+") %>% as.numeric(),
         Mixed = str_extract(Mixed, "[:digit:]+") %>% as.numeric(),
         vs = str_extract(Comparison, "- [:alpha:]+") %>% str_remove("- "),
         P.adj = round(P.adj, digits = 3)) %>%
  filter(Alone == Mixed) %>%
  select(Alone, vs, Z, P.adj, Sig) %>%
  arrange(Alone, vs)



# W1 Lat ANOVA ------------------------------------------------------------------
W1lat.aov <- aov(`W1 Lat` ~ Genotype * dB, data = To_Graph)
summary(W1lat.aov)
TukeyHSD(W1lat.aov)$`Genotype:dB` %>%
  as_tibble(.name_repair = "unique", rownames = "Comparison") %>%
  filter(grepl("WT:.*?-Het:.*?|Het:.*?-WT:.*?", Comparison)) %>%
  mutate(dB1 = gsub("^.*?WT:(\\d+).*?$","\\1", Comparison),
         dB2 = gsub("^.*?Het:(\\d+).*?$","\\1", Comparison)) %>%
  filter(dB1 == dB2) %>%
  mutate(sig = stars.pval(`p adj`))
