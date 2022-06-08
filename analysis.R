
# Hit/FA Rate -------------------------------------------------------------
# creates, displays, and saves a graph of hit rate, false alarm rate, and trial count across conditions
# Currently done off the Summary spreadsheet and NOT calculated from the actual data.
# Only trial count is sanity checked against the real data.

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


ggsave("Descriptive_trials_FA_hit.jpg",
       plot = last_plot(), # or an explicit ggplot object name
       path = ProjectFolder,
       width = 1200, height = 600, units = "px", dpi = 100)

# Threshold Calculation ---------------------------------------------------
# Signal detection index calculation by the psycho package. We use d' a sensitivity measure.
# https://neuropsychology.github.io/psycho.R/2018/03/29/SDT.html

# Creates a properly formated table for psycho by adding the overall CR/FA to each row
dprime_table <- function(df) {
  # print(df)
  check = df %>% filter(Type == 0) %>% count() %>% as.numeric() #%>% print
  CRnum = (if (check == 1) filter(df, Type == 0) %>% .$C.R. %>% as.numeric() else check) #%>% print
  FAnum = (if (check == 1) filter(df, Type == 0) %>% .$F.A. %>% as.numeric() else check) #%>% print
  new_df = df %>% filter(Type == 1) %>% rename(CR = C.R., FA = F.A.) %>%
    mutate(CR = ifelse(is.na(CR), CRnum, CR),
           FA = ifelse(is.na(FA), FAnum, CR),
           Hit = as.numeric(Hit),
           Miss = as.numeric(Miss)) %>% replace(is.na(.), 0) #%>% print
  return(new_df)
}

# Signal detection index calculation
dprime_calc <- function(df) {
  # print(df)
  dprime(n_hit = df$Hit,
         n_fa = df$FA,
         n_miss = df$Miss,
         n_cr = df$CR,
         adjusted = TRUE) %>%
    as_tibble() %>%
    mutate(dB = df$`Inten (dB)`,
           Type =  case_when(df$`Freq (kHz)` == 0 ~ "BBN",
                             TRUE ~ paste0(df$`Freq (kHz)`, "kHz"))# %>% print
    ) #%>% print
}

# Threshold calculation calculation
TH_calc <- function(df) {
  # library(drda)
  # drda(dprime ~ dB, data = df) %>% plot
  fit = loess(dprime ~ dB, data = df)
  # plot(fit)
  TH = approx(x = fit$fitted, y = fit$x, xout = TH_cutoff)$y #%>% print
  return(TH)
}

# TODO: Do a polynomial line fitting of d' and then find the intercept at the TH cutoff.
# Issue no guarentee of bracketing and indeed failure for Green 1 @ 32kHz
# Pulling the wrong line
# ID      Sex   Condition Stim  BG_Type BG_Intensity `Dur (ms)` dprime    dB Type  score
# <chr>   <chr> <fct>     <chr> <chr>   <chr>             <dbl>  <dbl> <dbl> <fct> <dbl>
# 2 Green 1 Male  Baseline  tone  NA      NA                   50  1.28      0 32kHz 0.219
# 3 Green 1 Male  Baseline  tone  NA      NA                   50  1.17      5 32kHz 0.327
# 4 Green 1 Male  Baseline  tone  NA      NA                   50  1.91     10 32kHz 0.406

TH_data <-
  Analysis_data %>%
  group_by(ID, Sex, Condition, Stim, BG_Type, BG_Intensity, `Dur (ms)`, Type, `Freq (kHz)`, `Inten (dB)`, Response) %>% #View
  summarise(count = n()) %>%
  spread(Response, count) %>% #View
  group_by(ID, Sex, Condition, Stim, BG_Type, BG_Intensity, `Dur (ms)`) %>% #print
  nest() %>%
  mutate(dprime_data = map(data, dprime_table),
         dprime = map(dprime_data, dprime_calc)) %>% #print
  unnest(dprime) #%>% print

TH_cutoff <- 1.5

TH <-
  TH_data %>%
  # filter(ID == "Green 1") %>%
  # filter(Stim == "tone") %>%
  # filter(`Dur (ms)` == 50) %>%
  # filter(Condition == "Post HHL") %>%
  # filter(Type == "16kHz") %>%
  select(ID:`Dur (ms)`, dprime, dB, Type) %>% #print
  mutate(Type = fct_relevel(Type, levels = c("BBN", "4kHz", "8kHz", "16kHz", "32kHz"))) %>% #print
  group_by(ID, Sex, Condition, BG_Type, BG_Intensity, `Dur (ms)`, Type) %>%
  nest() %>%
  mutate(TH = map_dbl(data, TH_calc)) %>%
  select(-data) %>%
  spread(Type, TH)

# Average Thresholds
Avg_TH_Condition <-
  TH %>%
  filter(Condition %in% c("Baseline", "Post HHL")) %>%
  filter(!(ID %in% HL_not_done)) %>%
  group_by(Condition, BG_Type, BG_Intensity) %>%
  summarise("BBN_avg" = mean(BBN, na.rm = TRUE),
            "4kHz_avg" = mean(`4kHz`, na.rm = TRUE),
            "8kHz_avg" = mean(`8kHz`, na.rm = TRUE),
            "16kHz_avg" = mean(`16kHz`, na.rm = TRUE),
            "32kHz_avg" = mean(`32kHz`, na.rm = TRUE))

Avg_TH <-
  TH %>%
  filter(Condition %in% c("Baseline", "Post HHL")) %>%
  filter(!(ID %in% HL_not_done)) %>%
  group_by(BG_Type, BG_Intensity) %>%
  summarise("BBN" = mean(BBN, na.rm = TRUE),
            "4kHz" = mean(`4kHz`, na.rm = TRUE),
            "8kHz" = mean(`8kHz`, na.rm = TRUE),
            "16kHz" = mean(`16kHz`, na.rm = TRUE),
            "32kHz" = mean(`32kHz`, na.rm = TRUE)) %>%
  gather(Type, TH, "BBN", "4kHz", "8kHz", "16kHz", "32kHz") %>%
  mutate(Type = fct_relevel(Type, "BBN", "4kHz", "8kHz", "16kHz", "32kHz"))


# Change in D' ------------------------------------------------------------
# Calculate difference between pre- and post-hearing loss d'

dprime <-
  TH_data %>%
  filter(Condition %in% c("Baseline", "Post HHL")) %>%
  filter(!(ID %in% HL_not_done)) %>%
  select(ID, Sex, Condition, Type, BG_Type, BG_Intensity, `Dur (ms)`, dprime, dB) %>%
  spread(Condition, dprime) %>%
  mutate(dprime_change = `Post HHL` - Baseline,
         Type = fct_relevel(Type, "BBN", "4kHz", "8kHz", "16kHz", "32kHz"),
         BG = case_when(BG_Type == "NA" & BG_Intensity == "NA" ~ "Quiet",
                        BG_Type == "PNK" & BG_Intensity == "30" ~ "30dB Pink Noise Background",
                        BG_Type == "PNK" & BG_Intensity == "50" ~ "50dB Pink Noise Background",
                        BG_Type == "WN" & BG_Intensity == "50" ~ "50dB White Noise Background",
                        TRUE ~ "ISSUE") %>%
           fct_relevel("Quiet", "30dB Pink Noise Background", "50dB Pink Noise Background", "50dB White Noise Background")) %>%
  gather(Condition, dprime, "dprime_change", "Baseline", `Post HHL`)


# Psychoacuostic Graph -----------------------------------------------------


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
  # stat_summary(fun.data = n_fun, geom = "text", vjust = 5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-1, 91), breaks = c(10,30,50,70,90)) +
  labs(x = "Intensity (dB)",
       y = "Change in d' following HHL (+/- SE)",
       color = "Go Frequency") +
  facet_wrap(~ BG, scale = "fixed", nrow = 3, strip.position = "top") +
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


# BBN Graph (overall)
TH_data %>%
  select(ID, Sex, Condition, Type, BG_Type, BG_Intensity, `Dur (ms)`, dprime, dB) %>%
  filter(!(ID %in% HL_not_done)) %>%
  # filter(Condition != "Recovery") %>%
  filter(Condition != "Recovery 2") %>%
  filter(Condition != "Post 2nd Exposure") %>%
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

# BBN Graph (by sex)
TH_data %>%
  select(ID, Sex, Condition, Type, BG_Type, BG_Intensity, `Dur (ms)`, dprime, dB) %>%
  filter(!(ID %in% HL_not_done)) %>%
  # filter(Condition != "Recovery") %>%
  filter(Condition != "Recovery 2") %>%
  filter(Condition != "Post 2nd Exposure") %>%
  filter(Stim == "BBN") %>%
  # filter(Sex == "Male") %>%
  ggplot(aes(x = dB, y = dprime)) +
  stat_summary(aes(color = Condition, linetype = Sex, group = interaction(Condition, Sex)),
               fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(1)) +
  stat_summary(aes(color = Condition, shape = Sex, group = interaction(Condition, Sex)),
               fun = mean,
               geom = "point", position = position_dodge(1), size = 3) +
  stat_summary(aes(color = Condition, linetype = Sex, group = interaction(Condition, Sex)),
               fun = mean, geom = "line") +
  facet_wrap(~ Type, scale = "free",) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = "grey80")
    # panel.grid.minor.x = element_line(color = "grey80"))
  )

ggsave("Psychoacoustic_BBN_by_Sex.jpg",
       plot = last_plot(), # or an explicit ggplot object name
       path = ProjectFolder,
       width = 1200, height = 600, units = "px", dpi = 100)


# 4-32 Graph (Overall)
TH_data %>%
  select(ID, Sex, Condition, Type, BG_Type, BG_Intensity, `Dur (ms)`, dprime, dB) %>%
  filter(!(ID %in% HL_not_done)) %>%
  # filter(Condition != "Recovery") %>%
  filter(Condition != "Recovery 2") %>%
  filter(Condition != "Post 2nd Exposure") %>%
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

# 4-32 Graph (By Frequency & BG)
TH_data %>%
  select(ID, Sex, Condition, Type, BG_Type, BG_Intensity, `Dur (ms)`, dprime, dB) %>%
  filter(!(ID %in% HL_not_done)) %>%
  # filter(Condition != "Recovery") %>%
  filter(Condition != "Recovery 2") %>%
  filter(Condition != "Post 2nd Exposure") %>%
  filter(Stim == "tone") %>%
  filter(BG_Intensity == "50") %>%
  # filter(Sex == "Male") %>%
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

# 4-32 Graph (By Frequency and Sex)
TH_data %>%
  select(ID, Sex, Condition, Type, BG_Type, BG_Intensity, `Dur (ms)`, dprime, dB) %>%
  filter(!(ID %in% HL_not_done)) %>%
  # filter(Condition != "Recovery") %>%
  filter(Condition != "Recovery 2") %>%
  filter(Condition != "Post 2nd Exposure") %>%
  filter(Stim == "tone") %>%
  # filter(Sex == "Male") %>%
  ggplot(aes(x = dB, y = dprime)) +
  stat_summary(aes(color = Condition, linetype = Sex, group = interaction(Condition, Sex)),
               fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(1)) +
  stat_summary(aes(color = Condition, shape = Sex, group = interaction(Condition, Sex)),
               fun = mean,
               geom = "point", position = position_dodge(1), size = 3) +
  stat_summary(aes(color = Condition, linetype = Sex, group = interaction(Condition, Sex)),
               fun = mean, geom = "line") +
  facet_wrap(~ fct_relevel(Type, "4kHz", "8kHz", "16kHz", "32kHz"), scale = "free") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = "grey80")
    # panel.grid.minor.x = element_line(color = "grey80"))
  )

ggsave("Psychoacoustic_tone_by_Sex.jpg",
       plot = last_plot(), # or an explicit ggplot object name
       path = ProjectFolder,
       width = 2400, height = 1200, units = "px", dpi = 100)



# Reaction time calculation -----------------------------------------------

TH_filter <- function(df) {
  # print(df)
  ID = unique(df$ID) # %>% print
  Condition = unique(df$Condition) # %>% print
  BG_Intensity = unique(df$BG_Intensity) # %>% print
  Dur = unique(df$`Dur (ms)`) # %>% print
  kHz = unique(df$`Freq (kHz)`)
  kHz = if_else(kHz == "0", "BBN", paste0(kHz,"kHz")) # %>% print

  # print(TH)
  # print(paste(ID))

  cuttoff = TH %>% # have to use UQ to force the evaluation of the variable
    filter(ID == UQ(ID) & Condition == UQ(Condition) & BG_Intensity == UQ(BG_Intensity) & `Dur (ms)` == UQ(Dur)) %>%
    pull(UQ(kHz)) #%>% print

  cuttoff = ifelse(identical(cuttoff, numeric(0)), -99, cuttoff) # %>% print
  # ifelse(identical(cuttoff, numeric(0)), df, filter(df, `Inten (dB)` >= UQ(cuttoff))) %>% print

  df %>%
    filter(`Inten (dB)` >= UQ(cuttoff))
}

Data_over_TH <-
  Analysis_data
  ungroup() %>%
  # select(-data_trimmed, -Blocks_trimmed) %>%
  # filter(ID == "Green 1") %>%
  # filter(Stim == "BBN") %>%
  filter(Type == 1 & Response == "Hit") %>%
  filter(`Inten (dB)` != -100) %>%
  mutate(Rat = .$ID, Con = .$Condition, BG = .$BG_Intensity, Dur = .$`Dur (ms)`, kHz = .$`Freq (kHz)`) %>%
  group_by(Rat, Sex, Con, BG, Dur, kHz) %>%
  nest %>%
  mutate(data = map(data, TH_filter)) # %>% print


Rxn_overall <-
  Data_over_TH %>%
  ungroup() %>%
  unnest(data) %>%
  filter(Type == 1 & Response == "Hit") %>%
  filter(`Inten (dB)` != -100) %>%
  # Filter by TH table so that only reaction times above thresholds are included
  group_by(ID, Sex, Condition, BG_Type, BG_Intensity, Stim, `Dur (ms)`, `Freq (kHz)`, `Inten (dB)`) %>%
  do(describe(.$`R Time (ms)`)) %>%
  as_tibble() %>%
  select(-vars)

# Change in reaction time
Rxn_change <-
  Rxn_overall %>%
  filter(Condition %in% c("Baseline", "Post HHL")) %>%
  filter(!(ID %in% HL_not_done)) %>%
  select(ID, Sex, Condition, BG_Type, BG_Intensity, Stim, `Dur (ms)`, `Freq (kHz)`, `Inten (dB)`, mean) %>%
  spread(Condition, mean) %>%
  mutate(Rxn_change = `Post HHL`- Baseline) %>%
  gather(Condition, mean, "Rxn_change")


# React time comparison between overall and daily averaging was basically identical, so daily dropped

# Rxn_daily <-
#   Data_over_TH %>%
#     ungroup() %>%
#     unnest(data) %>%
#     # filter(Date > "2022-2-25" & Date < "2022-2-26") %>%
#     # filter(ID == "Green 1") %>%
#     group_by(ID, Date, Condition, BG_Type, BG_Intensity, Stim, `Dur (ms)`, `Freq (kHz)`, `Inten (dB)`) %>%
#     do(describe(.$`R Time (ms)`)) %>%
#     as_tibble() %>%
#     left_join(.,
#               Data_over_TH %>%
#                 ungroup() %>%
#                 unnest(data_trimmed) %>%
#                 # filter(Date > "2022-2-25" & Date < "2022-2-26") %>%
#                 # filter(ID == "Green 1") %>%
#                 group_by(ID, Date, Condition, BG_Type, BG_Intensity, Stim, `Dur (ms)`, `Freq (kHz)`, `Inten (dB)`) %>%
#                 do(describe(.$`R Time (ms)`)) %>%
#                 as_tibble() %>%
#                 rename_with( ~ paste0(., "_trimmed"), n:se)
#     ) %>%
#     select(-vars) %>%
#     select(ID:`Inten (dB)`, mean, mean_trimmed) %>%
#     group_by(ID, Condition, BG_Type, BG_Intensity, `Dur (ms)`, `Freq (kHz)`, `Inten (dB)`) %>%
#     summarise(mean = mean(mean), mean_trimmed = mean(mean_trimmed))
#
# # React time comparison between averaging methods - basically identical
# Rxn_overall %>%
#   select(ID:mean, mean_trimmed) %>%
#   # rename(mean_all = mean,
#   #        mean_trimmed_all = mean_trimmed) %>%
#   left_join(., Rxn_daily,
#             by = c("ID", "Condition", "Dur (ms)", "Freq (kHz)", "Inten (dB)"),
#             suffix = c("", "_daily")) %>% View




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
  # filter(Condition != "Recovery") %>%
  filter(Condition != "Recovery 2") %>%
  filter(Condition != "Post 2nd Exposure") %>%
  filter(!(ID %in% HL_not_done)) %>%
  filter(`Freq (kHz)` != "0") %>%
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
  facet_wrap(~ `Freq (kHz)`, scale = "free") +
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

# Rxn Comparison ----------------------------------------------------------

model <- function(df) {
  # View(df)
  df = df %>% filter(Type == 1 & Response == "Hit") #%>% View
  # df %>% .$Condition %>% unique() %>% print
  lm = if (df$`Stim Source` == "BBN") lm(`R Time (ms)` ~ Condition*`Inten (dB)`, data = df)
  else lm(`R Time (ms)` ~ Condition*`Inten (dB)`*`Freq (kHz)`, data = df)
  # summary(lm) %>% print
  # summary(lm)$coefficients %>% as.data.frame() %>% rownames_to_column(var = "coefficient") %>% tibble %>% print
  return(lm)
}

lm_p <- function(df) {
  result = summary(df)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column(., var = "coefficient") %>%
    tibble %>%
    mutate(coefficient = gsub("ConditionPost", "Post",coefficient)) %>%
    print
  return(result)
}

# Analysis_data
#   ungroup() %>%
#   unnest(data) %>%
#   # filter(ID == "Green 1") %>% #print
#   # filter(BG_Type == "NA") %>%
#   # filter(Stim == "BBN") %>%
#   # filter(`Dur (ms)` == 50) %>% #View
#   # Filter out any individuals that haven't had hearing loss yet
#   group_by(ID, Condition, BG_Type, BG_Intensity, Stim, `Dur (ms)`) %>%
#   nest() %>% #print
#   spread(Condition, data) %>%
#   # select(-`Post 2nd Exposure`) %>% #print
#   filter(!map_lgl(`Post HHL`, is.null)) %>% #print
#   filter(!map_lgl(Baseline, is.null)) %>% #print
#   gather("Condition", "data", 6:ncol(.)) %>%
#   unnest(cols = c(data)) %>%
#   # prep for model
#   group_by(ID, BG_Type, BG_Intensity, Stim, `Dur (ms)`) %>% #View
#   select(-data_trimmed, -Blocks_trimmed) %>% #View
#   nest %>% #print
#   mutate(lm = map(data, model),
#          summary = map(lm, ~summary(.x)),
#          coefficients = map(lm, lm_p),
#          HHL_estimate = map(coefficients, ~slice(.x, 2:2))) %>%
#   unnest(HHL_estimate)
# # mutate(lm = map(data, model),
# #        summary=list(summary(lm)),
# #        coefficients = map(lm, lm_p)) %>% print#View
#
#
# # Overall Model
# model_data <-
#   Analysis_data
#   ungroup() %>%
#   unnest(data) %>%
#   # filter(ID == "Green 1") %>%
#   # filter(BG_Type == "NA") %>%
#   filter(Stim != "BBN") %>%
#   # filter(`Dur (ms)` == 50) %>% #View
#   # Filter out any individuals that haven't had hearing loss yet
#   group_by(ID, Condition, BG_Type, BG_Intensity, Stim, `Dur (ms)`) %>%
#   nest() %>% #print
#   spread(Condition, data) %>%
#   # select(-`Post 2nd Exposure`) %>% #print
#   filter(!map_lgl(`Post HHL`, is.null)) %>% #print
#   filter(!map_lgl(Baseline, is.null)) %>% #print
#   gather("Condition", "data", 6:ncol(.)) %>%
#   unnest(cols = c(data)) %>%
#   # prep for model
#   # group_by(BG_Type, BG_Intensity, Stim, `Dur (ms)`) %>% #View
#   filter(Type == 1 & Response != "Miss") %>% #print
#   filter(Condition != "Post 2nd Exposure") %>%
#   mutate(BG_Intensity = fct_relevel(BG_Intensity, levels = c("NA", "30", "50")))
#
# m0.lmer = lmer(`R Time (ms)` ~ 1 + (1|ID), REML = T, data =  model_data)
# # m1.lmer = lmer(`R Time (ms)` ~ 1 + (1 + ID | Date), REML = T, data =  model_data)
# m1.lmer = lmer(`R Time (ms)` ~ 1 + `Inten (dB)` + (1|ID), REML = T, data =  model_data)
# m2.lmer = lmer(`R Time (ms)` ~ 1 + `Inten (dB)`*`Freq (kHz)` + (1|ID), REML = T, data =  model_data)
# m3.lmer = lmer(`R Time (ms)` ~ 1 + `Inten (dB)`+`Freq (kHz)` + (1|ID), REML = T, data =  model_data)
# m4.lmer = lmer(`R Time (ms)` ~ 1 + `Inten (dB)`*`Freq (kHz)`*BG_Intensity + (1|ID), REML = T, data =  model_data)
# m5.lmer = lmer(`R Time (ms)` ~ 1 + `Inten (dB)`*`Freq (kHz)`+ BG_Intensity + (1|ID), REML = T, data =  model_data)
# m6.lmer = lmer(`R Time (ms)` ~ 1 + `Inten (dB)`*`Freq (kHz)`*BG_Intensity*Condition + (1|ID), REML = T, data =  model_data)
# m7.lmer = lmer(`R Time (ms)` ~ 1 + `Inten (dB)`*`Freq (kHz)`*BG_Intensity*Condition*Stim + (1|ID), REML = T, data =  model_data)
# anova(m1.lmer, m0.lmer, test = "Chi")
# summary(m6.lmer)
