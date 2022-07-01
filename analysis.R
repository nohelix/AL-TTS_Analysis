
# Daily summary from raw data -------------------------------------------------------------
# Calculates hit rate, false alarm rate, and trial count from data across conditions
# Summarized by day and individual

writeLines("Analyzing summary data (trials/hits/FAs)")

# Summarize Hits, misses, FAs, & CRs for each individual by day
Hit_summary_by_day <-
  Analysis_data %>%
  filter(!(ID %in% HL_not_done)) %>%
  filter(Condition != "Recovery" & Condition != "Recovery") %>%
  group_by(ID, Date, Sex, Condition, Stim, BG_Type, BG_Intensity, Response) %>%
  summarise(count = n()) %>%
  spread(Response, count, fill = 0) %>%
  mutate(Trials = `C.R.` + `F.A.` + Hit + Miss,
         `Hit%` = Hit / (Hit + Miss),
         `FA%` = `F.A.` / (`C.R.` + `F.A.`)) %>%
  # Sanity checking count
  # left_join(., Analysis_data %>%
  #           group_by(ID, Date) %>%
  #           count(), by = c("Date", "ID")) %>%
  # filter(Trials != n)
  # Summarize for each individual by day
  group_by(ID, Sex, Condition, Stim, BG_Type, BG_Intensity) %>%
  summarise(count = n_distinct(Date),
            Trials = mean(Trials, na.rm = T),
            Hit = mean(`Hit%`, na.rm = T),
            FA = mean(`FA%`, na.rm = T)) %>%
  ungroup() %>%
  mutate(BG_Intensity = factor(BG_Intensity, levels = c("NA", "30", "50")),
         Stim = factor(Stim, levels = c("BBN", "tone")))

Hit_summary_by_day_BBN <-
  Hit_summary_by_day %>%
  filter(Stim == "BBN")

Hit_summary_by_day_tone <-
  Hit_summary_by_day %>%
  filter(Stim == "tone")

# Trial Count Analysis ----------------------------------------------------

# ANOVA
Trials.aov = aov(Trials ~ Condition * BG_Intensity, data = Hit_summary_by_day_tone)

# Parametric check
Trials.aov$residuals %>%
  shapiro.test()

# Summary
summary(Trials.aov)

# Hit Rate Analysis ----------------------------------------------------

# ANOVA
Hit.aov = aov(Hit ~ Condition * BG_Intensity, data = Hit_summary_by_day_tone)

# Parametric check
Hit.aov$residuals %>%
  shapiro.test()

# Summary
summary(Hit.aov)

# Non-Parametric ANOVA
kruskal.test(Hit ~ BG_Intensity, data = Hit_summary_by_day_tone)
kruskal.test(Hit ~ Condition, data = Hit_summary_by_day_tone)

kruskal.test(Hit ~ Condition,
             data = Hit_summary_by_day_tone %>%
                    filter(Stim == "tone" & BG_Intensity == "NA"))

kruskal.test(Hit ~ Condition,
             data = Hit_summary_by_day_tone %>%
               filter(Stim == "tone" & BG_Intensity == "50"))

# False Alarm Rate Analysis ----------------------------------------------------

# ANOVA
FA.aov = aov(FA ~ Condition * BG_Intensity, data = Hit_summary_by_day_tone)

# Parametric check
FA.aov$residuals %>%
  shapiro.test()

# Summary
summary(FA.aov)

TukeyHSD(FA.aov)$`BG_Intensity` %>%
  as_tibble(.name_repair = "unique", rownames = "Comparison")

TukeyHSD(FA.aov)$`Condition` %>%
  as_tibble(.name_repair = "unique", rownames = "Comparison")

# Threshold Calculation ---------------------------------------------------
# Signal detection index calculation by the psycho package. We use d' a sensitivity measure.
# https://neuropsychology.github.io/psycho.R/2018/03/29/SDT.html

# Creates a properly formatted table for psycho by adding the overall CR/FA to each row
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

writeLines("Calculating Thresholds")

# Calculate d' and save (along with hit/miss/CR/FA table)
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



# Threshold calculation calculation based on TH_cutoff intercept of fit curve
# LOESS: Local Regression is a non-parametric approach that fits multiple regressions
# see http://r-statistics.co/Loess-Regression-With-R.html
TH_calc <- function(df) {
  # Uncomment to see line fitting by a package which shows line
  # library(drda)
  # drda(dprime ~ dB, data = df) %>% plot
  fit = loess(dprime ~ dB, data = df)
  # plot(fit)
  TH = approx(x = fit$fitted, y = fit$x, xout = TH_cutoff)$y #%>% print
  return(TH)
}


TH <-
  TH_data %>%
  select(ID:`Dur (ms)`, dprime, dB, Type) %>% #print
  mutate(Type = fct_relevel(Type, levels = c("BBN", "4kHz", "8kHz", "16kHz", "32kHz"))) %>% #print
  group_by(ID, Sex, Condition, BG_Type, BG_Intensity, `Dur (ms)`, Type) %>%
  nest() %>%
  mutate(TH = map_dbl(data, TH_calc)) %>%
  select(-data) %>%
  spread(Type, TH)


# Threshold Tables for Viewing --------------------------------------------

# Average Thresholds
Avg_TH_Condition <-
  TH %>%
  filter(Condition %in% c("Baseline", "Post HHL")) %>%
  filter(!(ID %in% HL_not_done)) %>%
  group_by(Condition, BG_Type, BG_Intensity) %>%
  summarise("BBN_avg" = mean(BBN, na.rm = TRUE) %>% round(digits = 0),
            "4kHz_avg" = mean(`4kHz`, na.rm = TRUE) %>% round(digits = 0),
            "8kHz_avg" = mean(`8kHz`, na.rm = TRUE) %>% round(digits = 0),
            "16kHz_avg" = mean(`16kHz`, na.rm = TRUE) %>% round(digits = 0),
            "32kHz_avg" = mean(`32kHz`, na.rm = TRUE) %>% round(digits = 0))

Avg_TH <-
  TH %>%
  filter(Condition %in% c("Baseline", "Post HHL")) %>%
  filter(!(ID %in% HL_not_done)) %>%
  group_by(BG_Type, BG_Intensity) %>%
  summarise("BBN" = mean(BBN, na.rm = TRUE) %>% round(digits = 0),
            "4kHz" = mean(`4kHz`, na.rm = TRUE) %>% round(digits = 0),
            "8kHz" = mean(`8kHz`, na.rm = TRUE) %>% round(digits = 0),
            "16kHz" = mean(`16kHz`, na.rm = TRUE) %>% round(digits = 0),
            "32kHz" = mean(`32kHz`, na.rm = TRUE) %>% round(digits = 0)) %>%
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


# Reaction time calculation -----------------------------------------------

writeLines("Calculating average RXN time")

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
  Analysis_data %>%
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
  mutate(Rxn_change = `Post HHL` - Baseline) %>%
  gather(Condition, mean, "Rxn_change")


# React time comparison between overall and daily averaging was basically identical, so daily dropped

Rxn_daily <-
  Data_over_TH %>%
    ungroup() %>%
    unnest(data) %>%
    # filter(Date > "2022-2-25" & Date < "2022-2-26") %>%
    # filter(ID == "Green 1") %>%
    group_by(ID, Date, Condition, BG_Type, BG_Intensity, Stim, `Dur (ms)`, `Freq (kHz)`, `Inten (dB)`) %>%
    do(describe(.$`R Time (ms)`)) %>%
    group_by(ID, Condition, BG_Type, BG_Intensity, `Dur (ms)`, `Freq (kHz)`, `Inten (dB)`) %>%
    summarise(mean = mean(mean))

# React time comparison between averaging methods - basically identical
Rxn_overall %>%
  select(ID:mean) %>%
  left_join(., Rxn_daily,
            by = c("ID", "Condition", "Dur (ms)", "Freq (kHz)", "Inten (dB)", "BG_Type", "BG_Intensity"),
            suffix = c("", "_daily"))


# Rxn Comparison ----------------------------------------------------------

# ANOVA
Rxn.aov = aov(mean ~ Condition * `Freq (kHz)` * `Inten (dB)` * BG_Intensity, data = Rxn_overall)

# Parametric check
Rxn.aov$residuals %>%
  shapiro.test()

# Summary
summary(Rxn.aov)

# TukeyHSD(Rxn.aov)
