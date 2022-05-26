# NOTES -------------------------------------------------------------------
# Much of this is hardcoded based on the file structure where we store the 
# Auerbach lab uses, the 'summary' workbook, and sheet layout. Altering these
# WILL BREAK this script.

# The individual files should be organized in to folders by day, with the raw 
# data CSV's being in another /export subfolder. This is determined by the 
# current Behavior_Analysis matlab script.

# Clear workspace ---------------------------------------------------------
  rm(list=ls())

# Global Variables --------------------------------------------------------

# The folder where the 'summary' worksheet Noise_TTS_Gp1_Green-Orange.xlsx is: 
  MainFolder = 'C:/Users/Noelle/Box/Behavior Lab/Projects (Behavior)/TTS'

# Should be greater than the maximum lines in the longest sheet of 'summary'
  range = "A3:X600"
  
  ProjectFolder = 'C:/Users/Noelle/Box/Auerbach Lab (Personal)/TTS Analysis'
  
  HL_not_done = c("Orange 4", "Orange 5", "Green 2", "Green 3")

# Working directory -------------------------------------------------------
  setwd(MainFolder)

# Package loading ---------------------------------------------------------
  
  # data loading/manipulation
  library(readxl); library(tidyverse); library(magrittr); library(dplyr); library(tidyr)
  
  # Analysis
  library(reshape2); library(psych); library(psycho); library(lme4); library(lmerTest)
  # library(gtools); 
  
  # Data visualization
  library(ggplot2); library(forcats)
  
  # Exporting data
  library(writexl)
  

# Xlsx multisheet import --------------------------------------------------
# Read All Excel Sheets
# Modified from https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames
read_excel_allsheets <- function(filename, range, sheetlist, tibble = FALSE) {
  # but if you like tidyverse tibbles (the default with read_excel) then just pass tibble = TRUE
  #sheets <- readxl::excel_sheets(filename)
  #sheetname <- lapply(sheets, function(X) read_excel(filename, sheet = X, range = "A1:A1", col_names = FALSE) %>% toString %>% print)
  
  #iterate through each sheet (lapply) and create an individual dataframe
  x <- lapply(sheetlist, function(X) readxl::read_excel(filename, sheet = X, range=range) %>% 
                # hard-coded column selection; this will fail if the columns get moved - i.e. Fmr1 Group 1
                select(1:4, 6:8,21,24) %>% 
                rename(Date = 1, File = 2,
                       Weight = 3, WeightChange = 4,
                       Trials = 5, Hit = 6, FA = 7,
                       Thresholds = 8, Phase = 9) %>%
                # remove empty days
                filter(Date != NA | File != "Did not run") %>%
                # change type of columns
                mutate(Trials = as.numeric(Trials),
                       Hit = as.numeric(Hit),
                       FA = as.numeric(FA)) %>%
                # Add extra columns to deal with the eventual concatenation (typical wide to tall conversion)
                mutate(ID= X,
                       Genotype=read_excel(filename, sheet = X, range = "A2:A2", col_names = FALSE) %>% toString(),
                      # Regex: This uses the summary sheet.It will need to be different for the actual file names.
                      # Group 1 - select Frequency which is the beginning of the file to the first _ (eg. BBN, 4-32kHz).
                      # Group 2 - select Intensity which from the 1st _ to dB. This should catch both ranges (eg 30-90dB), single dB (eg 60dB), and words (eg MIX, MIX5step) 
                      # Group 3 - select Duration which is the 1st number(s) preceding the ms (eg. 50ms, 50-300ms).
                      # Optional second time should only be 1ms and represents a shorter response window.
                       Frequency=gsub("(^.*?)_(.*?dB)_(.*?ms)_.*$","\\1", File), # extracts frequency from name of file
                       Intensity=gsub("(^.*?)_(.*?dB)_(.*?ms)_.*$","\\2", File), # extracts intensity from name of file
                       Duration=gsub("(^.*?)_(.*?dB)_(.*?ms)_.*$","\\3", File), # extracts duration from name of file 
                       BG_Type=ifelse(grepl("_BG_", File), 
                                      gsub("(^.*?_BG_)(.*?)_(\\d+).*", "\\2", File), 
                                      "NA"), # extracts the background type (PNK, BBN, WN, etc) from name of file; useful for filtering
                       BG_Intensity=ifelse(grepl("_BG_", File),
                                           gsub("(^.*?_BG_)(.*?)_(\\d+).*", "\\3", File),
                                           "NA"))) # extracts the background intensity (30, 50, etc) from name of file
  
  # Converts from the default tibble structure (flexibile, easy to read, less predictable) to hard data frame
  if(!tibble) x <- lapply(x, as.data.frame)
  
  # name the elements in the list with the sheet names
  names(x) <- gsub("\\s", "", sheetlist)
  
  # returns list
  x
}


# Import Data -------------------------------------------------------------
# Note that the excel spreadsheet range is hardcoded to 9,999 lines, so if the
# data exceds that, it will not be loaded.

  # Load TTS spreadsheet (hard coded)
  TTS_RatID_list <- excel_sheets("Noise_TTS_Gp1_Green-Orange.xlsx") %>% .[1:11] %>% as.list()

  # The message creation for the log is slow & spammy so I have suppressed the 'New Name' message
  suppressMessages(
  TTS_Data_Raw <- read_excel_allsheets("Noise_TTS_Gp1_Green-Orange.xlsx", 
                                       range = range, sheetlist=TTS_RatID_list, tibble=T)
  )

# Data Processing ---------------------------------------------------------

  TTS_Data <-
    TTS_Data_Raw %>%
  # Concat tables
      bind_rows() %>% 
  # Filter unclassified days
      filter(!(is.na(Phase))) %>%
  # Filter out FA correction, Maintaince, & Errors
      filter(!(Phase %in% c("FA correction", "Maintaince", "Error"))) %>%
  # Filter out Training and retraining
      filter(!(Phase %in% c("Training", "Retraining", "Training, tones", "Reset"))) %>%
  # Filter out Post-ABR
      filter(!(Phase %in% c("Post ABR", "Post ABR, post-HL", "Post ABR, post-HL2"))) %>%
  # if_else statement to detect grouping - either pre-hearing loss (i.e. Baseline), post 1st exposure or post 2nd exposure
    mutate(Condition = case_when(grepl("recovery$", Phase) ~ "Recovery",
                                  grepl("post-HL$", Phase) ~ "Post HHL",
                                  grepl("recovery2$", Phase) ~ "Recovery 2",
                                  grepl("post-HL2$", Phase) ~ "Post 2nd Exposure",
                                  TRUE ~ "Baseline"),
           Condition = fct_relevel(Condition,levels = c("Baseline", "Recovery", "Post HHL", "Recovery 2", "Post 2nd Exposure")),
           Sex = if_else(grepl("Green", ID), "Male", "Female"),
           "BBN_TH" = if_else(Frequency=="BBN", # If only 1 number (i.e. only digits from start to finish), its a BBN threshold
                              gsub("(^.\\d+)$", "\\1", Thresholds), NA_character_) %>% as.numeric() %>% round(),
           # Regex splits up the 4 listed limited thresholds calculated each day into separate columns by looking for the conserved spacer of ", "
           "4_TH" = if_else(Frequency=="4-32kHz", 
                            gsub("^(.*?), (.*?), (.*?), (.+?)$", "\\1", Thresholds), NA_character_) %>% as.numeric() %>% round(),
           "8_TH" = if_else(Frequency=="4-32kHz", 
                            gsub("^(.*?), (.*?), (.*?), (.+?)$", "\\2", Thresholds), NA_character_) %>% as.numeric() %>% round(),
           "16_TH" = if_else(Frequency=="4-32kHz", 
                            gsub("^(.*?), (.*?), (.*?), (.+?)$", "\\3", Thresholds), NA_character_) %>% as.numeric() %>% round(),
           "32_TH" = if_else(Frequency=="4-32kHz", 
                            gsub("^(.*?), (.*?), (.*?), (.+?)$", "\\4", Thresholds), NA_character_) %>% as.numeric() %>% round(),
    )


# TH Summary table: Thresholding ----------------------------------------------

# # Summary Table
#   Thresholds <-
#     TTS_Data %>%
#   # Group by ID, experiment type, etc
#     group_by(ID, Condition, BG_Type, BG_Intensity, Duration) %>%
#   # Summarize for each individual
#     select("ID", "BBN_TH", "4_TH", "8_TH", "16_TH", "32_TH") %>% 
#     do(
#       describeBy(., group= "ID", omit=TRUE, check=TRUE, mat=TRUE, digits=0) %>% 
#         rename(ID = group1, ) %>% #print)
#         mutate(Type = gsub("(^.*?)_TH.*$", "\\1", row.names(.))) #%>% print # Get the type of Threshold from the name, needs to work for numbers and letters (i.e. 4, 8, 16, 32, and BBN)
#     ) %>% #View
#   # Orangaize and simplify the table
#     select(ID, Type, BG_Type, BG_Intensity, Duration, n, mean, median, min, max) %>%
#     filter(ID != "ID") %>%
#     arrange(ID, factor(BG_Intensity, levels = c("NA","30","50")), factor(Type, levels = c("BBN","4","8", "16","32"))) %>%
#     filter(!(BG_Intensity %in% c("30", "50") & Type == "BBN")) %>%
#     print
# 
#   Thresholds_simple <-
#     Thresholds %>%
#     select(ID, Condition, BG_Type, BG_Intensity, Duration, Type, mean) %>% 
#     dcast(ID + Condition + BG_Type + BG_Intensity + Duration ~ Type, mean) %>%
#     arrange(factor(BG_Intensity, levels = c("NA","30","50"))) %>%
#     relocate("16", .after = "8") %>%
#     relocate("32", .after = "16") %>%
#     print

  
  
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
    ggplot(aes(x=fct_relevel(BG_Intensity, "NA","30","50"), y=value)) +
      geom_boxplot(aes(fill=Condition)) +
      labs(title="BBN & 4-32kHz",
           x="Background Intensity (dB)") +
      facet_wrap(.~ fct_relevel(variable, "Trials", "Hit", "FA"), scales="free_y") +
      theme_classic() +
      theme(
        axis.title.x = element_text(size=14, color="black"),   # size of x-axis title
        axis.title.y = element_text(size=14, color="black"),   # size of y-axis title
        axis.text.x = element_text(size=12, color="black"),    # size of x-axis text
        axis.text.y = element_text(size=12, color="black"),    # size of y-axis text
        strip.text.x = element_text(size = 14, color="black"), # size of facet titles
        legend.position="right"                                 # hide legend
      )
  
  
  ggsave("Descriptive_trials_FA_hit.jpg",
         plot = last_plot(), # or an explicit ggplot object name
         path = ProjectFolder, 
         width = 1200, height = 600, units = "px", dpi = 100)
    
  
# Get & verify raw files list ---------------------------------------------
# This needs to walk a directory, find the .csv file that starts with the same
# name START as the 'file' column.

  # Get folder and file names
  File_list <-
    TTS_Data %>%
      # Select date and file columns
      select(Date, File, ID, Sex, Condition, BG_Type, BG_Intensity) %>%
      # filter(Date > "2022-01-01") %>%
      # Change date to same format as folder names
      mutate(Folder = as.character.Date(Date) %>% gsub("-", "", .)) %>% print

  # Get EXPECTED file list
  File_list <-
    File_list %>%
    group_by(ID, Sex, Date, File) %>%
    # filter(Folder == "20220106") %>%
    do(
      list.files(path = paste("./data/", .$Folder, "/export/", sep = ""),
                 pattern = paste(gsub(" ", "", .$ID), "_", .$File, "_.*.csv", sep = ""), # The IDs have a space between color and # but the files don't. This removes the space.
                 full.names = TRUE) %>% 
      tibble(FileName = .)
      ) %>% 
    left_join(File_list, ., by = c("ID", "File", "Date")) %>% # needs to be rejoined to the original list to ID missing files.
    select(-Sex.y) %>%
    rename(Sex = Sex.x)
    
  # Get complete file list
  File_list <-
    File_list %>%
    group_by(ID, Date, File) %>%
    # filter(Folder == "20220106") %>%
    do(
      list.files(path = paste("./data/", .$Folder, "/export/", sep = ""),
                 # added line start to force the ID on the Matlab auto labels
                 pattern = paste("^", gsub(" ", "", .$ID), "_", sep = ""),
                 full.names = TRUE) %>% 
        tibble(FileName = .)
    ) %>% 
    left_join(File_list, ., by = c("ID", "File", "Date"), suffix = c("_expected", "_complete"))

  # Check for missing raw data csv files
  File_list_verify <-
    File_list %>%
    mutate(
      ActualFile = gsub("./data/\\d+/export/.+?_", "", FileName_complete), # Remove the file folder and the automatically added animal ID from the mapping
      ActualFile = gsub("_\\d{8}-\\d+.*_raw_data.csv", "", ActualFile), # Remove the file ending that is automatically added starting with the Date and running through .csv. This includes the box #
      Date2 = gsub("./data/(\\d{4})(\\d{2})(\\d{2})/", "\\1-\\2-\\3", FileName_complete) %>% as.Date(), # Get the date from the folder
      ID2 = gsub(".*/export/(\\D+?)(\\d+?)_.*", "\\1 \\2", FileName_complete), # Get the ID from the automatic labeling, note the 2 groups select the color then number to allow for formating
      DateCheck = if_else(Date == Date2, "Good", "BAD", missing=NA_character_),
      IDCheck = if_else(ID == ID2, "Good", "BAD", missing=NA_character_),
      File_Match = case_when(IDCheck != DateCheck ~ "ISSUE",
                             IDCheck == DateCheck & IDCheck == "Good" ~ "Good",
                             (IDCheck == DateCheck & is.na(DateCheck)) ~ NA_character_,)
    ) %>%
    relocate(ActualFile, .after = File)

  # Missing raw files, all missing files do not open for export in our current Behavior_Analysis script 
  File_list_verify%>%
    filter(is.na(FileName_complete)) %>%
  # .mat files from 9-22-2021 and earlier are in the old format and can not be exported currently
    filter(Date > "2021-9-22") %>%
    arrange(Date) %>%
    View

  # ID files with unexpected names  
  setwd(ProjectFolder)
  File_list_verify%>%
    filter(!(is.na(FileName_complete) & is.na(FileName_expected))) %>%
    write_xlsx(., "File_Check.xlsx")
  setwd(MainFolder)

# Import Reaction time ----------------------------------------------------
# For efficiency make sure the files are available 'offline' i.e. locally.
# This needs to walk the directory and find the .csv file designated in the file list.
# The message creation for the log is slow & spammy so I have suppressed the 'New Name' message
# However, given the total number of files this still take about 1 minute so only run when needed
# by un-commenting the code

  suppressMessages(
  Raw_Data <-
    File_list %>%
      # filter(Date > "2021-11-02" & Date < "2021-11-03") %>%
      filter(!(is.na(FileName_complete))) %>% # print
      group_by(Date, ID, Sex, FileName_complete, Condition, BG_Type, BG_Intensity) %>%
      do(
        .$FileName_complete %>%
        map_dfr(read_csv, col_select = c(-11,-10), col_types = cols("Time (s)" = col_character()))
      ) %>%
      mutate(
        Date2 = gsub("./data/(\\d{4})(\\d{2})(\\d{2})/", "\\1-\\2-\\3", FileName_complete) %>% as.Date(), # Get the date from the folder
        ID2 = gsub(".*/export/(\\D+?)(\\d+?)_.*", "\\1 \\2", FileName_complete), # Get the ID from the automatic labeling, note the 2 groups select the color then number to allow for formating
        DateCheck = if_else(Date == Date2, "Good", "BAD", missing="NA"),
        IDCheck = if_else(ID == ID2, "Good", "BAD", missing="NA"),
        File_Match = case_when(IDCheck != DateCheck ~ "ISSUE",
                               IDCheck == DateCheck & IDCheck == "Good" ~ "Good",
                              (IDCheck == DateCheck & is.na(DateCheck)) ~ NA_character_,)
        ) %>%
      ungroup %>%
      select(-c(FileName_complete, Date2, ID2, DateCheck, IDCheck)) # %>% View
  )


  # Sanity check that Trials and the actual line count match
  # NOTE: this required manually renaming a file (Orange5_Orange5_4-32kHz_MIXdB_50ms_4s_20220420-130546_BOX#001_raw_data.csv)
  # as the Orange5_Green5 seemed to be causing issues
  Raw_Data %>%
    group_by(ID, Date) %>%
    count() %>%
    left_join(TTS_Data, ., by = c("Date", "ID")) %>%
    select(ID, Date, Trials, n) %>%
    filter(Trials != n) %>%
    View()
  

# Sample selection --------------------------------------------------------
# BBN & single frequency comes in blocks of 10 with either 5 or 10dB steps
# 4-32kHz comes in blocks of 40 with either 5 or 10dB steps

  # Drops the 1st block of every recording which is of different size based on they type of stimulus
  slicer <- function(df) {
    Stim = df$`Stim Source` %>% unique
    multiple = case_when(Stim == "tone" ~ 40, Stim == "BBN" ~ 10)
    total = count(df)
    start = (multiple+1)
    blocks = (total/multiple) %>% floor() %>% as.numeric()
    final = multiply_by(blocks, multiple)
    table = slice(df, as.numeric(start):as.numeric(final)) 
    return(table)
  }
  
  # gets block count based on stimulus type
  block_count <- function(df) {
    Stim = df$`Stim Source` %>% unique
    multiple = case_when(Stim == "tone" ~ 40, Stim == "BBN" ~ 10)
    total = count(df)
    blocks = (total/multiple) %>% floor() %>% as.numeric()
    return(blocks)
  }

# Select samples and save to nested main dataframe
  # Feels like I should be able to create a master data frame by Rat & stim to simplify things
  
  Data <-
    Raw_Data %>%
    mutate(Stim = .$`Stim Source`)
  
  Data_trimmed <-
    Data %>%
    group_by(Date, ID, Sex, Stim) %>%
    nest() %>%
    mutate(data_no1st = map(data, slicer),
           Blocks_trimmed = map_dbl(data_no1st, block_count))

  
  # Calculate the maximum number of blocks to keep every day
  BBN_maxBlock <-
    Data_trimmed %>%
      filter(Stim == "BBN") %>%
      .$Blocks_trimmed %>%
      min()
  
  # Set a reasonable minimum but does not discard days that are below the minimum 
  # under the assumption that exhaustion/lack of motivation isn't an issue
  tone_maxBlock <-
    Data_trimmed %>%
    filter(Stim == "tone") %>%
    # filter(Blocks_trimmed >= 6) %>%
    .$Blocks_trimmed %>%
    min() %>%
    if_else(. >=5, ., 5)
  
  
  # removes extra blocks based on a minimum (set above), to account for exhaustion/motivation fall off.
  trimmer <- function(df) {
    Stim = df$`Stim Source` %>% unique
    multiple = case_when(Stim == "tone" ~ 40, Stim == "BBN" ~ 10)
    max = case_when(Stim == "tone" ~ tone_maxBlock, Stim == "BBN" ~ BBN_maxBlock)
    total = count(df)
    blocks = (total/multiple) %>% floor() %>% as.numeric()
    final = multiply_by(max, multiple)
    table = slice(df, 1:as.numeric(final)) 
    return(table)
  }

# Add matched Blocks to main dataframe
  Data_trimmed <-
    Data_trimmed %>%
      filter(Blocks_trimmed >= tone_maxBlock) %>%
      mutate(data_block = map(data_no1st, trimmer))
  
  Data_no1st <-
    Data_trimmed %>%
    select(-data, -Blocks_trimmed, -data_block) %>%
    unnest(data_no1st)
  
  Data_trimmedBlocks <- 
    Data_trimmed %>%
    select(-data, -Blocks_trimmed, -data_no1st) %>%
    unnest(data_block)
  
# Threshold Calculation ---------------------------------------------------
  
  # Group into hits, misses, CRs and FAs in prep for calculating d'
  # response_counter <- function(df) {
  #   df %>%
  #     unnest() %>%
  #     group_by(ID, Condition, `Stim Source`, BG_Type, BG_Intensity, `Dur (ms)`, Type, `Freq (kHz)`, `Inten (dB)`, Response) %>% #View
  #     summarise(count = n(), .groups = "keep") %>%
  #     spread(Response, count) %>% #View
  #     group_by(ID, Condition, `Stim Source`, BG_Type, BG_Intensity, `Dur (ms)`) # %>% print
  # }
  
  dprime_table <- function(df) {
    # print(df)
    check = df %>% filter(Type == 0) %>% count() %>% as.numeric() #%>% print
    CRnum = (if (check == 1) filter(df, Type == 0) %>% .$C.R. %>% as.numeric() else check) #%>% print
    FAnum = (if (check == 1) filter(df, Type == 0) %>% .$F.A. %>% as.numeric() else check) #%>% print
    new_df = df %>% filter(Type == 1) %>% rename(CR = C.R., FA = F.A.) %>%
      mutate(CR = if (is.na(CR)) CRnum,
             FA = if (is.na(FA)) FAnum,
             Hit = as.numeric(Hit),
             Miss = as.numeric(Miss)) %>% replace(is.na(.), 0) #%>% print
    return(new_df)
  }
  
  dprimer <- function(df) {
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

  
  # The issue is that the overall datafram is still grouped by date (and needs to be for the slicing of samples above TH....)
  # so I am not sure how to do this except to run through the whole thing 3 times with different data frames - all data, no 1st trial, trimmed blocks
  
  
  TH_data <-
    Data %>%
      group_by(ID, Sex, Condition, Stim, BG_Type, BG_Intensity, `Dur (ms)`, Type, `Freq (kHz)`, `Inten (dB)`, Response) %>% #View
      summarise(count = n()) %>% 
      spread(Response, count) %>% #View
      group_by(ID, Sex, Condition, Stim, BG_Type, BG_Intensity, `Dur (ms)`) %>% #print
      nest() %>%
      mutate(dprime_data = map(data, dprime_table),
             dprime = map(dprime_data, dprimer)) %>% #print
      unnest(dprime) #%>% print
  
  TH_cutoff <- 1.5
  
  # Pulling the wrong line
  # ID      Sex   Condition Stim  BG_Type BG_Intensity `Dur (ms)` dprime    dB Type  score
  # <chr>   <chr> <fct>     <chr> <chr>   <chr>             <dbl>  <dbl> <dbl> <fct> <dbl>
  # 2 Green 1 Male  Baseline  tone  NA      NA                   50  1.28      0 32kHz 0.219
  # 3 Green 1 Male  Baseline  tone  NA      NA                   50  1.17      5 32kHz 0.327
  # 4 Green 1 Male  Baseline  tone  NA      NA                   50  1.91     10 32kHz 0.406
  
  TH <-
    TH_data %>%
      # filter(ID == "Green 1") %>%
      # filter(Stim == "tone") %>%
      # filter(Type == "32kHz") %>% 
      select(ID:`Dur (ms)`, dprime, dB, Type) %>% #print
      mutate(Type = fct_relevel(Type, levels = c("BBN", "4kHz", "8kHz", "16kHz", "32kHz")),
             score = abs(dprime - TH_cutoff)) %>% #print
      filter(score < 1) %>%
      group_by(ID, Sex, Condition, BG_Type, BG_Intensity, `Dur (ms)`, Type) %>%
      do(
        filter(., rank(.$score) == 1 | rank(.$score) == 2) %>% #print)
        summarize(TH = weighted.mean(.$dB, (1-.$score))) %>% #print
        round(., digits=0)
      ) %>%
      spread(Type, TH)
  
  
  # Thresholds_simple %>% 
  #   as_tibble() %>% 
  #   # filter(ID == "Green 1") %>%
  #   filter(!(Duration == "50-300ms")) %>%
  #   mutate(`Dur (ms)` = gsub("(^\\d+)ms", "\\1", Duration) %>% as.numeric()) %>%
  #   select(-Duration) %>% #print
  #   left_join(TH, .,
  #             by = c("ID", "Condition", "BG_Type", "BG_Intensity", "Dur (ms)"),
  #             suffix = c("", ".old")) %>% print
  #   # filter(ID == "Green 1") %>% print
  
  # For Ben Viewing
  # TH %>% 
  #   filter(`Dur (ms)` != "100") %>% 
  #   filter(Condition %in% c("Baseline")) %>% 
  #   filter(!(ID %in% HL_not_done)) %>% 
  #   filter(BG_Intensity %in% c("NA", "50")) %>% 
  #   # write.table("clipboard", sep="\t", row.names=FALSE) %>%
  #   View
  
  # Average Thresholds
  Avg_TH_Condition <-
    TH %>%
      filter(Condition %in% c("Baseline", "Post HHL")) %>% 
      filter(!(ID %in% HL_not_done)) %>%
      group_by(Condition, BG_Type, BG_Intensity) %>%
      summarise("BBN_avg"=mean(BBN, na.rm = TRUE),
                "4kHz_avg"=mean(`4kHz`, na.rm = TRUE),
                "8kHz_avg"=mean(`8kHz`, na.rm = TRUE),
                "16kHz_avg"=mean(`16kHz`, na.rm = TRUE),
                "32kHz_avg"=mean(`32kHz`, na.rm = TRUE))
  
  Avg_TH <-
    TH %>%
      filter(Condition %in% c("Baseline", "Post HHL")) %>% 
      filter(!(ID %in% HL_not_done)) %>%
      group_by(BG_Type, BG_Intensity) %>%
      summarise("BBN"=mean(BBN, na.rm = TRUE),
                "4kHz"=mean(`4kHz`, na.rm = TRUE),
                "8kHz"=mean(`8kHz`, na.rm = TRUE),
                "16kHz"=mean(`16kHz`, na.rm = TRUE),
                "32kHz"=mean(`32kHz`, na.rm = TRUE)) %>%
      gather(Type, TH, "BBN", "4kHz", "8kHz", "16kHz", "32kHz") %>%
      mutate(Type=fct_relevel(Type, "BBN", "4kHz", "8kHz", "16kHz", "32kHz"))


# Change in D' ------------------------------------------------------------

  dprime <-
  TH_data %>%
    filter(Condition %in% c("Baseline", "Post HHL")) %>%
    filter(!(ID %in% HL_not_done)) %>%
    select(ID, Sex, Condition, Type, BG_Type, BG_Intensity, `Dur (ms)`, dprime, dB) %>%
    spread(Condition, dprime) %>%
    mutate(dprime_change = `Post HHL`-Baseline,
           Type=fct_relevel(Type, "BBN", "4kHz", "8kHz", "16kHz", "32kHz"),
           BG=case_when(BG_Type == "NA" & BG_Intensity == "NA" ~ "Quiet",
                        BG_Type == "PNK" & BG_Intensity == "30" ~ "30dB Pink Noise Background",
                        BG_Type == "PNK" & BG_Intensity == "50" ~ "50dB Pink Noise Background",
                        BG_Type == "WN" & BG_Intensity == "50" ~ "50dB White Noise Background",
                        TRUE ~ "ISSUE") %>% 
              fct_relevel("Quiet", "30dB Pink Noise Background", "50dB Pink Noise Background", "50dB White Noise Background")) %>%
    gather(Condition, dprime, "dprime_change", "Baseline", `Post HHL`)
  

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
    ggplot(aes(x=dB, y=dprime, color=Type, group=Type)) +
      geom_hline(yintercept=0, linetype="dashed", size=1.2) +
      # Add vline for each average threshold
      geom_vline(aes(xintercept=TH, color=Type), linetype="longdash", size=1, show.legend = FALSE) +
      # geom_text(aes(label=..count..), stat = "count", hjust=0, vjust=0) +
      stat_summary(fun = mean,
                   fun.min = function(x) mean(x) - se(x),
                   fun.max = function(x) mean(x) + se(x),
                   geom = "errorbar", width=1, position=position_dodge(1)) +
      stat_summary(fun = mean, geom = "point", position=position_dodge(1), size=3) +
      stat_summary(fun=mean, geom="line") +
      # stat_summary(fun.data = n_fun, geom = "text", vjust = 5, show.legend = FALSE) +
      scale_x_continuous(limits = c(-1, 91), breaks=c(10,30,50,70,90)) +
      labs(x="Intensity (dB)",
           y="Change in d' following HHL (+/- SE)",
           color="Go Frequency") +
      facet_wrap(~ BG, scale="fixed", nrow = 3, strip.position="top") +
      theme_classic() + 
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_line(colour="grey80")
        # panel.grid.minor.x = element_line(colour="grey80"))
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
    ggplot(aes(x=dB, y=dprime)) +
    stat_summary(aes(color=Condition, group=Condition),
                 fun = mean,
                 fun.min = function(x) mean(x) - se(x),
                 fun.max = function(x) mean(x) + se(x),
                 geom = "errorbar", width=1, position=position_dodge(1)) +
    stat_summary(aes(color=Condition, group=Condition),
                 fun = mean,
                 geom = "point", position=position_dodge(1), size=3) +
    stat_summary(aes(color=Condition, group=Condition), 
                 fun=mean, geom="line") +
    facet_wrap(~ Type, scale="free") +
    theme_classic() + 
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.major.x = element_line(colour="grey80")
      # panel.grid.minor.x = element_line(colour="grey80"))
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
    ggplot(aes(x=dB, y=dprime)) +
      stat_summary(aes(color=Condition, linetype=Sex, group=interaction(Condition, Sex)),
                   fun = mean,
                   fun.min = function(x) mean(x) - se(x),
                   fun.max = function(x) mean(x) + se(x),
                   geom = "errorbar", width=1, position=position_dodge(1)) +
      stat_summary(aes(color=Condition, shape=Sex, group=interaction(Condition, Sex)),
                   fun = mean,
                   geom = "point", position=position_dodge(1), size=3) +
      stat_summary(aes(color=Condition, linetype=Sex, group=interaction(Condition, Sex)), 
                   fun=mean, geom="line") +
      facet_wrap(~ Type, scale="free",) +
      theme_classic() + 
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_line(colour="grey80")
        # panel.grid.minor.x = element_line(colour="grey80"))
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
    ggplot(aes(x=dB, y=dprime)) +
    stat_summary(aes(color=Condition, group=Condition),
                 fun = mean,
                 fun.min = function(x) mean(x) - se(x),
                 fun.max = function(x) mean(x) + se(x),
                 geom = "errorbar", width=1, position=position_dodge(1)) +
    stat_summary(aes(color=Condition, group=Condition),
                 fun = mean,
                 geom = "point", position=position_dodge(1), size=3) +
    stat_summary(aes(color=Condition, group=Condition), 
                 fun=mean, geom="line") +
    facet_wrap(~ BG_Intensity, scale="free", nrow = 3, strip.position="top") +
    theme_classic() + 
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.major.x = element_line(colour="grey80")
      # panel.grid.minor.x = element_line(colour="grey80"))
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
    ggplot(aes(x=dB, y=dprime)) +
    geom_hline(yintercept = 1.5, linetype = "dashed", size=1.2) +
    stat_summary(aes(color=Condition, group=Condition),
                 fun = mean,
                 fun.min = function(x) mean(x) - se(x),
                 fun.max = function(x) mean(x) + se(x),
                 geom = "errorbar", width=1, position=position_dodge(1)) +
    stat_summary(aes(color=Condition, group=Condition),
                 fun = mean,
                 geom = "point", position=position_dodge(1), size=3) +
    stat_summary(aes(color=Condition, group=Condition), 
                 fun=mean, geom="line") +
    facet_wrap(~ BG_Intensity + fct_relevel(Type, "4kHz", "8kHz", "16kHz", "32kHz"), scale="free") +
    xlim(20, 61)+
    theme_classic() + 
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.major.x = element_line(colour="grey80")
      # panel.grid.minor.x = element_line(colour="grey80"))
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
    ggplot(aes(x=dB, y=dprime)) +
    stat_summary(aes(color=Condition, linetype=Sex, group=interaction(Condition, Sex)),
                 fun = mean,
                 fun.min = function(x) mean(x) - se(x),
                 fun.max = function(x) mean(x) + se(x),
                 geom = "errorbar", width=1, position=position_dodge(1)) +
    stat_summary(aes(color=Condition, shape=Sex, group=interaction(Condition, Sex)),
                 fun = mean,
                 geom = "point", position=position_dodge(1), size=3) +
    stat_summary(aes(color=Condition, linetype=Sex, group=interaction(Condition, Sex)), 
                 fun=mean, geom="line") +
    facet_wrap(~ fct_relevel(Type, "4kHz", "8kHz", "16kHz", "32kHz"), scale="free") +
    theme_classic() + 
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.major.x = element_line(colour="grey80")
      # panel.grid.minor.x = element_line(colour="grey80"))
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
    Data %>%
      ungroup() %>% 
      # select(-data_trimmed, -Blocks_trimmed) %>%
      # filter(ID == "Green 1") %>%
      # filter(Stim == "BBN") %>%
      filter(Type == 1 & Response == "Hit") %>%
      filter(`Inten (dB)` != -100) %>%
      mutate(Rat = .$ID, Con = .$Condition, BG = .$BG_Intensity, Dur =.$`Dur (ms)`, kHz = .$`Freq (kHz)`) %>%
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
      mutate(Rxn_change = `Post HHL`-Baseline) %>%
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
    ggplot(aes(x=`Inten (dB)`, y=mean)) +
      # geom_line(aes(color=Condition, linetype=ID))+
      # geom_point(aes(color=Condition, fill=ID))+
      # geom_point(aes(group=ID), colour = "grey70")+
      # geom_line(aes(group=ID, color=Condition))+
      stat_summary(aes(color=Condition, group=Condition),
                   fun = mean,
                   fun.min = function(x) mean(x) - se(x),
                   fun.max = function(x) mean(x) + se(x),
                   geom = "errorbar", width=1, position=position_dodge(1)) +
      stat_summary(aes(color=Condition, group=Condition),
                   fun = mean,
                   geom = "point", position=position_dodge(1), size=3) +
      stat_summary(aes(color=Condition, group=Condition), fun=mean, geom="line") +
      labs(title = paste(BG, "at", BG_dB, "dB"),
           x="Intensity (dB)",
           y="Reaction time (ms, mean +/- SE)") +
      facet_wrap(~ `Freq (kHz)`, scale="free") +
      theme_classic() + 
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_line(colour="grey80")
        # panel.grid.minor.x = element_line(colour="grey80"))
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
    ggplot(aes(x=`Inten (dB)`, y=mean)) +
      # geom_line(aes(color=BG_Intensity, linetype=ID))+
      # geom_point(aes(color=BG_Intensity, fill=ID))+
      # geom_point(aes(group=ID), colour = "grey70")+
      # geom_line(aes(group=ID, color=BG_Intensity))+
      stat_summary(aes(color=BG_Intensity, group=BG_Intensity),
                   fun = mean,
                   fun.min = function(x) mean(x) - se(x),
                   fun.max = function(x) mean(x) + se(x),
                   geom = "errorbar", width=1, position=position_dodge(1)) +
      stat_summary(aes(color=BG_Intensity, group=BG_Intensity),
                   fun = mean,
                   geom = "point", position=position_dodge(1), size=3) +
      stat_summary(aes(color=BG_Intensity, group=BG_Intensity), fun=mean, geom="line", ) +
      labs(title = "32kHz",
           x="Intensity (dB)",
           y="Reaction time (ms, mean +/- SE)") +
      facet_wrap(~ `Condition`, scale="free") +
      theme_classic() + 
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_line(colour="grey80")
        # panel.grid.minor.x = element_line(colour="grey80"))
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
    ggplot(aes(x=`Inten (dB)`, y=mean)) +
      geom_line(aes(color=ID, linetype=Condition))+
      geom_point(aes(color=ID, shape=Condition))+
      labs(title = paste(BG, "at", BG_dB, "dB"),
           x="Intensity (dB)",
           y="Reaction time (ms)") +
      facet_wrap(~ `Freq (kHz)`, scale="free") +
      theme_classic() + 
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_line(colour="grey80")
        # panel.grid.minor.x = element_line(colour="grey80"))
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
    ggplot(aes(x=`Inten (dB)`, y=mean)) +
      geom_hline(yintercept=0, linetype="dashed", size=1.2)+
      stat_summary(aes(color=Sex, linetype=BG_Intensity, group=interaction(Sex, BG_Intensity)),
                   fun = mean,
                   fun.min = function(x) mean(x) - se(x),
                   fun.max = function(x) mean(x) + se(x),
                   geom = "errorbar", width=1, position=position_dodge(1)) +
      stat_summary(aes(color=Sex, shape=BG_Intensity, group=interaction(Sex, BG_Intensity)),
                   fun = mean,
                   geom = "point", position=position_dodge(1), size=3) +
      stat_summary(aes(color=Sex, linetype=BG_Intensity, group=interaction(Sex, BG_Intensity)), 
                   fun=mean, geom="line") +
      labs(title = "Change in Reaction time",
           x="Intensity (dB)",
           y="Reaction time (ms, mean +/- SE)") +
      facet_wrap(~ `Freq (kHz)`, scale="free") +
      theme_classic() + 
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_line(colour="grey80")
        # panel.grid.minor.x = element_line(colour="grey80"))
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
    lm = if (df$`Stim Source` == "BBN") lm(`R Time (ms)` ~ Condition*`Inten (dB)`, data=df)
    else lm(`R Time (ms)` ~ Condition*`Inten (dB)`*`Freq (kHz)`, data=df)
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
  
  # Data %>%
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
  #   Data %>%
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
  