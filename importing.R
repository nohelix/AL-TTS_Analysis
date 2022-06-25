# Xlsx multisheet import --------------------------------------------------
# Read All Excel Sheets
# Modified from https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames
read_excel_allsheets <- function(filename, range, sheetlist, tibble = FALSE) {
  # but if you like tidyverse tibbles (the default with read_excel) then just pass tibble = TRUE
  #sheets <- readxl::excel_sheets(filename)
  #sheetname <- lapply(sheets, function(X) read_excel(filename, sheet = X, range = "A1:A1", col_names = FALSE) %>% toString %>% print)

  #iterate through each sheet (lapply) and create an individual dataframe
  x <- lapply(sheetlist, function(X) readxl::read_excel(filename, sheet = X, range = range) %>%
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
                mutate(ID = X,
                       Genotype = read_excel(filename, sheet = X, range = "A2:A2", col_names = FALSE) %>% toString(),
                      # Regex: This uses the summary sheet.It will need to be different for the actual file names.
                      # Group 1 - select Frequency which is the beginning of the file to the first _ (eg. BBN, 4-32kHz).
                      # Group 2 - select Intensity which from the 1st _ to dB. This should catch both ranges (eg 30-90dB), single dB (eg 60dB), and words (eg MIX, MIX5step)
                      # Group 3 - select Duration which is the 1st number(s) preceding the ms (eg. 50ms, 50-300ms).
                      # Optional second time should only be 1ms and represents a shorter response window.
                       Frequency = gsub("(^.*?)_(.*?dB)_(.*?ms)_.*$","\\1", File), # extracts frequency from name of file
                       Intensity = gsub("(^.*?)_(.*?dB)_(.*?ms)_.*$","\\2", File), # extracts intensity from name of file
                       Duration = gsub("(^.*?)_(.*?dB)_(.*?ms)_.*$","\\3", File), # extracts duration from name of file
                       BG_Type = ifelse(grepl("_BG_", File),
                                      gsub("(^.*?_BG_)(.*?)_(\\d+).*", "\\2", File),
                                      "NA"), # extracts the background type (PNK, BBN, WN, etc) from name of file; useful for filtering
                       BG_Intensity = ifelse(grepl("_BG_", File),
                                           gsub("(^.*?_BG_)(.*?)_(\\d+).*", "\\3", File),
                                           "NA"))) # extracts the background intensity (30, 50, etc) from name of file

  # Converts from the default tibble structure (flexible, easy to read, less predictable) to hard data frame
  if (!tibble) x <- lapply(x, as.data.frame)

  # name the elements in the list with the sheet names
  names(x) <- gsub("\\s", "", sheetlist)

  # returns list
  x
}


# Import Data -------------------------------------------------------------
# Note that the excel spreadsheet range is hardcoded to 9,999 lines, so if the
# data exceds that, it will not be loaded.

  # Load TTS spreadsheet (hard coded)
  TTS_RatID_list <- excel_sheets("Noise_TTS_Gp1_Green-Orange.xlsx") %>% .[1:15] %>% as.list() %>% .[. != "Orange 11" & . != "Orange 12" & . != "Green 11" & . != "Green 12"]

  # The message creation for the log is slow & spammy so I have suppressed the 'New Name' message
  suppressMessages(
  TTS_Data_Raw <- read_excel_allsheets("Noise_TTS_Gp1_Green-Orange.xlsx",
                                       range = range, sheetlist = TTS_RatID_list, tibble = TRUE)
  )

# Data Processing ---------------------------------------------------------

  TTS_Data <-
    TTS_Data_Raw %>%
  # Concat tables
      bind_rows() %>%
  # Filter unclassified days
      filter(!(is.na(Phase))) %>%
  # Filter out FA correction, Maintenance, & Errors
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
           "BBN_TH" = if_else(Frequency == "BBN", # If only 1 number (i.e. only digits from start to finish), its a BBN threshold
                              gsub("(^.\\d+)$", "\\1", Thresholds), NA_character_) %>% as.numeric() %>% round(),
           # Regex splits up the 4 listed limited thresholds calculated each day into separate columns by looking for the conserved spacer of ", "
           "4_TH" = if_else(Frequency == "4-32kHz",
                            gsub("^(.*?), (.*?), (.*?), (.+?)$", "\\1", Thresholds), NA_character_) %>% as.numeric() %>% round(),
           "8_TH" = if_else(Frequency == "4-32kHz",
                            gsub("^(.*?), (.*?), (.*?), (.+?)$", "\\2", Thresholds), NA_character_) %>% as.numeric() %>% round(),
           "16_TH" = if_else(Frequency == "4-32kHz",
                            gsub("^(.*?), (.*?), (.*?), (.+?)$", "\\3", Thresholds), NA_character_) %>% as.numeric() %>% round(),
           "32_TH" = if_else(Frequency == "4-32kHz",
                            gsub("^(.*?), (.*?), (.*?), (.+?)$", "\\4", Thresholds), NA_character_) %>% as.numeric() %>% round(),
    )



# Get & verify raw files list ---------------------------------------------
# create a list of all relevant files (daily runs) that need to be imported
# and checks filenames on filesystem vs summary xls log's expected filenames
# popup for outright missing files (empty popup = no problems)
# warnings of mismatches export to xls

# This needs to walk a directory, find the .csv file that starts with the same
# name START as the 'file' column.

  # Get folder and file names
  File_list <-
    TTS_Data %>%
      # Select date and file columns
      select(Date, File, ID, Sex, Condition, BG_Type, BG_Intensity) %>%
      # filter(Date > "2022-01-01") %>%
      # Change date to same format as folder names
      mutate(Folder = as.character.Date(Date) %>% gsub("-", "", .)) #%>% print

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
      DateCheck = if_else(Date == Date2, "Good", "BAD", missing = NA_character_),
      IDCheck = if_else(ID == ID2, "Good", "BAD", missing = NA_character_),
      File_Match = case_when(IDCheck != DateCheck ~ "ISSUE",
                             IDCheck == DateCheck & IDCheck == "Good" ~ "Good",
                             (IDCheck == DateCheck & is.na(DateCheck)) ~ NA_character_,)
    ) %>%
    relocate(ActualFile, .after = File)

  # Missing raw files, all missing files do not open for export in our current Behavior_Analysis script
  File_list_verify %>%
    filter(is.na(FileName_complete)) %>%
  # .mat files from 9-22-2021 and earlier are in the old format and can not be exported currently
    filter(Date > "2021-9-22") %>%
    arrange(Date) %>%
    View

  # ID files with unexpected names
  setwd(ProjectFolder)
  File_list_verify %>%
    filter(!(is.na(FileName_complete) & is.na(FileName_expected))) %>%
    write_xlsx(., "File_Check.xlsx")
  setwd(MainFolder)

# Import Reaction time ----------------------------------------------------
# Imports data from the individual csvs using the listing from previous step
# data goes into one massive dataframe, Raw_Data
# Sanity-checks Date and Rat ID, and indicates Good/ISSUE/na in File_Match column
# Sanity-checks listed trial count vs actual rows, popup table (empty=no problems)

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
        DateCheck = if_else(Date == Date2, "Good", "BAD", missing = "NA"),
        IDCheck = if_else(ID == ID2, "Good", "BAD", missing = "NA"),
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
# Makes three versions of master data frame:
#   All Trials / Drop 1st Block / Matched Blocks (take max(x) blocks from all rats)

# BBN & single frequency comes in blocks of 10 with either 5 or 10dB steps
# 4-32kHz comes in blocks of 40 with either 5 or 10dB steps

  # Drops the 1st block of every recording which is of different size based on they type of stimulus
  slicer <- function(df) {
    Stim = df$`Stim Source` %>% unique
    multiple = case_when(Stim == "tone" ~ 40, Stim == "BBN" ~ 10)
    total = count(df)
    start = (multiple + 1)
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

  Data_all <-
    Raw_Data %>%
    mutate(Stim = .$`Stim Source`)

  # Data_no1st has 1st block data removed, i.e. fewer rows
  # keeping Blocks_trimmed count (column) -- shouldn't interfere, but makes this dataframe distinct
  Data_no1st <-
    Data_all %>%
    group_by(Date, ID, Sex, Stim) %>%
    nest() %>%
    mutate(data = map(data, slicer),
           Blocks_trimmed = map_dbl(data, block_count))

  # Calculate the maximum number of blocks to keep every day
  BBN_maxBlock <-
    Data_no1st %>%
    filter(Stim == "BBN") %>%
    .$Blocks_trimmed %>%
    min()

  # Set a reasonable minimum but does not discard days that are below the minimum
  # under the assumption that exhaustion/lack of motivation isn't an issue
  tone_maxBlock <-
    Data_no1st %>%
    filter(Stim == "tone") %>%
    # filter(Blocks_trimmed >= 6) %>%
    .$Blocks_trimmed %>%
    min() %>%
    if_else(. >= min_blocks, ., min_blocks)


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

# Add Matched Blocks to Data_trimmed
  Data_trimmedBlocks <-
    Data_no1st %>%
    filter(Blocks_trimmed >= tone_maxBlock) %>%
    mutate(data = map(data, trimmer)) %>%
    unnest(data)

  Data_no1st <-
    Data_no1st %>%
    unnest(data)

