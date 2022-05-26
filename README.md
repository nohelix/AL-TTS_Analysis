# AL-TTS_Analysis
TTS analysis (mostly R code)

**Testing.RData** has the code run the import of the individual data on 5/25/22
    *Note* This file is missing one exported data file. This is left as is for script checking purposes.

**To DO**:
change d' calculation to curve fitting (polynomial aka dose response)
- Ben agrees this is better than the bracketing fit. We will still need to see if the 10dB step vs. 5dB step lead to different numbers.
-- Initial idea would be to use dcr package and fit same as 'standard curve fitting'. 
-- Initial testing suggests that this may cause issues with other packages, namely breaks (?) read_xls_sheets
-- Alternative dose response curve fitting packages exist including newest drda. (look into it??)
--- https://www.biorxiv.org/content/10.1101/2021.06.07.447323v1.full.pdf

Improve code commenting & organization
-- graphing is cluttering code - each should probably be its own section
-- are code subsections possible (I don't think so)

Make whole thing a function so that I can just slice the data at top then pass it to whole thing
-- This may need to remove all but the 'core' graphs
-- I will need to extensively document the substeps so that I can slice the correct dataframes at will
-- When running as a funtion, make optional "Full Data Import" (i.e. clear workspace and reload up to Raw_Data)
