
# Notes -------------------------------------------------------------------



# Threshold filtering -----------------------------------------------------

TH %>%
  filter(`Dur (ms)` != "100") %>%
  filter(Condition %in% c("Baseline")) %>%
  filter(!(ID %in% HL_not_done)) %>%
  filter(BG_Intensity %in% c("NA", "50")) %>%
  # write.table("clipboard", sep="\t", row.names=FALSE) %>%
  View