library(tidyverse)


##batting####

# reading -----------------------------------------------------------------

# batting -----------------------------------------------------------------
setwd("./rawdata/batting")
csv_list <- list.files(pattern = "*.csv")
data <- do.call(rbind, lapply(csv_list, function(x) read_csv(x)))
data %>% 
  mutate(single = H - `2B` - `3B` - HR) %>% 
  set_names(c(
    "year", "team", "id", "name", "age", "game", "PA", "AB", "run", "hit", 
    "double", "triple", "HR", "RBI", "steal", "CS", "BB", "SO", "AVG", "OBP",
    "SLG", "OPS", "TB", "GDP", "HBP", "SH", "SF", "IBB", "single"
  )) %>% 
  mutate_all(~replace(., is.na(.), -99999)) %>% 
  mutate_all(~replace(., is.infinite(.), -99999)) %>% 
  relocate(single, .before = double) %>% 
  write_csv("../../data/PlayerBatting_byyear.csv")
setwd("../..")
