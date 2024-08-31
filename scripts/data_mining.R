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
  write_csv("../../data/PlayerBattingByYear.csv")
setwd("../..")


# pitching ----------------------------------------------------------------
setwd("./rawdata/pitching/")
csv_list <- list.files(pattern = "*.csv")
data <- do.call(rbind, lapply(csv_list, function(x) read_csv(x)))
data %>% 
  mutate(IP = (IP - as.integer(IP))*10 + as.integer(IP)*3) %>% select(IP)
  set_names(c(
    "year","team","playerID","name","age","win","lose","WL","ERA","game",
    "starter","closer","complete","shutout","save","outs","hit","R","ER","HR",
    "BB","IBB","K","HBP","BK","WP","BF","WHIP","H9","HR9","BB9","SO9","KBB" 
  )) %>% 
  mutate_all(~replace(., is.na(.), -99999)) %>% 
  mutate_all(~replace(., is.infinite(.), -99999)) %>% 
  relocate(single, .before = double) %>% 
  write_csv("../../data/PlayerPitchingByYear.csv")
setwd("../..")

