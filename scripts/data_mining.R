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
  mutate(IP = (IP - as.integer(IP))*10 + as.integer(IP)*3) %>% 
  set_names(c(
    "year","team","playerID","name","age","win","lose","WL","ERA","game",
    "starter","closer","complete","shutout","save","outs","hit","run","ER","HR",
    "BB","IBB","K","HBP","BK","WP","BF","WHIP","H9","HR9","BB9","K9","KBB" 
  )) %>% 
  mutate_all(~replace(., is.na(.), -99999)) %>% 
  mutate_all(~replace(., is.infinite(.), -99999)) %>% 
  write_csv("../../data/PlayerPitchingByYear.csv")
setwd("../..")

# fielding あと回し ----------------------------------------------------------------
setwd("./rawdata/fielding/")
csv_list <- list.files(pattern = "*.csv")
data <- do.call(rbind, lapply(csv_list, function(x) read_csv(x)))
data %>% 
  mutate(IP = (IP - as.integer(IP))*10 + as.integer(IP)*3) %>% 
  set_names(c(
    "year","team","playerID","name","age","win","lose","WL","ERA","game",
    "starter","closer","complete","shutout","save","outs","hit","run","ER","HR",
    "BB","IBB","K","HBP","BK","WP","BF","WHIP","H9","HR9","BB9","K9","KBB" 
  )) %>% 
  mutate_all(~replace(., is.na(.), -99999)) %>% 
  mutate_all(~replace(., is.infinite(.), -99999)) %>% 
  write_csv("../../data/PlayerFieldingByYear.csv")
setwd("../..")


# league rank -------------------------------------------------------------
setwd("./rawdata/league/rank/")
csv_list <- list.files(pattern = "*Rank.csv")
readrank <- function(x){
  df <- read_csv(x)
  if(ncol(df) == 6) return(df)
  return(df %>% mutate(ties = 0) %>% arrange(ties, .after = loses))
}
data <- do.call(
  rbind, 
  lapply(
    csv_list, 
    function(x){
      df <- read_csv(x)
      if(ncol(df) == 8){
        df <- df %>% mutate(ties = 0) %>% relocate(ties, .after = loses)
      }
      return(df)
    }
))
data %>% write_csv("../../../data/LeagueRank.csv")
setwd("../../..")


# batting by team ---------------------------------------------------------
setwd("./rawdata/league/batting/")
csv_list <- list.files(pattern = "*TeamBatting.csv")
data <- do.call(rbind, lapply(csv_list, function(x) read_csv(x)))
data %>% write_csv("../../../data/TeamBattingByYear.csv")
setwd("../../..")

# batting by league -------------------------------------------------------
setwd("./rawdata/league/batting/")
csv_list <- list.files(pattern = "*LeagueBatting.csv")
data <- do.call(rbind, lapply(csv_list, function(x) read_csv(x)))
data %>% write_csv("../../../data/LeagueBattingByYear.csv")
setwd("../../..")

# pitching by team --------------------------------------------------------
setwd("./rawdata/league/pitching/")
csv_list <- list.files(pattern = "*TeamPitching.csv")
data <- do.call(rbind, lapply(csv_list, function(x) read_csv(x)))
data %>% write_csv("../../../data/TeamPitchingByYear.csv")
setwd("../../..")

# pitching by league -----------------------------------------------------
setwd("./rawdata/league/pitching/")
csv_list <- list.files(pattern = "*LeaguePitching.csv")
data <- do.call(rbind, lapply(csv_list, function(x) read_csv(x)))
data %>% write_csv("../../../data/LeaguePitchingByYear.csv")
setwd("../../..")

