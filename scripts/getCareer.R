library(tidyverse)

calcAVG <- function(ab, hit){
  avg = if_else(ab != 0, hit/ab, 0.000)
  return(avg)
}

calcOBP <- function(hit, ab, bb, hbp, sh){
  top <- hit + bb + hbp
  buttom <- ab + bb + hbp + sh
  obp <- if_else(buttom != 0, top/buttom, 0.000)
  return(obp)
}

calcSLG <- function(tb, ab){
  slg <- if_else(ab != 0, tb/ab, 0.000)
  return(slg)
}

calcOPS <- function(obp, slg){
  ops <- obp + slg
  return(ops)
}

read_csv("./data/PlayerBattingByYear.csv") %>% 
  mutate(across(everything(), ~ifelse(. == -99999, NA, .))) %>% 
  select(-year, -team, -age, -name) %>% 
  group_by(id) %>% 
  summarise(across(everything(), ~sum(.))) %>% 
  mutate(
    AVG = calcAVG(AB, hit),
    OBP = calcOBP(hit, AB, BB, HBP, SH),
    SLG = calcSLG(TB, AB),
    OPS = calcOPS(OBP, SLG)
  ) %>% 
  write_csv("./data/PlayerBattingCareer.csv")

