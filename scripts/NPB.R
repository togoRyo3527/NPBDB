
library(rvest)
library(xml2)
library(tidyverse)
library(romaji)

# NBP data ####

url <- "https://npb.jp/bis/players/all/index.html"
npbPlayerData <- function(url){
  npb <- read_html(url)
  
  kanaList <- list()
  wordList <- list()
  for(i in 1:10){
    pb <- txtProgressBar(min=1, max=nrow(10), style=3)
    if(i <= 7){
      for(j in 1:5){
        path <- npb %>% 
          html_node(
            xpath = paste('//*[@id="pl_lk_unit"]/ul[',i,']/li[',j,']/a', sep = "")
            )
        kana <- path %>% html_attr("href")
        word <- path %>% html_text()
        kanaList[[5*(i-1) + j]] <- kana
        wordList[[5*(i-1) + j]] <- word
      }
    } else if(i == 8){
      for(j in 1:3){
        path <- npb %>% 
          html_node(
            xpath = paste(
              '//*[@id="pl_lk_unit"]/ul[8]/li[',2*j-1,']/a', 
              sep = ""
              )
            )
        kana <- path %>% html_attr("href")
        word <- path %>% html_text()
        kanaList[[7*5 + j]] <- kana
        wordList[[7*5 + j]] <- word
      }
    } else if(i == 9){
      for(j in 1:5){
        path <- npb %>% 
          html_node(xpath = paste('//*[@id="pl_lk_unit"]/ul[9]/li[',j,']/a', sep = ""))
        kana <- path %>% html_attr("href")
        word <- path %>% html_text()
        kanaList[[5*7 + 3 + j]] <- kana
        wordList[[5*7 + 3 + j]] <- word
      }
    } else if(i == 10){
      path <- npb %>% 
      html_node(xpath = '//*[@id="pl_lk_unit"]/ul[10]/li/a')
      kana <- path %>% html_attr("href")
      word <- path %>% html_text()
      kanaList[[44]] <- kana
      wordList[[44]] <- word
    }
    setTxtProgressBar(pb, i)
  }
  #
  allKanaURL <- paste("https://npb.jp/bis/players/all/",kanaList, sep = "")
  result <- c()
  for(i in 40:44){
    wordList[i] %>% print
    kanalink <- read_html(allKanaURL[i])
    
    index <- kanalink %>% html_node(xpath = '//*[@id="pl_result_list"]/div[1]')
    player <- index %>% html_children() %>% html_attr("href")
    #カナごとの選手一覧URL
    allPlayerURL <- paste("https://npb.jp/",player, sep = "")
    table <- c()
    for(j in 1:length(allPlayerURL)){
      playerindex <- read_html(allPlayerURL[j])
      name1 <- playerindex %>% 
        html_node(xpath = '//li[@id="pc_v_name"]') %>% html_text
      name2 <- playerindex %>% 
        html_node(xpath = '//li[@id="pc_v_kana"]') %>% html_text
      table <- playerindex %>% 
        html_node(xpath = '//*[@id="pc_bio"]/table') %>% html_table %>% 
        as.data.frame() %>% column_to_rownames("X1")%>% t() %>% 
        bind_cols(nameJP = name1, kana = name2) %>% bind_rows(table)
      print(paste(j,"/",length(allPlayerURL)))
      Sys.sleep(1)
    }
    result <- rbind(result,table)
    wordList[i] %>% print
    Sys.sleep(2)
  }
}

###

#
thrhit <- function(vec){
  vec = case_when(
    vec == "右" ~ "R",
    vec == "左" ~ "L",
    vec == "両" ~ "B",
    TRUE ~ NA
  )
}
#

## wrangling ####
result111 <- result
result112 <- result111 %>% 
  mutate(
    nameJP = str_replace_all(nameJP, "　", " "),
    nameJP = str_replace_all(nameJP, "                                    ",""),
    kana = str_replace_all(kana, "・", " "),
    birthday = as.Date(生年月日, format = "%Y年%m月%d日"),
    height = str_split(`身長／体重`, "／", simplify = TRUE)[,1],
    weight = str_split(`身長／体重`, "／", simplify = TRUE)[,2],
    height = str_replace(height, "cm", ""),
    weight = str_replace(weight, "kg", ""),
    height = as.integer(na_if(height, "- ")),
    weight = as.integer(na_if(weight, "- ")),
    throwing = str_split(投打, "投", simplify = TRUE)[,1],
    hitting = str_split(投打, "投", simplify = TRUE)[,2],
    throwing = str_replace(throwing, "-右打","--"),
    hitting = str_replace_all(hitting, "打",""),
    throwing = thrhit(throwing),
    hitting = thrhit(hitting),
    draftYear = if_else(ドラフト != "", str_sub(ドラフト,1,4), NA),
    draft = if_else(ドラフト != "", str_sub(ドラフト,6,-1), NA),
    draft = str_replace(draft, "ドラフト", ""),
    career1 = str_split(経歴, " - ", simplify = TRUE)[,1],
    career2 = str_split(経歴, " - ", simplify = TRUE)[,2],
    career3 = str_split(経歴, " - ", simplify = TRUE)[,3],
    career4 = str_split(経歴, " - ", simplify = TRUE)[,4],
    career5 = str_split(経歴, " - ", simplify = TRUE)[,5],
    career6 = str_split(経歴, " - ", simplify = TRUE)[,6],
    career7 = str_split(経歴, " - ", simplify = TRUE)[,7],
    career8 = str_split(経歴, " - ", simplify = TRUE)[,8],
    career9 = str_split(経歴, " - ", simplify = TRUE)[,9],
    career10 = str_split(経歴, " - ", simplify = TRUE)[,10],
    career11 = str_split(経歴, " - ", simplify = TRUE)[,11],
    career1 = na_if(career1, ""),
    career2 = na_if(career2, ""),
    career3 = na_if(career3, ""),
    career4 = na_if(career4, ""),
    career5 = na_if(career5, ""),
    career6 = na_if(career6, ""),
    career7 = na_if(career7, ""),
    career8 = na_if(career8, ""),
    career9 = na_if(career9, ""),
    career10 = na_if(career10, ""),
    career11 = na_if(career11, ""),
    
    
    ) %>% 
  select(-投打,-`身長／体重`,-生年月日,-経歴,-ドラフト)


### 日本語 ####
##
library(reticulate)
py_install("pykakasi")
pykakasi <- import("pykakasi")
convert <- pykakasi$kakasi()$convert
##

result111 %>% 
  mutate(
    kana = str_replace_all(kana, "・", " ")
    ) %>% 
  select(kana)
  mutate(
    draftYear = if_else(ドラフト != "", str_sub(ドラフト,1,4), NA),
    draft = if_else(ドラフト != "", str_sub(ドラフト,6,-1), NA),
    draft = str_replace(draft, "ドラフト", "")
    
    ) %>% 
  select(ドラフト,draftYear,draft) %>% tail(20)

