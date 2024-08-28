
library(tidyverse)
library(rvest)
library(xml2)

allYearHTML <- c(
  'http://www.baseball-reference.com/register/league.cgi?code=JPPL&class=Fgn', #パ
  'http://www.baseball-reference.com/register/league.cgi?code=JPCL&class=Fgn' #セ
  )

# functions ####
splitLeft <- function(str,separ){
  # str    target string     対象文字列
  # separ  separator         分割する文字列
  #splitLeft("aaa/rrr","/") -> "aaa"
  #
  strsplit(str,separ)[[1]][[1]]
}

splitRight <- function(str,separ){
  # str    target string     対象文字列
  # separ  separator         分割する文字列
  #splitRight("aaa/rrr","/") -> "rrr"
  pos<-regexpr(separ,str)[1]+nchar(separ)
  substr( str, pos, nchar( str ))
}


# read ####

reading_player <- function(){
  
  if(!dir.exists("data")){
    dir.create("data")
    dir.create("./data/batting")
    dir.create("./data/pitching")
    dir.create("./data/fielding")
  }
  
  for(league in allYearHTML){
    allYearHtml <- read_html(league)
    # 年度を指定する 1950 ~ 2023
    for(yearID in 46:((today() %>% year()) - 1949)){ # 2
      Year <- (today() %>% year()) - yearID + 1
      paste("Yrear:", Year) %>% print
      paste("yearID", yearID) %>% print
      
      linkTeamList <- allYearHtml %>% 
        html_nodes(xpath = paste0('//*[@id="lg_history"]/tbody/tr[', yearID, ']/td'))
      linkList <- linkTeamList %>% html_nodes("a") %>% html_attr("href")
      teamList <- linkTeamList %>% html_nodes("a") %>% html_text()
      teamList %>% print
      
      teamStatsURL <- paste0("http://www.baseball-reference.com", linkList)
      
      for(i in 1:length(teamList)){
        teamList[i] %>% print
        teamName <- teamList[i] %>% gsub(pattern = " ",  replacement = "")
        txt <- read_html(teamStatsURL[i]) %>% 
          gsub("<!--", "", .) %>% gsub("-->", "", .)
        html <- read_html(txt)
      
# 各チームの個人打撃成績 -------------------------------------------------------------
        tryCatch({
          batting_stats <- html %>% 
            html_node(xpath = '//*[@id="team_batting"]') %>% html_table() 
          playerID <- html %>%
            html_nodes(xpath = '//*[@id="team_batting"]/tbody/tr/td[1]') %>% 
            html_attr("data-append-csv") %>% 
            splitRight("id=")
          
          fileName <- paste0("./data/batting/", Year, teamName, ".csv")
          fileName %>% print
          batting_stats %>% 
            select(-Notes) %>% 
            filter(Rk != "") %>% 
            mutate(
              Team = teamList[i],
              Year = Year,
              PlayerID = playerID
            ) %>%  
            select(Year, Team, PlayerID, everything(), -Rk) %>% 
            write_csv(file = fileName)
          },
          error = function(e){
            message(paste("Error writing file:", fileName))
            message("Error message:", e)
          }
        )

# 各チームの個人投球成績 -------------------------------------------------------------
        tryCatch({
          pitching_stats <- html %>% 
            html_node(xpath = '//*[@id="team_pitching"]') %>% html_table()
          playerID <- html %>%
            html_nodes(xpath = '//*[@id="team_pitching"]/tbody/tr/td[1]') %>% 
            html_attr("data-append-csv") %>% 
            splitRight("id=")
        
          fileName <- paste0("./data/pitching/", Year, teamName, ".csv")
          fileName %>% print
          pitching_stats %>% 
            select(-Notes) %>% 
            filter(Rk != "") %>% 
            mutate(
              Team = teamList[i],
              Year = Year,
              PlayerID = playerID
            ) %>%  
            select(Year, Team, PlayerID, everything(), -Rk) %>% 
            write_csv(file = fileName)
          },
          error = function(e){
            message(paste("Error writing file:", fileName))
            message("Error message:", e)
          }
        )

# 各チームポジション別個人守備成績 --------------------------------------------------------
        PATH <- '//*[@id="team_fielding_'
        for(pos in list("1B", "2B", "3B", "SS", "OF", "C", "P")){
          fileName <- paste0("./data/fielding/",Year,teamName,"_",pos,".csv")
          fileName %>% print
          xpath <- paste0(PATH, pos, '"]')
          tryCatch({
            field <- html %>% html_node(xpath = xpath)
            
            # もし守備データが無い場合はループを抜ける
            if(is.na(field)){
              messege("守備データがありません")
              break
            }
            
            field <- field %>% html_table()
            playerID <- html %>% 
              html_nodes(xpath = paste0(xpath, '/tbody/tr/th[1]')) %>% 
              html_attr("data-append-csv") %>% 
              splitRight("id=")
            field %>% 
              select(-Notes) %>% 
              mutate(
                Team = teamList[i],
                Year = Year,
                PlayerID = playerID,
                position = pos
              ) %>%  
              select(Year, Team, position, PlayerID, everything()) %>% 
              write_csv(file = fileName)
            },
            error = function(e){
              message(paste("Error writing file:", fileName))
          }
          )
        }
        # Wait for 
        Sys.sleep(10) 
      }
    }
  }
}


reading_player()

