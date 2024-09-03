
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
  
  if(!dir.exists("rawdata")){
    dir.create("rawdata")
    dir.create("./rawdata/batting")
    dir.create("./rawdata/pitching")
    dir.create("./rawdata/fielding")
  }
  
  for(league in allYearHTML){
    allYearHtml <- read_html(league)
    # 年度を指定する 1950 ~ 2023
    for(yearID in 2:((today() %>% year()) - 1949)){ # 2
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
          
          fileName <- paste0("./rawdata/batting/", Year, teamName, ".csv")
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
        
          fileName <- paste0("./rawdata/pitching/", Year, teamName, ".csv")
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
          fileName <- paste0("./rawdata/fielding/",Year,teamName,"_",pos,".csv")
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

reading_league <- function(){
  if(!dir.exists("./rawdata/league")){
    dir.create("./rawdata/league")
    dir.create("./rawdata/league/rank")
    dir.create("./rawdata/league/batting")
    dir.create("./rawdata/league/pitching")
    dir.create("./rawdata/league/fielding")
  }
  
  for(i in 1:2){
    allYearHtml <- read_html(allYearHTML[i])
    Leagues <- c("Pacific", "Central")
    League <- Leagues[i]
    # 年度を指定する 1950 ~ 2023
    for(yearID in 2:((today() %>% year()) - 1949)){ # 2
      Year <- (today() %>% year()) - yearID + 1
      paste("Year:", Year) %>% print
      paste("yearID: ", yearID) %>% print
      url <- paste0(
        "http://www.baseball-reference.com/",
        allYearHtml %>% 
          html_node(xpath = paste0(
            '//*[@id="lg_history"]/tbody/tr[',yearID,']/th/a'
            )) %>% 
          html_attr("href")
        )
      txt <- read_html(url) %>% 
        gsub("<!--", "", .) %>% gsub("-->", "", .)
      html_season <- read_html(txt)

# 順位表 ---------------------------------------------------------------------
      fileName <- paste0(Year, League, "Rank.csv")
      fileName %>% print()
      html_season %>% 
        html_node(xpath = '//*[@id="div_regular_season"]') %>% 
        html_table() %>% 
        set_names(c(
          "team", "wins", "loses", "ties", "WL", "GB"
        )) %>% 
        rowid_to_column(var = "rank") %>% 
        mutate(
          year = Year,
          league = League,
          GB = if_else(GB == "--", "0", GB) %>% as.numeric()
          ) %>% 
        select(year, league, everything()) %>% 
        write_csv(paste0("./rawdata/league/rank/", fileName))

# チーム打撃 -------------------------------------------------------------------
      data <- html_season %>% 
        html_node(xpath = '///*[@id="div_league_batting"]') %>% 
        html_table() %>% 
        select(-Aff) %>% 
        mutate(
          year = Year,
          league = League,
          single = H - `2B` - `3B` - HR) %>% 
        select(year, league, everything()) %>% 
        set_names(c(
          "year", "league", "team", "aveage", "RG","game", "PA", "AB", 
          "run", "hit", "double", "triple", "HR", "RBI", "steal", "CS", 
          "BB", "SO", "AVG", "OBP", "SLG", "OPS", "TB", "GDP", 
          "HBP", "SH", "SF", "IBB", "single"
        )) %>% 
        mutate_all(~replace(., is.na(.), -99999)) %>% 
        mutate_all(~replace(., is.infinite(.), -99999)) %>% 
        relocate(single, .before = double)
      fileName <- paste0(Year, League, "TeamBatting.csv")
      fileName %>% print()
      data %>% 
        filter(team != "League Totals") %>% 
        write_csv(paste0("./rawdata/league/batting/", fileName))
      fileName <- paste0(Year, League, "LeagueBatting.csv")
      fileName %>% print()
      data %>% 
        filter(team == "League Totals") %>% 
        select(-team) %>% 
        write_csv(paste0("./rawdata/league/batting/", fileName))

# チーム投球 -------------------------------------------------------------------
      data <- html_season %>% 
        html_node(xpath = '//*[@id="div_league_pitching"]') %>% 
        html_table() %>% 
        select(-Aff) %>% 
        mutate(
          year = Year,
          league = League,
          IP = (IP - as.integer(IP))*10 + as.integer(IP)*3
          ) %>% 
        select(year, league, everything()) %>% 
        set_names(c(
          "year", "league", "team", "aveage", "RG", "win", "lose", "WL", 
          "ERA","RA9", "game", "starter", "closer", "complete", "shutout", 
          "save", "outs", "hit", "run", "ER", "HR", "BB", "IBB", "K", 
          "HBP", "BK", "WP", "BF", "WHIP", "H9", "HR9", "BB9", "K9", "KBB" 
        )) %>% 
        mutate_all(~replace(., is.na(.), -99999)) %>% 
        mutate_all(~replace(., is.infinite(.), -99999))
      fileName <- paste0(Year, League, "TeamPitching.csv")
      fileName %>% print()
      data %>% 
        filter(team != "League Totals") %>% 
        write_csv(paste0("./rawdata/league/pitching/", fileName))
      fileName <- paste0(Year, League, "LeaguePitching.csv")
      fileName %>% print()
      data %>% 
        filter(team == "League Totals") %>% 
        select(-team) %>% 
        write_csv(paste0("./rawdata/league/pitching/", fileName))

# チーム守備 -------------------------------------------------------------------
      table <- html_season %>% 
        html_node(xpath = '//*[@id="div_league_fielding"]')
      if(is.na(table)) next
      data <- table %>% 
        html_table() %>% 
        set_names(c(
          "team", "Aff", "G", "CG", "PO",
          "A", "E", "DP", "Fld", "PB",
          "SB", "CS", "CS%" ,"lgCS%", "POsub",
          "Attendance", "Managers"
        )) %>% 
        select(-Aff, -POsub, -Attendance) %>% 
        mutate(
          year = Year,
          league = League, 
        ) %>% 
        separate(Managers, into = c("manager", "manager_sub"), sep = "/", fill = "right", extra = "merge") %>%
        mutate(
          manager_sub = ifelse(is.na(manager_sub), "---", manager_sub)
        ) %>% 
        select(year, league, team, manager, manager_sub, everything()) %>% 
        mutate_all(~replace(., is.na(.), -99999)) %>% 
        mutate_all(~replace(., is.infinite(.), -99999))
      fileName <- paste0(Year, League, "TeamFielding.csv")
      fileName %>% print()
      data %>% 
        filter(team != "League Totals") %>% 
        write_csv(paste0("./rawdata/league/fielding/", fileName))
      fileName <- paste0(Year, League, "LeagueFielding.csv")
      fileName %>% print()
      data %>% 
        filter(team == "League Totals") %>% 
        select(-team, -manager, -manager_sub) %>% 
        write_csv(paste0("./rawdata/league/fielding/", fileName))
      
      # Wait for 
      Sys.sleep(15) 
    }
  }
}

reading_league()
