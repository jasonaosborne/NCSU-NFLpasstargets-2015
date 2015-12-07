library(XML)

game_index <- function(year){
  # Get the page's source
  url <- paste0("http://www.pro-football-reference.com/years/",year,"/games.htm")
  web_page <- readLines(url)
  
  # Pull out the appropriate line
  mypattern = '<td align=\"center\" ><a href=\"/boxscores/([^<]*).htm\">boxscore</a></td>'
  playindex <- web_page[grep(mypattern, web_page)]
  
  # Delete unwanted characters in the lines we pulled out
  playindex <- gsub("<td align=\"center\" ><a href=\"/boxscores/", "", playindex,
                    fixed = TRUE)
  playindex <- gsub(".htm\">boxscore</a></td>", "", playindex, fixed = TRUE)
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  playindex <- trim(playindex)
  return(playindex)
}

for (year in startyear:endyear) {
  # get all play indexes for current year
  gamelist <- game_index(year)
  
  # initialize a list to hold the results
  targets <- list()
  
  # for each game, find the targets and receptions data
  for(i in 1:length(gamelist)){
    game <- gamelist[i]
    
    #load home team roster
    setwd("C:/Users/Graham/Desktop/SportsAnalytics/rosters")
    home <- toupper(substr(game, nchar(game)-2, nchar(game)))
    home <- ifelse (home %in% c("HOU", "IND") && year %in% c("1994"), 
                    if (home == "HOU") {
                      home <- "OTI"
                    } else {
                      home <- "CLT"
                    }, home)
    
    #file <- paste0(toupper(substr(game, nchar(game)-2, nchar(game))), "_", 
                   #year, "_roster.RData")
    file <- paste0(home, "_", year, "_roster.Rdata")
    
    load(file)
    roster <- playerMat
    rm(playerMat, file)
    
    # get url for the 1st item in playlist
    url <- paste0("http://www.pro-football-reference.com/boxscores/", game,
                  ".htm")
    
    tornados <- htmlParse(url)
    tables <- readHTMLTable(tornados,stringsAsFactors = FALSE)
    n <- length(tables)
    
    flag <- rep(NA, n)
    for(j in 1:n) {
      names <- colnames(tables[[j]])
      flag[j] <- prod(c("Tm", "Tgt") %in% names)
    }
    
    #create table with players only from the pass/rush/rec table
    df <- tables[[which(flag==1)]]
    finaltab <- df[which(df[,1]!=""),c(1,2,13,14,15)]
    
    # Check to make sure there are no data errors
    if (all(finaltab[, 3]=="")) {
      print(paste("Game ID:", game, "has been dropped from the analysis."))
      
      # clean up for the next iteration
      rm(game, roster, url, tornados, tables, n, flag, names, j, df, finaltab)
      
      next
    }
    
    #remove QBs
    finaltab <- finaltab[which(finaltab[,3]!=""),]
    colnames(finaltab)[1] <- "Name"
    
    #load away team roster
    away <- finaltab[1,2]
    away <- ifelse (away %in% c("HOU", "IND") && year %in% c("1994"), 
                    if (away == "HOU") {
                      away <- "OTI"
                    } else {
                      away <- "CLT"
                    }, away)
    
    file <- paste0(away, "_", year, "_roster.Rdata")
    
    load(file)
    roster <- rbind(playerMat, roster)
    colnames(roster) <- c("Pos", "Name")
    rm(playerMat, file)
    
    finaltab.pos <- merge(x=finaltab, y=roster, by="Name", all.x=T)
    
    finaltab.forreal <- cbind(year, game, finaltab.pos[, c(1, 2, 6, 3:5)])
    colnames(finaltab.forreal)[1:2] <- c("Year", "GameID")
    
    # save the table as output
    targets[[i]] <- finaltab.forreal
    
    # clean up for the next iteration
    rm(game, roster, url, tornados, tables, n, flag, names, j, df, finaltab,
       finaltab.pos, finaltab.forreal)
  }
  
  targets.allgames <- Reduce("rbind", targets)
  
  # Save the data for the year
  file <- paste0("C:/Users/Graham/Desktop/SportsAnalytics/Targets_", year,
                 ".RData")
  save(targets.allgames, file=file)
}