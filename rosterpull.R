teamid <- c("nwe", "buf", "nyj", "mia", "nyg", "phi", "was", "dal", "cin", "pit",
            "rav", "cle", "min", "gnb", "chi", "det", "clt", "htx", "jax", "oti",
            "car", "atl", "tam", "nor", "den", "rai", "kan", "sdg", "crd", "ram",
            "sea", "sfo")

setwd("C:/Users/Graham/Desktop/SportsAnalytics/rosters")

for(year in 1994:2015) {
  start <- proc.time()[3]  # Save starting time
  for(teamnum in 1:length(teamid)){
    team <- teamid[teamnum]
    
    #Pull up Roster for Given Team & Year
    url <- paste0("http://www.pro-football-reference.com/teams/", team, "/", 
                  year, "_roster.htm")
    
    # Read source page for roster
    src <- readLines(url)
    
    # Find Player Record in Source
    find1 <- grep('href="/players/', src)  # Finds records that have a playerid
    find2 <- grep('csk=', src) # Finds records that also have thier name
    find <- intersect(find1, find2) # Find records that have both patterns
    playerList <- src[find] # Subset source code
    
    n.players <- length(playerList)
    playerMat <- matrix(NA, nrow=n.players, ncol=2)
    for (i in 1:n.players) {
      # Strip the player page URL from the original roster HTML
      url.p <- strsplit(strsplit(playerList[i], 'href=\"')[[1]][2], '">')[[1]][1]
      url.p <- paste("http://www.pro-football-reference.com", sep="", url.p)
      
      # Pull up player page and strip locations of their position and name
      playersrc <- readLines(url.p)
      find.pos <- grep("<p><strong>Position:</strong>", playersrc)
      find.name <- grep("<title>", playersrc)
      
      # Format and save player position if available
      if (length(find.pos)==0) {
        playerMat[i, 1] <- NA
      } else {
        temp <- strsplit(playersrc[find.pos], " ")[[1]]
        playerMat[i, 1] <- temp[length(temp)]
      }
      
      # Format and save player name
      playerMat[i, 2] <- strsplit(strsplit(playersrc[find.name], "<title>")[[1]][2],
                                  " NFL Football")[[1]][1]
    }
    
    table(playerMat[, 1], useNA="ifany")
    
    output <- paste0(toupper(team),"_", year, "_roster.RData")
    save(playerMat, file=output)
  }
  print(paste("1 year takes", round((proc.time()[3] - start)/60, 3), "minutes"))
}


# DEPRECATED CODE:
#Pull tables from the Roster Page & kicking out players we don't like
# tornados <- htmlParse(url)
# tornado.tables<- readHTMLTable(tornados,stringsAsFactors = FALSE)
# n <- length(tornado.tables)
# 
# roster <- tornado.tables[[n]]
# 
# roster <- roster[((as.numeric(roster$No.) > 19) & (as.numeric(roster$No.) < 50)), ]
# roster <- roster[!is.na(roster$No.), ]