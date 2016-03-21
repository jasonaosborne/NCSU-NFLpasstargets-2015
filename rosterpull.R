# setwd("C:/Users/Graham/Desktop/SportsAnalytics/rosters")
setwd("S:/Desktop/GitHub/NCSU-NFLpasstargets-2015/Rosters")

startyear <- 1994;  endyear <- 2014
  
  for(year in startyear:endyear) {
    
    if (year==1994) {
      teamid <- c("sfo", "dal", "mia", "gnb", "sdg", "det", "min", "nwe", "nor",
                  "den", "cle", "buf", "was", "kan", "atl", "pit", "phi", "phi",
                  "clt", "rai", "sea", "ram", "nyg", "cin", "chi", "nyj", "tam",
                  "crd", "oti")
    } else if (year==1995) {
      teamid <- c("sfo", "det", "dal", "min", "pit", "gnb", "mia", "chi", "den",
                  "sea", "atl", "kan", "buf", "cin", "oti", "rai", "clt", "was",
                  "sdg", "nor", "phi", "ram", "nwe", "nyg", "cle", "car", "jax",
                  "crd", "tam", "nyj")
    } else if (year==1996 || year==1997 || year==1998) {
      teamid <- c("gnb", "nwe", "sfo", "den", "cin", "rav", "car", "was", "phi",
                  "oti", "pit", "rai", "mia", "jax", "buf", "clt", "sea", "sdg",
                  "atl", "ram", "det", "crd", "min", "dal", "chi", "nyj", "nyg",
                  "nor", "tam", "kan")
    } else if (year==1999 || year==2000 || year==2001) {
      teamid <- c("ram", "was", "clt", "car", "min", "jax", "oti", "kan", "rai",
                  "gnb", "dal", "sea", "mia", "rav", "det", "buf", "pit", "den",
                  "nyj", "nwe", "nyg", "sfo", "atl", "cin", "phi", "chi", "tam",
                  "sdg", "nor", "crd", "cle")
    } else {
      teamid <- c("nwe", "buf", "nyj", "mia", "nyg", "phi", "was", "dal", "cin",
                  "pit", "rav", "cle", "min", "gnb", "chi", "det", "clt", "htx",
                  "jax", "oti", "car", "atl", "tam", "nor", "den", "rai", "kan",
                  "sdg", "crd", "ram", "sea", "sfo")
    }
    
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
      # make sure the players are unique for that year
      playerMat <- unique(playerMat)
#       if(any(names(table(playerMat[, 1], useNA="ifany"))=="NA")) {
#         warning(paste(upcase(team), year, ":Some player positions missing"))
#       }
      
      output <- paste0(toupper(team),"_", year, "_roster.RData")
      save(playerMat, file=output)
    }
    print(paste(year, "took", round((proc.time()[3] - start)/60, 3), "minutes"))
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