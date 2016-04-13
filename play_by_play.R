library(XML)

## REMOVE PUNCTUATION AT THE END OF PLAYER NAMES #####
# STILL NEED: POSS, LOC, TARGETPOSS 

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

year <- 2003

#initialize list to contain all of the results dataframes
everythingYouEverWanted <- list()

# Pull list of all games for the current year
gamelist <- game_index(year)

# Pull the play-by-play information for each game
for (i in 1:length(gamelist)) {
  # Put together URL for the game
  url <- paste0("http://www.pro-football-reference.com/boxscores/", gamelist[i],
                ".htm")
  
  # Pull all of the tables from the game page 
  tableSource <- htmlParse(url)
  tables <- readHTMLTable(tableSource, stringsAsFactors=F)
  
  # Pull play-by-play table and remove columns (Time, EPB, EPA, Win%) and
  # rows (Quarter breaks and subtitle lines)
  PBP.raw <- tables$pbp_data
  PBP <- PBP.raw[, -c(2, 9:11)]                                 # Remove cols
  whichRows <- which(PBP$Quarter=="" | PBP$Quarter=="Quarter")  # Find rows
  PBP <- PBP[-whichRows ,]                                      # Remove rows
  
  # Identify home and away teams
  homeTeam <- colnames(PBP)[which(colnames(PBP)=="Detail")+2]
  awayTeam <- colnames(PBP)[which(colnames(PBP)=="Detail")+1]
  # Sanity check
  if (homeTeam != toupper(substr(gamelist[i], nchar(gamelist[i])-2, 
                                 nchar(gamelist[i])))) {
    warning("Home Team designation is incorrect.")
  }
  
  #parse the play by play data
  #keep only rows that are 'passed' or 'sacked'
  keepRows <- c(grep('sack',PBP$Detail),grep('pass',PBP$Detail))
  rmRows <- grep('Two Point',PBP$Detail)
  if (length(rmRows)>0 & length(which(keepRows %in% rmRows))>0) {
    keepRows <- keepRows[-which(keepRows %in% rmRows)] 
  }
  plays <- PBP[keepRows[order(keepRows)],]
  rownames(plays) <- NULL
  
  #initialize the data frame to hold everything we cherish
  results <- data.frame(GameID = rep(gamelist[i], nrow(plays)),
                        Home = rep(homeTeam, nrow(plays)),
                        Away = rep(awayTeam, nrow(plays)),
                        Quarter = numeric(nrow(plays)),
                        Poss = character(nrow(plays)),
                        Down = numeric(nrow(plays)),
                        Distance = numeric(nrow(plays)),
                        Location = numeric(nrow(plays)),
                        QB = character(nrow(plays)),
                        Target = character(nrow(plays)),
                        TargetPos = character(nrow(plays)),
                        Yards = numeric(nrow(plays)),
                        Penalty = numeric(nrow(plays)),
                        PointDiff = numeric(nrow(plays)),
                        stringsAsFactors = F)
  
  rm(PBP.raw,PBP,tableSource,url,whichRows,keepRows,tables)
  
  for (j in 1:nrow(plays)) {
    detail <- plays$Detail[j]
    if (regexpr(' pass',detail) > 0) {
      #getting QB name
      results$QB[j] <- substr(detail,start = 1,stop = regexpr(' pass',detail)-1)
      
      #getting target information
      if (length(grep('incomplete',detail))>0 & length(grep('intended for',detail)) == 0) {
        results$Target[j] <- ''
      } else {
      targetPos <- ifelse(regexpr(' complete to ',detail) > 0,
                          regexpr(' complete to ',detail), 
                          regexpr(' intended for ',detail))
      
      temp <- strsplit(substr(detail,start = targetPos, stop = nchar(detail)), " ")
      temp <- paste(temp[[1]][4],temp[[1]][5])
      results$Target[j] <- gsub("\\.","",temp)
      rm(targetPos,temp) # cleanup
      }
      
      #getting yards
      temp <- strsplit(substr(detail,start=1,stop=(regexpr(' yards',detail)-1))," ")
      results$Yards[j] <- ifelse(length(temp[[1]])==0, 0, 
                                 as.numeric(temp[[1]][length(temp[[1]])]))
      rm(temp)  # Cleanup
      
      results$Penalty[j] <- ifelse(length(grep('Penalty',detail))>0 & 
                                     (length(grep('Declined',detail))==0),1,0)
      
    } else if (regexpr(' sack',detail) > 0) {
      results$QB[j] <- substr(detail,start = 1,stop = regexpr(' sack',detail)-1)
      #getting yards
      temp <- strsplit(substr(detail,start=1,stop=(regexpr(' yards',detail)-1))," ")
      results$Yards[j] <- ifelse(length(temp[[1]])==0, 0, 
                                 as.numeric(temp[[1]][length(temp[[1]])]))
      rm(temp)  # cleanup
    } else {
      warning('Play contains no pass/sack')
    }
    
    #get quarter, down, and distance
    results$Quarter[j] <- plays$Quarter[j]
    results$Down[j] <- plays$Down[j]
    results$Distance[j] <- plays$ToGo[j]
    
    #calculate point differential -- always HOME - AWAY
    results$PointDiff[j] <- as.numeric(plays[j, which(colnames(plays)==homeTeam)]) - 
      as.numeric(plays[j, which(colnames(plays)== awayTeam)])
    
    
    # Pull in rosters to identify player designations
    #load in home roster and rename
    load(paste0("./rosters/",homeTeam,"_",year,"_roster.RData"))
    homeRoster <- unique(playerMat)
    #subset on QBs
    homeQB <- subset(homeRoster,homeRoster[,1]=="QB")
    #might as well remove playerMat
    rm(playerMat)
    load(paste0("./rosters/",awayTeam,"_",year,"_roster.RData"))
    awayRoster <- unique(playerMat)
    rm(playerMat)
    
    #QB lookup to assign possession
    if (sum(grepl(results$QB[j],homeQB[,2]))==0) {
      results$Poss[j] <- awayTeam
    } else {
      results$Poss[j] <- homeTeam
    }
    
    #assign position to targetPos
    if (results$Target[j] == '') {
      results$TargetPos[j] <- ''
    } else if (results$Poss[j] == results$Home[j]) {
      results$TargetPos[j] <- homeRoster[grep(results$Target[j],homeRoster[,2])[1],1]
      #not sure if it should be the first listed position
    } else if (results$Poss[j] == results$Away[j]) {
      results$TargetPos[j] <- awayRoster[grep(results$Target[j],awayRoster[,2])[1],1]
    } else {
      warning('Something is wrong')
    }
    
    #we want field positon to be 0=home team's endzone, 100=away team's endzone
    curToken <- strsplit(plays$Location[j],' ')
    if (curToken[[1]][1] == results$Poss[j]) {
      results$Location[j] <- as.numeric(curToken[[1]][2])
    } else {
      results$Location[j] <- 100 - as.numeric(curToken[[1]][2])
    }
    #get rid of the token
    rm(curToken)
  }
  everythingYouEverWanted[[i]] <- results
}