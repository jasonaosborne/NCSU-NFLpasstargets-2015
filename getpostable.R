setwd("~/NC State/Sports Analytics/NCSU-NFLpasstargets-2015/Rosters")

for (year in 1995:2014) {
  # assign list of team names based on year
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
  # creates an empty list
  rosterlist <- list()
  for (i in 1:length(teamid)) {
    # build file name and load data
    load(paste0(paste(toupper(teamid[i]), year, "roster", sep="_"), ".Rdata"))
    # naming the columns of the matrix that we're pulling in
    colnames(playerMat) <- c("pos", "name")
    #remove players with missing position
    if(any(is.na(playerMat[, 1]))) {
      playerMat <- playerMat[-which(is.na(playerMat[,1])),]
    }
    rosterlist[[i]] <- data.frame(year, team = toupper(teamid[i]), 
                                     unique(playerMat))
  }
  
  playerMat_all <- Reduce("rbind", rosterlist)
  
  save(playerMat_all,
       file = paste0(paste("ALL", year, "roster", sep="_"), ".Rdata"))
  
}

# creates an empty list that we're filling with position names
posNames <- list()
totalRB <- rep(0,20)
grandTotal <- rep(0,20)

for (i in 1995:2014) {
  load(paste0(paste("ALL", i, "roster", sep="_"), ".Rdata"))
  posCount <- table(playerMat_all$pos, useNA = "always")
  posNames[[i]] <- names(posCount)
  
  # prints warning if there are NAs
  if (posCount[length(posCount)] != 0) {
    warning("NAs found")
  }
  
  want <- c("FB","RB","LB-RB","WR-RB","RB-TE","RB-WR","LB-RB-TE",
           "DB-RB","RB-TE-LB","FB-LB","TE-FB","FB-TE","RB/TE","LB/RB")
  keepPos <- posCount[which(names(posCount) %in% want)]
  totalRB[i-1994] <- sum(keepPos)
  grandTotal[i-1994] <- sum(posCount)
}

# puts all the position names together and
# gets rid of duplicates position names
# fullPosList <- unique(Reduce("c", posNames))

# just graph the proportions - look at Matt's chart in other code
# year on x axis, proportion on y axis

