library(XML)
targets.list  <- list()

# this code downloads the first table that contains "Tgt" in the header
# this table can be merged with roster information

year.url <- "http://www.pro-football-reference.com/years/2014/games.htm"
year.html <- readLines(year.url)
boxscore.matches <- regexpr("boxscores/\\d",year.html)
year.boxscorelines <- which(boxscore.matches > 0)
#for(j in 1:5){
for(j in 1:length(year.boxscorelines)){
i <- year.boxscorelines[j]
gameid <- substr(year.html[i],boxscore.matches[i]+10,boxscore.matches[i]+21)
cat(gameid,"\n")
game.url <- paste("http://www.pro-football-reference.com/boxscores/",gameid,".htm",sep="")
tables.df <- readHTMLTable(game.url)
ntables <- length(tables.df)
for(k in 1:ntables){
if("Tgt" %in% names(tables.df[[k]])){targetk <- k; break}
}
targets.df <- tables.df[[targetk]]
targets.df <- cbind(gameid,targets.df)
targets.list <- rbind(targets.list,targets.df)
}
save.image("targets.RData")
