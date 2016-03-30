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

year <- 1994

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
  
  # Pull in rosters to identify player designations
}