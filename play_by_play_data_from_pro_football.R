# play_index function gets all indexes for football plays in your selected year
play_index <- function(year = 2014){
  
  # Get the page's source
  url <- paste0("http://www.pro-football-reference.com/years/",year,"/games.htm")
  web_page <- readLines(url)
  
  # Pull out the appropriate line
  mypattern = '<td align=\"center\" ><a href=\"/boxscores/([^<]*).htm\">boxscore</a></td>'
  playindex <- web_page[grep(mypattern, web_page)]
  
  # Delete unwanted characters in the lines we pulled out
  playindex <- gsub("<td align=\"center\" ><a href=\"/boxscores/", "", playindex, fixed = TRUE)
  playindex <- gsub(".htm\">boxscore</a></td>", "", playindex, fixed = TRUE)
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  playindex <- trim(playindex)
  return(playindex)
  
}


library(XML)

# get all play indexes for 2015
playlist <- play_index(2015)

# get url for the 1st item in playlist
url <- paste0("http://www.pro-football-reference.com/boxscores/", playlist[5],".htm")

tornados <- htmlParse(url)
tornado.tables<- readHTMLTable(tornados,stringsAsFactors = FALSE)
n <- length(tornado.tables)

df <- tornado.tables[[n]]

# remove rows: value of quarter variables not in (1, 2, 3, 4) 
# save the last row "End of Regulation"
last_row <- df[nrow(df),]

# remove all header rows 
df[(df$Quarter == "Quarter" | df$Quarter == ""),]
df <- df[!(df$Quarter == "Quarter" | df$Quarter == ""),]
df <- rbind(df, last_row)
save(df, file ="df5.RData")


