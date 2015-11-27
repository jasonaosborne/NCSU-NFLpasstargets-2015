# this code is based on the exampledata dataset from Graham's handout on Thurs

### the basic idea is that we need a contingency table with RB targets
#   and non-RB targets for each year

# for example, if we only looked at 2015 and 2014, we'd want something like:
# year | RB Tgts | Other Tgts
# ---------------------------
# 2015 | 67      | 186
# 2014 | 52      | 209
# ...  
# ---------------------------
# (I just made up these numbers, but you get the idea)

# This will count how many years of data we have
# unique() will return each unique entry in the "Year" column
# length() returns how many items are in that vector
n.years <- length(unique(exampledata$Year))

# This puts the years in ascending order. I'm just doing this to make the table
# and plots look nicer later on
# rev() reverses the order of entries. so "Year" goes down from 2015,
# and this will just make it go up to 2015 instead
sorted.year <- rev(unique(exampledata$Year))

# here, I'm just creating basically a blank table that we'll fill in later with
# the correct values
# matrix(0,n,m) will make a matrix of n rows and m columns with all elements = 0
# I want the number of rows to be how many years we have
# and the number of columns to be 2: one for RB tgts, and the other for the rest
# you can call elements of a matrix A by A[n,m], where n = row and m = column
# if you leave one blank, it will call the entire row or column
# for example: A[2,] will return the second row; A[,1] is the first column
tgts <- matrix(0,n.years,2)
# naming the rows and columns. column 1 is RBs, column 2 is others
rownames(tgts) <- rev(unique(exampledata$Year)) #using each year as the rowname
colnames(tgts) <- c("RB Targets","Non-RB Targets")

# starting loop to fill in matrix
for(i in 1:n.years){
  # basically, I want to break this dataset up into two pieces: I need the total
  # number of RB tgts and total number of non-RB tgts, and I want to do this for
  # each year
  # so for a given year, I'm going to sum up the Tgt for all rows where Pos = RB
  # and for all where Pos is not equal (!=) to RB
  # "&" is the operator for AND
  
  tgtRB <- sum(exampledata[exampledata$Pos=="RB" & exampledata$Year==sorted.year[i],]$Tgt)
  
  # so this is saying: subset our original dataset -- exampledata[] -- by taking 
  # all the rows where "Pos" is "RB" AND where "Year" is i, where i is whichever
  # year we're currently looking at, i.e., 2015, 2014, etc.
  # then, we're calling the "Tgt" column -- $Tgt -- from our new, subsetted data
  # and summing all the values in that column
  
  tgtOther <- sum(exampledata[exampledata$Pos!="RB" & exampledata$Year==sorted.year[i],]$Tgt)
  
  # this does the same, except the only difference is that we're taking all the
  # rows of "exampledata" where "Pos" is anything other than "RB"
  
  # then for that year, I fill in the corresponding row of our table with each
  # of these values
  tgts[i,] <- c(tgtRB,tgtOther)
}
# if we wanted, we could break this down further by just subsetting the data
# however we want, like we could add in a condition for team

# now i can make a contingency table
# prop.table(x,1) gets the proportion for rows, so column 1 will be 
# (RBtgts)/(RBtgts+otherTgts) and column 2 is (otherTgts)/(RBtgts+otherTgts)
RBtgt.table <- prop.table(tgts,1)

# we're interested in how the proportion of RB tgts has changed over time,
# so this is just the first column of our contingency table
RBtgt.prop <- RBtgt.table[,1]

# plotting the data over time
plot(sorted.year,RBtgt.prop,type="l",main="Proportion of RB Targets by Year",
     xlab="Year",ylab="RB Target %")
