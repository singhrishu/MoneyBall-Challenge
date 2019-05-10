#Moneyball Project

#Requirement : Need to know basic rules of Baseball

"""
Goal : Three players have left the team and the coach is looking 
to recruit best available players from the league.
He has the dataset from 1985 to 2013 about the statistics of 
hits, no. of home runs, no. of 1's, no. of 2's, and no.of 3's etc. and their salaries.

Business Rules :
1) The total combined salary of the three players can not exceed 15 million dollars.
2) Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
3) Their mean OBP had to equal to or greater than the mean OBP of the lost players

Dataset : Batting.csv and Salaries.csv

"""
batting <- read.csv("Batting.csv")
salary <- read.csv("Salaries.csv")

head(batting)
head(salary)

str(batting)
str(salary)

head(batting$AB)
head(batting$X2B)

#Feature Engineering
# 1.Calculate Batting Avaerage 

batting$BA <- batting$H / batting$AB

#Checking last 5 entries of the BA column of the data frame
tail(batting$BA,5)

# 2.Calculate On Base Percentage
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)

# 3.Calculate X1B (Singles)
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

# 4.Calculate Slugging Average (SLG)
batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) ) / batting$AB

# Check strtcture of data frame
str(batting)

#Merging salary Data with Batting Data
sal <- read.csv("Salaries.csv")

#Check for unwanted columns and remove the data that occured long time back
summary(batting)

#Removing batting data before 1985
batting <- subset(batting,yearID >=1985)

#Checking the yearID column for the reassignment
summary(batting)

"""
merge the batting data with the salary data! Since we have players playing multiple years, 
we'll have repetitions of playerIDs for multiple years, meaning we want to merge on both players and years.
"""
combo <- merge(batting,sal,by=c('playerID','yearID'))

#Use summary to check the data
summary(combo)

"""
Analyzing lost players
The team has lost 3 players.The players lost were: first baseman 2000 
AL MVP Jason Giambi (giambja01) to the New York Yankees, 
outfielder Johnny Damon (damonjo01) to the Boston Red Sox and infielder Rainer Gustavo Ray Olmedo ('saenzol01').
Check the missing players statistics
"""

lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01') )
lost_players

#All the three players were lost in 2001, so analyze only 2001 dataset to pick players
lost_players <- subset(lost_players,yearID == 2001)

#Reduce the dataset. Remove unwanted data
lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]
head(lost_players)

#Find replacement players keeping in mind the business rules
install.packages("dplyr")
library(dplyr)
avail.players <- filter(combo,yearID==2001)

#Create a scatter plot to find more info
install.packages("ggplot2")
library(ggplot2)
ggplot(avail.players,aes(x=OBP,y=salary)) + geom_point()

#There is no point paying above 8 million. So, it becomes the salary cut-off point
#There are also players with OBP=0. Should get rid of them.
avail.players <- filter(avail.players,salary<8000000,OBP>0)

#The total AB of the lost players is 1469. This is about 1500, meaning I should probably cut off my avail.players at 1500/3= 500 AB
avail.players <- filter(avail.players,AB >=500)

#sort by OBP 
possible <- head (arrange(avail.players,desc(OBP)),10)

#Grab only required columns
possible <- possible[,c('playerID','OBP','AB','salary')]

#Show the list of replacement players.
possible

#Final list of players
possible[2:4,]

