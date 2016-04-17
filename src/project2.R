##### Hackathon 2 #####

setwd("D:/Document/HACKATHON")
data <- read.csv("playbyplay20120510040.csv",header=T,stringsAsFactors=F)
library(dplyr)
data <- arrange(data,GameID,LineNumber)

data.trim <- data[1:1000,]
team1 <- str_sub(data.trim[,1],9,11)
team2 <- str_sub(data.trim[,1],12,14)
Event.team <- str_sub(str_match(data.trim[,4],"\"\[.{3}"),2,-1)
Event.team[is.na(Event.team)] <- ""
all.events <- c("Score","Rebound","Missed","Foul","")
Event <- 

score <- str_sub(str_match(data.trim[,4],"\\[.*\\]"),6,-2)
entry <- str_replace_all(data.trim[,4],"[^\\d\\s\\w]"," ")


load("playbyplay_ordered_filled.RData")
data2$TimeRemaining <- as.character(data2$TimeRemaining)


# Various Visualization

## 1 when do they call timeout? Histogram.
data2.Timeout <- filter(data2,Event=='Timeout')
data2.Timeout$TimeRemaining <- strptime(data2.Timeout$TimeRemaining,"%Y-%m-%d %H:%M:%S")
hist(data2.Timeout$TimeRemaining,breaks='secs',main="Distribution of Timeouts, 2011-2012 Regular Season",xlab="Time Remaining",ylab="Count",freq=T)
abline(v=as.numeric(strptime('1900-01-01 00:36:00 EST',"%Y-%m-%d %H:%M:%S")))
text(as.numeric(strptime('1900-01-01 00:36:00 EST',"%Y-%m-%d %H:%M:%S")),40,labels="End of\n1st Quarter")
abline(v=as.numeric(strptime('1900-01-01 00:24:00 EST',"%Y-%m-%d %H:%M:%S")))
text(as.numeric(strptime('1900-01-01 00:24:00 EST',"%Y-%m-%d %H:%M:%S")),40,labels="End of\n2nd Quarter")
abline(v=as.numeric(strptime('1900-01-01 00:12:00 EST',"%Y-%m-%d %H:%M:%S")))
text(as.numeric(strptime('1900-01-01 00:12:00 EST',"%Y-%m-%d %H:%M:%S")),40,labels="End of\n3rd Quarter")


## 2 Home team timeout.
data2.Timeout.home <- data2[data2$Team1==data2$Event.team & data2$Event=='Timeout',]
data2.Timeout.home$TimeRemaining <- strptime(data2.Timeout.home$TimeRemaining,"%Y-%m-%d %H:%M:%S")
hist(data2.Timeout.home$TimeRemaining,breaks='secs',main="Distribution of Home Team Timeouts, 2011-2012 Regular Season",xlab="Time Remaining",ylab="Count",freq=T)
abline(v=as.numeric(strptime('1900-01-01 00:36:00 EST',"%Y-%m-%d %H:%M:%S")))
text(as.numeric(strptime('1900-01-01 00:36:00 EST',"%Y-%m-%d %H:%M:%S")),25,labels="End of\n1st Quarter")
abline(v=as.numeric(strptime('1900-01-01 00:24:00 EST',"%Y-%m-%d %H:%M:%S")))
text(as.numeric(strptime('1900-01-01 00:24:00 EST',"%Y-%m-%d %H:%M:%S")),25,labels="End of\n2nd Quarter")
abline(v=as.numeric(strptime('1900-01-01 00:12:00 EST',"%Y-%m-%d %H:%M:%S")))
text(as.numeric(strptime('1900-01-01 00:12:00 EST',"%Y-%m-%d %H:%M:%S")),25,labels="End of\n3rd Quarter")


## 3 Away team timeout.
data2.Timeout.away <- data2[data2$Team2==data2$Event.team & data2$Event=='Timeout',]
data2.Timeout.away$TimeRemaining <- strptime(data2.Timeout.away$TimeRemaining,"%Y-%m-%d %H:%M:%S")
hist(data2.Timeout.away$TimeRemaining,breaks='secs',main="Distribution of Away Team Timeouts, 2011-2012 Regular Season",xlab="Time Remaining",ylab="Count",freq=T)
abline(v=as.numeric(strptime('1900-01-01 00:36:00 EST',"%Y-%m-%d %H:%M:%S")))
text(as.numeric(strptime('1900-01-01 00:36:00 EST',"%Y-%m-%d %H:%M:%S")),20,labels="End of\n1st Quarter")
abline(v=as.numeric(strptime('1900-01-01 00:24:00 EST',"%Y-%m-%d %H:%M:%S")))
text(as.numeric(strptime('1900-01-01 00:24:00 EST',"%Y-%m-%d %H:%M:%S")),20,labels="End of\n2nd Quarter")
abline(v=as.numeric(strptime('1900-01-01 00:12:00 EST',"%Y-%m-%d %H:%M:%S")))
text(as.numeric(strptime('1900-01-01 00:12:00 EST',"%Y-%m-%d %H:%M:%S")),20,labels="End of\n3rd Quarter")


## 4 Timeout distribution of high score difference games
game.results <- data2[data2$Event == "End of 4th Quarter",] # 971 games in total
game.score.diff <- game.results[,6]-game.results[,7]
hist(game.score.diff,seq(-50,40,5))
summary(game.score.diff)
sum(abs(game.score.diff-mean(game.score.diff))>=20) # high score diff 20 (abs)
HSD.GameID <- game.results[abs(game.score.diff-mean(game.score.diff))>=20,]$GameID
HSD.games <- data2[data2$GameID %in% HSD.GameID,]
HSD.games.Timeout <- HSD.games[HSD.games$Event == 'Timeout',]

HSD.games.Timeout$TimeRemaining <- strptime(HSD.games.Timeout$TimeRemaining,"%Y-%m-%d %H:%M:%S")
hist(HSD.games.Timeout$TimeRemaining,breaks='secs',main="Distribution of High-Final-Score-difference Games Timeouts, 2011-2012 Regular Season",xlab="Time Remaining",ylab="Count",freq=T)
abline(v=as.numeric(strptime('1900-01-01 00:36:00 EST',"%Y-%m-%d %H:%M:%S")))
text(as.numeric(strptime('1900-01-01 00:36:00 EST',"%Y-%m-%d %H:%M:%S")),6,labels="End of\n1st Quarter")
abline(v=as.numeric(strptime('1900-01-01 00:24:00 EST',"%Y-%m-%d %H:%M:%S")))
text(as.numeric(strptime('1900-01-01 00:24:00 EST',"%Y-%m-%d %H:%M:%S")),6,labels="End of\n2nd Quarter")
abline(v=as.numeric(strptime('1900-01-01 00:12:00 EST',"%Y-%m-%d %H:%M:%S")))
text(as.numeric(strptime('1900-01-01 00:12:00 EST',"%Y-%m-%d %H:%M:%S")),6,labels="End of\n3rd Quarter")

## 5 Timeout distribution of low score difference games
game.results <- data2[data2$Event == "End of 4th Quarter",] # 971 games in total
game.score.diff <- game.results[,6]-game.results[,7]
hist(game.score.diff,seq(-50,40,5))
summary(game.score.diff)
sum(abs(game.score.diff-mean(game.score.diff))<=3) # low score diff 3 (abs)
LSD.GameID <- game.results[abs(game.score.diff-mean(game.score.diff))<=3,]$GameID
LSD.games <- data2[data2$GameID %in% LSD.GameID,]
LSD.games.Timeout <- LSD.games[LSD.games$Event == 'Timeout' & !is.na(LSD.games$TimeRemaining),]

LSD.games.Timeout$TimeRemaining <- strptime(LSD.games.Timeout$TimeRemaining,"%Y-%m-%d %H:%M:%S")
hist(LSD.games.Timeout$TimeRemaining,breaks='secs',main="Distribution of Low-Final-Score-difference Games Timeouts, 2011-2012 Regular Season",xlab="Time Remaining",ylab="Count",freq=T)
abline(v=as.numeric(strptime('1900-01-01 00:36:00 EST',"%Y-%m-%d %H:%M:%S")))
text(as.numeric(strptime('1900-01-01 00:36:00 EST',"%Y-%m-%d %H:%M:%S")),20,labels="End of\n1st Quarter")
abline(v=as.numeric(strptime('1900-01-01 00:24:00 EST',"%Y-%m-%d %H:%M:%S")))
text(as.numeric(strptime('1900-01-01 00:24:00 EST',"%Y-%m-%d %H:%M:%S")),20,labels="End of\n2nd Quarter")
abline(v=as.numeric(strptime('1900-01-01 00:12:00 EST',"%Y-%m-%d %H:%M:%S")))
text(as.numeric(strptime('1900-01-01 00:12:00 EST',"%Y-%m-%d %H:%M:%S")),20,labels="End of\n3rd Quarter")