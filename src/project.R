##### Hackathon #####

setwd("D:/Document/HACKATHON")
data <- read.csv("playbyplay20120510040.csv",header=T)
data$GameID <- as.character(data$GameID)
data$GameID <- as.character(data$GameID)
data$TimeRemaining <- as.character(data$TimeRemaining)
data$Entry <- as.character(data$Entry)

library(dplyr)
library(stringr)

# sample line
line <- data[10,]

# format new data row
new.line <- function(line){
  TimeRemaining <- strptime(paste("19000101",line[,3]),"%Y%m%d %H:%M:%S")
  
  entry <- str_replace_all(line[,4],"[^\\d\\s\\w]"," ")
  entry <- str_split(entry," ")[[1]]
  
  team1 <- str_sub(line[,1],9,11)
  team2 <- str_sub(line[,1],12,14)
    
  Event.team <- str_sub(str_match(line[,4],"\\[.{3}")[1,1],2,-1)
  if(is.na(Event.team)) Event.team = ""
  
  score <- str_sub(str_match(line[,4],"\\[.*\\]")[1,1],6,-2)
  score <- as.numeric(str_split(score,"-")[[1]])
  if(is.na(score)[1]) score <- c(0,0)
  if(Event.team == team2) score[1:2] <- score[2:1]
  
  Event <- ""
  if("PTS" %in% entry){
    Event <- "Score"
  } else if("Missed" %in% entry){
    Event <- "Missed"
  } else if("Rebound" %in% entry){
    Event <- "Rebound"
  } else if("Foul" %in% entry){
    Event <- "Foul"
  } else if("replaced" %in% entry){
    Event <- "replaced"
  } else if("Turnover" %in% entry){
    Event <- "Turnover"
  } else if("Timeout" %in% entry){
    Event <- "Timeout"
  } else if("Violation" %in% entry){
    Event <- "Violation"
  } else if("Jump" %in% entry & "Ball" %in% entry){
    Event <- "Jump Ball"
  } else if("Double" %in% entry & "Technical" %in% entry){
    Event <- "Double Technical"
  } else if("Technical" %in% entry){
    Event <- "Technical"
  } else if("End" %in% entry & "Quarter" %in% entry){
    Event <- line[,4]
  }
  
  return(data.frame(GameID=line[,1],LineNumber=line[,2],TimeRemaining=TimeRemaining,Team1=team1,Team2=team2,score1=score[1],score2=score[2],Event.team=Event.team,Event=Event,Entry=line[,4],stringsAsFactors=F))
}

# construct new dataframe
n <- nrow(data)

library(doParallel)
registerDoParallel(6)
getDoParWorkers()
system.time({
  data1 <- foreach(i=1:n) %dopar%{
    library(stringr)
    new.line(data[i,])
  }
})
# save(data1,file="playbyplay.RData")
load(file="playbyplay.RData")

k=1000
data1.df1 <- do.call(rbind.data.frame,data1[1:k])
write.table(data1.df1,file="playbyplay2012.csv",sep=",",row.names=F)
for(i in 1:200){
  data1.df2 <- do.call(rbind.data.frame,data1[(i*k+1):((i+1)*k)])
  write.table(data1.df2,file="playbyplay2012.csv",sep=",",append=T,row.names = F,col.names = F)
}
for(i in 201:400){
  data1.df2 <- do.call(rbind.data.frame,data1[(i*k+1):((i+1)*k)])
  write.table(data1.df2,file="playbyplay2012.csv",sep=",",append=T,row.names = F,col.names = F)
}
for(i in 401:426){
  data1.df2 <- do.call(rbind.data.frame,data1[(i*k+1):((i+1)*k)])
  write.table(data1.df2,file="playbyplay2012.csv",sep=",",append=T,row.names = F,col.names = F)
}
data1.df2 <- do.call(rbind.data.frame,data1[427001:427893])
write.table(data1.df2,file="playbyplay2012.csv",sep=",",append=T,row.names = F,col.names = F)

###
data2 <- read.csv("playbyplay2012.csv",header=T,stringsAsFactors=F)
data2 <- arrange(data2,GameID,LineNumber)
data2$TimeRemaining <- strptime(data2$TimeRemaining,"%Y-%m-%d %H:%M:%S")
save(data2,file="playbyplay_ordered.RData")
load("playbyplay_ordered.RData")


last.score <- c(0,0)
last.game <- data2$GameID[1]

system.time({
  for(i in 1:nrow(data2)){
    if(i %% 10000 == 0) print(i/10000)
    line <- data2[i,]
    if(line[1] == last.game & line[6] == 0 & line[7] == 0){
      data2[i,c(6,7)] <- data.frame(last.score)
    } else if(line[1] != last.game){
      last.score <- c(0,0)
      last.game <- line[1]
    } else{
      last.score <- line[6:7]
    }
  }
})

save(data2,file="playbyplay_ordered_filled.RData")


str(data)
