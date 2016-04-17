setwd("D:/RStudio/my work directory/new")
load("playbyplay_ordered_filled.RData")
#adjust the data structure#

#column 8:  1 is no time-out. 2 is timeout2. 3 is timeout1#
data_r<-data2
data2[,8]<-1
ind<-which(data_r[,9]=="Timeout"&data_r[,8]==data_r[,4])
data2[ind,8]<-3
ind<-which(data_r[,9]=="Timeout"&data_r[,8]==data_r[,5])
data2[ind,8]<-2
#column 9 is the whole description#
data2[,9]<-data_r[,10]
data2[,10]<-0

data2[,6]<-as.numeric(data2[,6])
data2[,7]<-as.numeric(data2[,7])
data2[,10]<-data2[,6]-data2[,7]#this is the difference of score#
data2[,11]<-0#records the efficiency of a time-out ,the bigger the better#
data2[,12]<-0#records the type of a time-out: 0 indicate no time-out,1 indicate type1,2 indicate type2 (type 2 is time-out near the end of match) #
#column13 is team.event#
#column14 is event#
data2<-cbind(data2,data_r[,c(8,9)])

#adjust the data structure#
time<-data2[,3]
time[is.na(time)]<-strptime("1900/01/01 0:0:0", '%Y/%m/%d %H:%M:%S')#na denote the negative time(extra time)#
hlagdata2<-rbind(data2[1,],data2)
tlagdata2<-rbind(data2,data2[1,])
indicate_wholegame<-c(0)#value 0 indicates the start of a new game#
indicate_wholegame<-hlagdata2[,1]==tlagdata2[,1]
indicate_wholegame<-indicate_wholegame[-length(indicate_wholegame)]
indicate_wholegame[1]<-0

markpoint<-strptime("1900/01/01 0:3:0", '%Y/%m/%d %H:%M:%S')
data2[which(data2[,8]!=1),12]<-(time[which(data2[,8]!=1)]<=markpoint)+1

#create a new dataset only records events with time-out and score-change#
indicate_scorechange<-c(0)#value 1 indicates a score change, compared with the previous event#
indicate_scorechange<-hlagdata2[,10]!=tlagdata2[,10]
indicate_scorechange<-indicate_scorechange[-length(indicate_scorechange)]
indicate_scorechange[1]<-0
data2_score_change<-data2[indicate_scorechange==1|data2[,8]!=1,]


lag<-6
hlagdata2<-rbind(data2_score_change[1:lag,],data2_score_change)
tlagdata2<-rbind(data2_score_change,data2_score_change[1:lag,])
ind1<-which(data2_score_change[,8]==3&data2_score_change[,12]==1)
ind2<-which(data2_score_change[,8]==2&data2_score_change[,12]==1)
forward_def<-hlagdata2[,10]-tlagdata2[,10]#h(t)-h(t+6)#
forward_def<-forward_def[-(1:lag)]
backward_def<-tlagdata2[,10]-hlagdata2[,10]#h(t)-h(t-6)#
backward_def<-backward_def[-((length(backward_def)-lag+1):length(backward_def))]
data2_score_change[ind1,11]<-(-backward_def[ind1])-forward_def[ind1]
data2_score_change[ind2,11]<-forward_def[ind2]+backward_def[ind2]
data2[indicate_scorechange==1|data2[,8]!=1,11]<-data2_score_change[,11]


func_home <- function(name1, name2){#team at home name1 is home, name2 is away
  data_3<-data2[data2[,4]==name1&data2[,5]==name2,]
  if (nrow(data_3) == 0){
    return(NULL)
  }
  if (data_3[1,1] == data_3[nrow(data_3), 1])
      {
    v11 <- data_3[data_3[,8] == 3&data_3[,12]==1,11]
    result <- c(mean(v11), data_3[nrow(data_3),10])
  }
  else
    {
    v11_1 <- data_3[data_3[,1] == data_3[1,1]&data_3[,8] == 3&data_3[,12]==1,11]
    v11_2 <- data_3[data_3[,1] == data_3[nrow(data_3),1]&data_3[,8] == 3&data_3[,12]==1,11]
    result1 <- c(mean(v11_1), data_3[sum(data_3[,1]==data_3[1,1]),10])
    result2 <- c(mean(v11_2), data_3[nrow(data_3), 10])
    result <- rbind(result1, result2)
    }
  return(result)
}

func_away <- function(name1, name2){#team at away
  data_3<-data2[data2[,4]==name1&data2[,5]==name2,]
  if (nrow(data_3) == 0){
    return(NULL)
  }
  if (data_3[1,1] == data_3[nrow(data_3), 1])
  {
    v11 <- data_3[data_3[,8] == 2&data_3[,12]==1,11]
    result <- c(mean(v11), -data_3[nrow(data_3),10])
  }
  else
  {
    v11_1 <- data_3[data_3[,1] == data_3[1,1]&data_3[,8] == 2&data_3[,12]==1,11]
    v11_2 <- data_3[data_3[,1] == data_3[nrow(data_3),1]&data_3[,8] == 2&data_3[,12]==1,11]
    result1 <- c(mean(v11_1), -data_3[sum(data_3[,1]==data_3[1,1]),10])
    result2 <- c(mean(v11_2), -data_3[nrow(data_3), 10])
    result <- rbind(result1, result2)
  }
  return(result)
}

df <- read.csv('leagues_NBA_2012_standings_expanded-standings.csv', header = T)
df <- df[-1,]
df <- df[,1:3]
df[,1] <- as.character(df[,1])
df[,1] <- as.numeric(df[,1])
temp <- read.table('newtext.txt', sep = '\t')
df <- df[order(df[,2]),]
df$abb <- temp[,1]
df <- df[order(df[,1]),]
df$abb <- as.character(df$abb)

result <- NULL
#for (i in 3:27){
for (i in c(3,7,11,15,19,23,27)){
  opponent_names <- df$abb[c(i-2,i-1,i+1,i+2)]
  my_name <- df$abb[i]
  result_team <- NULL
  for (j in 1:4){
    result_home <- func_home(my_name, opponent_names[j])
    result_away <- func_away(opponent_names[j], my_name)
    result_team <- rbind(result_team, result_home, result_away) 
  }
## result_team <- scale(result_team,scale=F)
  result <- rbind(result, result_team)
}


fit <- lm(result[,2]~result[,1])
plot(result[,1], result[,2],xlab="the efficiency of time-out",ylab="the final score difference")
abline(fit)

ind<-which(data2[,12]==1)
hist(data2[ind,11],xlab="efficiency of type_1 time out",ylab="frequency",main="the histogram of efficiency of type_1 timeout")






