library(dplyr)
library(plyr) #ddply
#------ read data frame
db=read.csv('OutputTable.csv')
#------ sort submissions
db=db[order(db$UserID,db$ProblemID,db$SubmissionNumber),]
dim(db)
View(db)
#------- aggregate by UserID and ProblemID ---------
  length(unique(db$UserID))
  agg.features=ddply(db, .(UserID,ProblemID), summarise, 
          overalGradeDiff=Grade[length(Grade)]-Grade[1], 
          countOfSubmissions=length(SubmissionNumber),
          totalTime = sum(TimeSinceLast, na.rm = T),
          countOfVideoEvents = sum(NVideoEvents,na.rm = T),
          countOfForumEvents = sum(NForumEvents,na.rm = T))
          # countOfVideoAndForumEvents= (sum(NVideoEvents,na.rm = T)+sum(NForumEvents,na.rm = T)))
  
  
  
#------ remove cases with only one attempt
  agg.features=filter(agg.features,countOfSubmissions>1); dim(agg.features)
#------ save feature file
  write.csv(agg.features, file='features.csv')
 