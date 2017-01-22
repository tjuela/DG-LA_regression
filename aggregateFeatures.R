library(dplyr)
library(plyr) #ddply
#------ read data frame
db=read.csv('datasets/OutputTable.csv')
#------ sort submissions
db=db[order(db$UserID,db$ProblemID,db$SubmissionNumber),]
# dim(db)
# View(db)
#------- aggregate by UserID and ProblemID ---------
  length(unique(db$UserID))
  agg.features=ddply(db, .(UserID,ProblemID), summarise, 
          overalGradeDiff=Grade[length(Grade)]-Grade[1], 
          countOfSubmissions=length(SubmissionNumber),
          totalTime = sum(TimeSinceLast, na.rm = T),
          totalVideoTime= sum(DurationOfVideoActivity,na.rm=T),
          countOfVideoEvents = sum(NVideoEvents,na.rm = T),
          countOfForumEvents = sum(NForumEvents,na.rm = T),
          countOfThreadViews = sum(NumberOfThreadViews, na.rm=T),
          NoOfVidoesWatched = sum(NoOfVidoesWatched, na.rm = T))
  
#-------normalize on submissions---------
  agg.features["VideoPerSubmission"]<-NA
  agg.features$VideoPerSubmission<-(agg.features$countOfVideoEvents/agg.features$countOfSubmissions)
  agg.features["ForumPerSubmission"]<-NA
  agg.features$ForumPerSubmission<-(agg.features$countOfForumEvents/agg.features$countOfSubmissions)
  agg.features["VideoTimePerSubmission"]<-NA
  agg.features$VideoTimePerSubmission<-(agg.features$totalVideoTime/agg.features$countOfSubmissions)
  agg.features["ThreadViewPerSubmission"]<-NA
  agg.features$ThreadViewPerSubmission<-(agg.features$countOfThreadViews/agg.features$countOfSubmissions)
  agg.features["NoOfVidoesWatchedPerSubmission"]<-NA
  agg.features$NoOfVidoesWatchedPerSubmission <-(agg.features$NoOfVidoesWatched/
                                                       agg.features$countOfSubmissions)
  agg.features[is.na(agg.features)] <- 0 
  
#------ remove cases with only one attempt
  agg.features=filter(agg.features,countOfSubmissions>1); dim(agg.features)
  
  
#------ save feature file
  write.csv(agg.features, file='features.csv')
 
  
  
#SAME FOR TEST  
  #------ read data frame
  db=read.csv('datasets/OutputTable_test.csv')
  #------ sort submissions
  db=db[order(db$UserID,db$ProblemID,db$SubmissionNumber),]
  # dim(db)
  # View(db)
  #------- aggregate by UserID and ProblemID ---------
  length(unique(db$UserID))
  agg.features=ddply(db, .(UserID,ProblemID), summarise, 
         overalGradeDiff=Grade[length(Grade)]-Grade[1], 
         countOfSubmissions=length(SubmissionNumber),
         totalTime = sum(TimeSinceLast, na.rm = T),
         totalVideoTime= sum(DurationOfVideoActivity,na.rm=T),
         countOfVideoEvents = sum(NVideoEvents,na.rm = T),
         countOfForumEvents = sum(NForumEvents,na.rm = T),
         countOfThreadViews = sum(NumberOfThreadViews, na.rm=T),
         NoOfVidoesWatched = sum(NoOfVidoesWatched, na.rm = T))
  
  #-------normalize on submissions---------
  agg.features["VideoPerSubmission"]<-NA
  agg.features$VideoPerSubmission<-(agg.features$countOfVideoEvents/agg.features$countOfSubmissions)
  agg.features["ForumPerSubmission"]<-NA
  agg.features$ForumPerSubmission<-(agg.features$countOfForumEvents/agg.features$countOfSubmissions)
  agg.features["VideoTimePerSubmission"]<-NA
  agg.features$VideoTimePerSubmission<-(agg.features$totalVideoTime/agg.features$countOfSubmissions)
  agg.features["ThreadViewPerSubmission"]<-NA
  agg.features$ThreadViewPerSubmission<-(agg.features$countOfThreadViews/agg.features$countOfSubmissions)
  agg.features["NoOfVidoesWatchedPerSubmission"]<-NA
  agg.features$NoOfVidoesWatchedPerSubmission <-(agg.features$NoOfVidoesWatched/
                                                   agg.features$countOfSubmissions)
  
  agg.features[is.na(agg.features)] <- 0 
  
  #------ remove cases with only one attempt
  agg.features=filter(agg.features,countOfSubmissions>1); dim(agg.features)
  
  #------ save feature file
  write.csv(agg.features, file='features_test.csv')