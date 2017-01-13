library(dplyr)
library(plyr) #ddply
#======================================================================== 
#         step 1: train classifier
#======================================================================== 

  #------ read features extracted from train set, using your python script
  db=read.csv('features.csv', stringsAsFactors = F)
  
  #------ sort submissions
  db=db[order(db$UserID,db$ProblemID,db$SubmissionNumber),]
  
  #--- replace NA values with 0
  db[is.na(db)]=0
  
  #----- remove first submissions
  db= filter(db,SubmissionNumber>0)
  
  #---- remove cases when there is no video or forum activity between two submissions
  # db$NVideoAndForum= db$NVideoEvents+db$NForumEvents
  # db= filter(db,NVideoAndForum>0)  
  db= filter(db, countOfForumEvents + countOfVideoEvents>0)
  
  #----- make a catgorical vribale, indicating if grade improved
  db$improved = factor(ifelse(db$GradeDiff>0 ,'Yes', 'No' ))
  table(db$improved)
  
  # ----- (Optional) split your training data into train and test set. Use train set to build your classifier and try it on test data to check generalizability. 
  set.seed(1234)
  tr.index= sample(nrow(db), nrow(db)*0.9)
  db.train= db[tr.index,]
  db.test = db[-tr.index,]
  dim(db.train)
  dim(db.test)
  
  #----- train classifier to predict 'improved' status 
  #----- Try different methods, model parameters, feature sets and find the best classifier 
  #----- Use AUC as model evaluation metric
  # model <- lm(overalGradeDiff~countOfVideoEvents+countOfForumEvents+totalTime, data=db.train)
  # plot(model)
  library(caret)
  
  fs = c(
    "countOfVideoEvents",
    "countOfForumEvents",
    "totalTime"
  )
  
  model <- train(
    y=db.train$overalGradeDiff,
    x=db.train[,fs],
    method = "lm"
  )
  
#----- check generalizability of your model on new data
  
  test.pred = predict(model, newdata=db.test[,fs]);
  test.y = db.test$overalGradeDiff
  
  SS.total      <- sum((test.y - mean(test.y))^2)
  SS.residual   <- sum((test.y - test.pred)^2)
  SS.regression <- sum((test.pred - mean(test.y))^2)
  SS.total <- (SS.regression+SS.residual)
  
  # NOT the fraction of variability explained by the model
  test.rsq <- 1 - SS.residual/SS.total  
  test.rsq
  
  # fraction of variability explained by the model
  SS.regression/SS.total 

#======================================================================== 
#         step 2.1: Use classifier to predict progress for test data
#======================================================================== 
  
  testDb=read.csv('OutputTable_test.csv', stringsAsFactors = F)
  testDb$Grade=NULL; testDb$GradeDiff=NULL;
  testDb[is.na(testDb)]=0
  
  #---- use trained model to predict progress for test data
  preds= predict(model, newdata=testDb);
  
#======================================================================== 
#         step 2.1: prepare submission file for kaggle
#======================================================================== 
  
  cl.Results=testDb[,c('ProblemID', 'UserID', 'SubmissionNumber')]
  cl.Results$improved=preds
  levels(cl.Results$improved)=c(0,1) # 
  cl.Results$uniqRowID= paste0(cl.Results$UserID,'_', cl.Results$ProblemID,'_', cl.Results$SubmissionNumber)
  cl.Results=cl.Results[,c('uniqRowID','improved')]
  table(cl.Results$improved)
  
  #----- keep only rows which are listed in classifier_templtae.csv file
  #----- this excludes first submissions and cases with no forum and video event in between two submissions
  classifier_templtaete= read.csv('classifier_templtae.csv', stringsAsFactors = F)
  kaggleSubmission=merge(classifier_templtaete,cl.Results )
  write.csv(kaggleSubmission,file='classifier_results.csv', row.names = F)
  
  
  #------- submit the resulting file (classifier_results.csv) to kaggle 
  #------- report AUC in private score in your report
  
  
  