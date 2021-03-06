library(dplyr)
library(plyr) #ddply
library(caret)
library(doMC)
library(corrplot)
#======================================================================== 
#         step 1: train classifier
#======================================================================== 

  #------ read features extracted from train set, using your python script
  db=read.csv('features.csv', stringsAsFactors = F)
  
  #------ sort submissions
  db=db[order(db$UserID,db$ProblemID),]
  
  #--- replace NA values with 0
  db[is.na(db)]=0
  

  
  # ----- (Optional) split your training data into train and test set. Use train set to build your classifier and try it on test data to check generalizability. 
  set.seed(1234)
  tr.index= sample(nrow(db), nrow(db)*0.99)
  db.train= db[tr.index,]
  db.test = db[-tr.index,]
  dim(db.train)
  dim(db.test)
  
  library(caret)
  
  fs = c(
    "countOfVideoEvents",
    #"countOfForumEvents",
    "ProblemID",
    "totalTime",
    "totalVideoTime",
    "NoOfVidoesWatched",
    "countOfSubmissions",
    "countOfThreadViews"
  )
  
  #for normalized features
  fsNorm =c(
    #"VideoPerSubmission",
    #"ForumPerSubmission",
    "ProblemID",
    "totalTime",
    "NoOfVidoesWatchedPerSubmission",
    "VideoTimePerSubmission",
    "ThreadViewPerSubmission",
    "countOfSubmissions"
  )
  
  #check correlation
  correlation_matrix <- cor(db.train[,fs])
  corrplot(correlation_matrix, method = "number")
  
  #Control function
  set.seed(123)
  ctrl <- trainControl(method = "repeatedcv", repeats =3, number = 10)
  
  #lm
  linearModel <- train(
    y=db.train$overalGradeDiff,
    x=db.train[,fsNorm],
    trControl = ctrl,
    metric = "RMSE",
    method = "lm"
  )

  #svmLinear
  svmLin <- train(y=db.train$overalGradeDiff,
                  x=db.train[,fs],
                  method= "svmLinear",
                  tuneLength=15,
                  trControl=ctrl,
                  metric ="RMSE",
                  preProc= c("center", "scale"))
  
  grid <- expand.grid(sigma = c(.980, .981, 0.982),
                      C = c(0.21, 0.22, 0.23)
  )
  #svmRadial
  svmRad <- train(y=db.train$overalGradeDiff,
                  x=db.train[,fs],
                  method= "svmRadial",
                  trControl=ctrl,
                  tuneGrid = grid,
                  metric ="RMSE",
                  preProc= c("center", "scale"))
  
  #Gaussian Process with Radial Basis Function Kernel
  gmr <- train(y=db.train$overalGradeDiff,
               x=db.train[,fs],
               method= "gaussprRadial",
               trControl=ctrl,
               metric="RMSE",
               preProc= c("center", "scale"))
  
  #cubist
  cubist<- train(y=db.train$overalGradeDiff,
                 x=db.train[,fs],
               method= "cubist",
               trControl=ctrl,
               tuneGrid = expand.grid(committees = c(20,21,22,23,24), neighbors = c(9)),
               metric="RMSE",
               preProc= c("center", "scale"))
  
  #glmnet
  glmnet<- train(y=db.train$overalGradeDiff,
                 x = db.train[,fsNorm],
                 method="glmnet",
                 trControl = ctrl,
                 tuneGrid = expand.grid(alpha = (1:10) * 0.1, lambda = (1:10) * 0.1),
                 metric = "RMSE",
                 preProc = c("center", "scale"))
  model4
  
  
#----- check generalizability of your model on new data
  
  test.pred = predict(model4, newdata=db.test[,fsNorm]);
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
  
  testDb=read.csv('features_test.csv', stringsAsFactors = F)
  testDb$OveralGradeDiff=NULL
  testDb[is.na(testDb)]=0
  
  #---- use trained model to predict progress for test data
  preds= predict(model4, newdata=testDb[,fsNorm]);
  
#======================================================================== 
#         step 2.1: prepare submission file for kaggle
#======================================================================== 
  
  cl.Results=testDb[,c('ProblemID', 'UserID')]
  cl.Results$overalGradeDiff=preds
  cl.Results$uniqRowID= paste0(cl.Results$UserID,'_', cl.Results$ProblemID)
  cl.Results=cl.Results[,c('uniqRowID','overalGradeDiff')]
  table(cl.Results$overalGradeDiff)
  
  #----- keep only rows which are listed in classifier_templtae.csv file
  #----- this excludes first submissions and cases with no forum and video event in between two submissions
  regression_template= read.csv('regression_template.csv', stringsAsFactors = F)
  kaggleSubmission=merge(regression_template,cl.Results )
  kaggleSubmission$overalGradeDiff[kaggleSubmission$overalGradeDiff>100] <- 100
  write.csv(kaggleSubmission,file='regression_results.csv', row.names = F)
  
  
  #------- submit the resulting file (classifier_results.csv) to kaggle 
  #------- report AUC in private score in your report
  
  
  