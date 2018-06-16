# test a model
library(e1071)
#install.packages("caret")
library(caret)
#remove.packages("caret")
#library(ModelMetrics)

#install.packages("neuralnet")
library(neuralnet)
#install.packages("nnet")
library(nnet)


#===============================================================================
#                         SVM (CLASSIFICATION, FACTOR APPROACH)
#===============================================================================
#===============================================================================
# Helper functions
#===============================================================================
## create a function to build model
FN_trainSVMClass <- function(dtin, gamma = 1, cost = 1){
  # SVM
  datain <- copy(dtin)
  datain[, sentiment := as.factor(sentiment)]
  set.seed(2018)
  model <- svm(sentiment ~ . -phraseId, data= datain,
               kernel = "radial",
               gamma = 1, cost =1, scale = F)
  model
}

FN_trainSVMBestClass <- function(dtin){
  # SVM
  datain <- copy(dtin)
  datain[, sentiment := as.factor(sentiment)]
  set.seed(2018)
  tune.out=tune(svm, sentiment ~ .-phraseId, data=datain, kernel ="radial",
                ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),
                             gamma=c(0.5,1,2,3,4)))

  model.tuned = svm(sentiment ~ . -phraseId, data = datain
                    , kernel ="radial"
                    ,gamma = tune.out$best.parameters$gamma
                    , cost = tune.out$best.parameters$cost)
  model.tuned
}


# create a function to test model 
FN_evalmodelClass <- function(tmodel, datain) {
  tsData <- copy(datain)
  tsData[, sentiment := as.factor(sentiment)]
  x <- predict(tmodel, tsData, type="class")
  y <- confusionMatrix(x, tsData[,sentiment])
  y
}

# run model 
tmodel <- FN_trainSVMClass(plistwide)
# predict on train 
SVMClass.result <- FN_evalmodelClass(tmodel,plistwide.test)
SVMClass.result

#tmodelbest <- FN_trainSVMBestClass(plistwide)
#SVMClass.result1 <- FN_evalmodelClass(tmodelbest,plistwide.test)
#SVMClass.result1

# 38.47% - 200 words - factor 
set.seed (1)
tune.out=tune(svm, sentiment ~ .-phraseId, data=plistwide, kernel ="radial",
                ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),
                             gamma=c(0.5,1,2,3,4)))
summary (tune.out)


#===============================================================================
#                         SVM (REGRESSION, OVE VS ALL APPROACH)
#===============================================================================
#===============================================================================
# Reusable functions 
#===============================================================================
# This function takes a multi-level variable, and transforms it into a binary outcome
# the indicated level (levelOne) is output with value 1, all others become 0
FN_setupLabel <- function(dtin, labelVar="sentiment", levelOne){
  # dtin <- bData
  # labelVar <- "label"
  # levelOne <- "2"
  dsin <- copy(dtin)
  dsin[get(labelVar) == levelOne, eval(labelVar):= -1]
  dsin[get(labelVar) >= 0, eval(labelVar) := 0]
  dsin[, eval(labelVar) := abs(get(labelVar))]
  # table(dsin[,labelVar, with = F])
  dsin
}

#===============================================================================
#                             SVM (REGRESSION, OVE VS ALL APPROACH)  
#===============================================================================
#===============================================================================
# Helper functions
#===============================================================================
# This function trains SVM
FN_trainSVM <- function(dtin, outcomeVar = "sentiment"){
  prepFormula <- as.formula(paste0(outcomeVar," ~ . -phraseId"))
  model <- svm(prepFormula, data= dtin)
  model
}

FN_train.bestSVM <- function(dtin, outcomeVar = "sentiment"){
  prepFormula <- as.formula(paste0(outcomeVar," ~ . -phraseId"))
  tuneResult <- tune(svm, prepFormula,  data = dtin,
                     ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(2:9)), scale = F) 
  tunedModel <- tuneResult$best.model
  tunedModel
}

#tuneResult <- tune(svm, Y ~ X,  data = data,
#ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(2:9))) 
#tunedModel <- tuneResult$best.model
#tunedModelY <- predict(tunedModel, data) 

# and this function evaluates its performance via the confusionMatrix for a given cutoff
FN_evalmodel <- function(tmodel, tsData, whichDigit = workOn, cutoff = 0.5){
  x <- predict(tmodel, tsData)
  x <- as.numeric(x > cutoff)
  y <- confusionMatrix(as.factor(x), as.factor(tsData[,sentiment]))
  y
}


#===============================================================================
#===============================================================================
# build SVR model  for each digit and store it and its best cutoff
#===============================================================================
#===============================================================================
BuildMySVM <- function(
  whichData = trainData 
  , workOn = 0 
  , testingData = keepData 
){
  #===============================================================================
  # Start working on sentiment recognition
  #===============================================================================
  
  set.seed(123)
  #===============================================================================
  # select a balanced sample of sentiment x and non sentiment x rows to train model
  #===============================================================================
  
  bData1 <- whichData[sentiment==workOn,] # take all rows with the sentiment worked on 
  
  # get 20% of each of the other digits' rows:
  bData2List <- lapply(c(0:4)[!c(0:4)%in%c(workOn)], function(x){
    # for each other sentiment, select first its rows
    tt <- whichData[sentiment==x,]
    # then get 20% of them...
    tsp <- sample(nrow(tt), floor(nrow(tt)/5), replace = F)
    # ... finally return those rows
    tt[tsp,]
  })
  
  bData2 <- rbindlist(bData2List) # stack the data tables with samples together
  table(bData2[,sentiment]) # confirm we got a decent representation of all other labels
  bData <- rbind(bData1, bData2) # stack the label and non-label rows
  rm(bData1, bData2, bData2List) # clean-up
  
  # setup outcome (label) to be 1 for the "workOn" sentiment, 0 otherwise
  bDataNew <- FN_setupLabel(bData, "sentiment", levelOne = workOn)
  table(bDataNew[,sentiment])
  bData <- copy(bDataNew); rm(bDataNew)
  
  # prepare the test data the same way
  testData <- FN_setupLabel(testingData, "sentiment", levelOne = workOn)
  
  #===============================================================================
  
  #===============================================================================
  # build SVM training function
  #===============================================================================
#model.svm <- FN_train.bestSVM(bData, outcomeVar = "sentiment")
  model.svm <- FN_trainSVM(bData, outcomeVar = "sentiment")

 
  #===============================================================================
  # evaluate the SVM over a grid to find the optimal cutoff
  #===============================================================================
  gridCutoff <- seq(from=0.01, to = 0.99, by=0.01)
  gridAccuracy <- lapply(gridCutoff, function(x) FN_evalmodel(tmodel = model.svm
                                                      , tsData = testData
                                                      , whichDigit = workOn
                                                      , cutoff = x)$overall["Accuracy"])
  
  # Identify the best cutoff
  SVM.bestCutoff <- gridCutoff[which.max(gridAccuracy)]
  # the next two rows are mostly useful in line-by-line debugging of the function, they do not return any data
  SVM.bestCutoff

  FN_evalmodel(model.svm, testData, cutoff = SVM.bestCutoff) # a bit better...
  
  # return a list containing the model and the best cutoff  
  return(list(model.svm, SVM.bestCutoff))
}

class(plistwide$sentiment)
class(plistwide.test$sentiment)


plistwide[, sentiment := as.numeric(sentiment)]

#write.csv(plistwide, file = "trainSentiment.csv")

model2 <- BuildMySVM(whichData =  plistwide, 0, testingData =  plistwide.test)
model2

# train Boosting for each of the digits 0-9
system.time(
  SVMModelsList <- lapply(c(0:4), function(x) BuildMySVM(plistwide, workOn=x, plistwide.test))
)
# @@ Figure out why we get double printing

#===============================================================================
# Finalize Boosting models for all digits
#===============================================================================
SVMAccuracy <- do.call(cbind, lapply(c(0:4), function(x) {
  keepDataTemp <- FN_setupLabel(plistwide.test, labelVar = "sentiment", levelOne = x)
  tempEval <- FN_evalmodel(SVMModelsList[[x+1]][[1]], keepDataTemp, x, SVMModelsList[[x+1]][[2]])
  print(tempEval$overall["Accuracy"])
}))


predictSN <- predict(SVMModelsList[[1]][[1]], plistwide.test)
predictN <- predict(SVMModelsList[[2]][[1]], plistwide.test)
predictNE <- predict(SVMModelsList[[3]][[1]], plistwide.test)
predictP <- predict(SVMModelsList[[4]][[1]], plistwide.test)
predictSP <- predict(SVMModelsList[[5]][[1]], plistwide.test)

realSent <- cbind(realSent, predictSN, predictN, predictNE, predictP, predictSP)

realSent[, predictNE := as.numeric(predictNE > SVMModelsList[[3]][[2]])]
realSent[, predictP := as.numeric(predictP > SVMModelsList[[4]][[2]])]
realSent[, predictN := as.numeric(predictN > SVMModelsList[[2]][[2]])]
realSent[, predictSN := as.numeric(predictSN > SVMModelsList[[1]][[2]])]
realSent[, predictSP := as.numeric(predictSP > SVMModelsList[[5]][[2]])]

realSent[, predictNE := as.numeric(predictNE > 0.5)]
realSent[, predictP := as.numeric(predictP > 0.5)]
realSent[, predictN := as.numeric(predictN > 0.5)]
realSent[, predictSN := as.numeric(predictSN > 0.5)]
realSent[, predictSP := as.numeric(predictSP > 0.5)]


realSent$final[realSent$predictNE > 0]<- 2
realSent$final[realSent$predictP > 0]<- 3
realSent$final[realSent$predictN > 0]<- 1
realSent$final[realSent$predictSP > 0]<- 4
realSent$final[realSent$predictSN > 0 ]<- 0

for(tcol in names(realSent)){
  realSent[is.na(get(tcol)), eval(tcol):=2]
}

confusionMatrix(as.factor(realSent$final), as.factor(realSent$sentiment))



# neuralnet
# Encode as a one hot vector multilabel data
train <- cbind(plistwide[, 3:132], class.ind(as.factor(plistwide$sentiment)))
# Set labels name
names(train) <- c(names(plistwide)[3:132],"s0","s1","s2","s3","s4")

#train <- copy(plistwide)

#train$s0 = train$sentiment == "0"
#train$s1 = train$sentiment == "1"
#train$s2 = train$sentiment == "2"
#train$s3 = train$sentiment == "3"
#train$s4 = train$sentiment == "4"


# Set up formula
n <- names(train)
f <- as.formula(paste("s0 + s1 + s2 + s3 + s4 ~"
                      , paste(n[!n %in% c("s0","s1","s2","s3","s4")]
                      , collapse = " + ")))
f

set.seed(1951994)
nn <- neuralnet(f,
                data = train,
                hidden = 3,
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign = "minimal")




# First, generate a prediction probability matrix based on a trained neural network and
# the testing dataset, testset:
#fixInNamespace("calculate.neuralnet", pos="package:neuralnet")
net.predict = compute(nn, plistwide.test[,3:132])$net.result
net.predict

#Accuracy
results <- data.frame(actual = plistwide.test$sentiment, prediction = net.predict)
results

results1 <- copy(results)
results1$actual <- NULL
results1 <- data.table(results1)

bestPred <- colnames(results1)[apply(results1,1,which.max)]
results <- cbind(results, bestPred)


results$bestPred <- as.numeric(factor(results$bestPred, 
                                           levels = c("prediction.1", "prediction.2"                                                      ,"prediction.3"
                                                  , "prediction.4"
                                                  ,"prediction.5"), 
                                                   ordered = TRUE))

#results$bestPred <- NULL





roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
net.prediction = c("versicolor", "virginica", "setosa")
[apply(net.predict, 1, which.max)]

attach(roundedresultsdf)
table(actual,prediction)



#Then, obtain other possible labels by finding the column with the greatest probability:

net.prediction = c("s0","s1","s2","s3","s4")[apply(net.predict, 1, which.max)]
#Generate a classification table based on the predicted labels and the labels of the testing dataset:
net.prediction

predict.table = table(plistwide.test$sentiment, net.prediction)
predict.table





#Next, generate classAgreement from the classification table:
classAgreement(predict.table)
confusionMatrix(predict.table)





# Compute predictions
pr.nn <- compute(nn, plistwide.test)

# Extract results
pr.nn_ <- pr.nn$net.result
head(pr.nn_)


# Accuracy (training set)
original_values <- max.col(train[, 131:135])
pr.nn_2 <- max.col(pr.nn_)
mean(pr.nn_2 == original_values)

str(plistwide.test)
summary(plistwide.test)

#nnet 
sent.nn = nnet(sentiment~.-phraseId, data = plistwide, size=10)

summary(sent.nn)

sent.predict = predict(sent.nn, plist.test, type="class")

