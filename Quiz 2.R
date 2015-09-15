# Quiz 2

# Question 1
library(AppliedPredictiveModeling)
library(caret)

#Option 1 doesn't include the diagnosis
data(AlzheimerDisease)
adData = data.frame(predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

#Option 2 makes the training and test sets the same
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[trainIndex,]

#Option 3 Doesn't work because the output is in a list format
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

# Option 4 Looks to be correct
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]


# Question 2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
hist(training$Superplasticizer, main="")
# Large number of values are 0. Log transform cannot adjust that.


# Question 3
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
IL.columns <- colnames(training)[grep("^IL", colnames(training))]
preProcess(training[,IL.columns], method = 'pca', thresh = 0.9)
# PCA needed 9 components to capture 90 percent of the variance

# Question 4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

library(e1071)
IL.columns <- colnames(training)[grep("^IL", colnames(training))]
IL.training <- data.frame(diagnosis = training[,1], training[,IL.columns])
IL.testing <- data.frame(diagnosis = testing[,1], testing[,IL.columns])

#Standard model
ModelFit <- train(IL.training$diagnosis ~., method="glm", data = IL.training)
confusionMatrix(testing$diagnosis, predict(ModelFit, IL.testing))
# Accuracy = 0.6463

# Pre-processed model
pcaPreProc <- preProcess(IL.training[,-1], method = 'pca', thresh = 0.8)
pcaTrain <- predict(pcaPreProc, IL.training[,-1])
pcaModelFit <- train(IL.training$diagnosis ~., method="glm", data = pcaTrain)
pcaTest <- predict(pcaPreProc, IL.testing[,-1])
confusionMatrix(testing$diagnosis, predict(pcaModelFit, pcaTest))
# Accuracy = 0.7195
