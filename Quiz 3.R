# Quiz 3

# Question 1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
set.seed(125)
summary(segmentationOriginal)
training = segmentationOriginal[segmentationOriginal$Case == "Train",]
testing = segmentationOriginal[segmentationOriginal$Case == "Test",]
modelFit <- train(Class ~., method="rpart", data=training)
modelFit$finalModel
"a. PS
 b. WS
 c. PS
 d. Not possible to predict"


# Question 2
"With K-fold cross validation, the following is true:
 - The bias is smaller with increasing K
 - Variance increases with increasing K
 - Leave-1-Out CV is the same as K-Fold CV where K = the sample size."


# Question 3
load("/Users/brettromero/Documents/Coursera/Practical Machine Learning/olive.rda")
modelFit <- train(Area ~., method="rpart", data=olive)
newdata = as.data.frame(t(colMeans(olive)))
predict(modelFit, newdata)
" 2.783282
  Result is strange because the Area appears to be a factor variable"


# Question 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
str(trainSA)
set.seed(13234)
modelFit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl
                  , data = trainSA
                  , method = "glm"
                  , family = "binomial"
                  )

missClass = function(values, prediction){
  sum(((prediction > 0.5)*1) != values)/length(values)
}

trainPred <- predict(modelFit, trainSA)
missClass(trainSA$chd, trainPred)
" 0.2727273"
testPred <- predict(modelFit, testSA[,-10])
missClass(testSA$chd, testPred)
" 0.3116883"


# Question 5 
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)

modelFit <- train(y ~ . , method = "rf", data = vowel.train)
varImp(modelFit)
"Overall
x.2  100.000
x.1   98.834
x.5   38.651
x.6   25.628
x.8   19.065
x.4    8.397
x.9    5.417
x.3    4.728
x.7    1.252
x.10   0.000"