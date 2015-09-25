###Gradient Boosting Machine###
require(caret, quietly = TRUE)
require(survival, quietly = TRUE)
require(gbm, quietly = TRUE)
require(plyr, quietly = TRUE)

set.seed(1000)

rawTraining <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
set.seed(1000)
inTrain = createDataPartition(rawTraining$classe, p = 0.75)[[1]]
training = rawTraining[ inTrain,]
validation = rawTraining[-inTrain,]

# Remove fields with mostly NAs and Blanks
training.no <- training[training$new_window == "no",]
NonNACols <- training.no[,!as.vector(apply(is.na(training.no), 2, sum) == nrow(training.no))]
ColsWithValues <- NonNACols[,as.vector(apply(NonNACols=="", 2, sum) < nrow(NonNACols))]
cleanTrain <- training[,colnames(ColsWithValues)]

#Remove Uneeded Fields
cleanTrain <- cleanTrain[,-c(1, 2, 3, 4, 6, 7)]

# Convert Timestamp into an Hour of Day Field
cleanTrain$cvtd_timestamp <- as.numeric(substr(cleanTrain$cvtd_timestamp, 12, 13))

# Principal Components
pcaPreProc <- preProcess(cleanTrain[,-54], method = c('center', 'scale', 'pca'), thresh = 0.9)
pcaTrain <- predict(pcaPreProc, cleanTrain[,-54])
pcaTrain <- data.frame(pcaTrain, classe = cleanTrain[,54])
pcaPreProc

# Set up 10-fold cross validation
CV10 <- trainControl(method = "cv"
                     , number = 10)

# Build Gradient Boosting Machine Model
require(survival, quietly = TRUE)
require(gbm, quietly = TRUE)
require(plyr, quietly = TRUE)
GBMParamGrid <- expand.grid(interaction.depth = c(2, 5, 10)
                            , n.trees = c(100, 200, 500)
                            , shrinkage = 0.1
                            , n.minobsinnode = 10)

GBMModelFit <- train(classe ~ .
                     , method = "gbm"
                     , trControl = CV10
                     , tuneGrid = GBMParamGrid
                     , data = cleanTrain
                     , verbose = FALSE #gbm prints a lot of output if TRUE
)
GBMModelFit
pcaGBMModelFit <- train(classe ~ .
                        , method = "gbm"
                        , trControl = CV10
                        , tuneGrid = GBMParamGrid
                        , data = pcaTrain
                        , verbose = FALSE #gbm prints a lot of output if TRUE
)
pcaGBMModelFit

# Build Random Forest Model
require(randomForest, quietly = TRUE)
RFParamGrid <- expand.grid(mtry = c(2, 10, 25, 50))
RFModelFit <- train(classe ~ .
                    , method = "rf"
                    , data = cleanTrain
                    , trControl = CV10
                    , tuneGrid = RFParamGrid
)
RFModelFit
pcaRFModelFit <- train(classe ~ .
                       , method = "rf"
                       , data = pcaTrain
                       , trControl = CV10
                       , tuneGrid = RFParamGrid
)
pcaRFModelFit

# Build Penalized Multinomial Regression Model
require(klaR, quietly = TRUE)
require(nnet, quietly = TRUE)
MNParamGrid <- expand.grid(decay = c(0.01, 0.001, 0.0001, 0.00001) )
MNModelFit <- train(classe ~ .
                    , method = "multinom"
                    , data = cleanTrain
                    , trControl = CV10
                    , tuneGrid = MNParamGrid
                    , trace = FALSE #multinom prints a lot of output if TRUE
)
MNModelFit
pcaMNModelFit <- train(classe ~ .
                       , method = "multinom"
                       , data = pcaTrain
                       , trControl = CV10
                       , tuneGrid = MNParamGrid
                       , trace = FALSE #multinom prints a lot of output if TRUE
)
pcaMNModelFit
