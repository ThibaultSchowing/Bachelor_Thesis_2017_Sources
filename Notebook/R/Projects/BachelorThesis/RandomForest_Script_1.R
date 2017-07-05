# Random Forest
rm(list=ls())
library(randomForest)
library(caret)

#http://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

dirFol <- "C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/R/Projects/BachelorThesis/"

setwd(dirFol)

#DataBase structure

datNam <- "DataRisaralda_v2Numeric_CompleteV2_utf-8.csv"

dataset   <- read.csv(datNam,row.names=1)
dataset   <- dataset[dataset$Acidez != 0,]
dataset$Acidez = as.factor(dataset$Acidez)


set.seed(123)
inTrain  <- createDataPartition(y=dataset[,72], p=0.7, list=F)
training <- dataset[inTrain,]
testing  <- dataset[-inTrain,]


dataNames = names(dataset)

x <- training[,1:71]
y <- training[,72]

# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=4)

#metric <- "RMSE"

mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(Acidez~.,data=dataset,tunegrid=tunegrid, mtry=mtry, method="rf", trControl=control)
print(rf_default)

plot(predict(rf_default,x),y)
confusionMatrix(predict(rf_default,testing[,1:71]),testing[,72])
