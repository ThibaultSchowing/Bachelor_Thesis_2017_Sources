# Random Forest
rm(list=ls())

if(.Platform$OS.type == "unix") {
  library(doMC)
  registerDoMC(cores = 20)
}else{
  dirFol <- "C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/R/Projects/BachelorThesis/"
  setwd(dirFol)
}

library(randomForest)
library(caret)

#http://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/


##################################################################

#Datasets - contain "Variedad" which is a text variable. Might be removed/factorized for some training methods.

datName <- "DataRisaralda_v2Numeric_Complete_cat_utf-8.csv"
dataset_categories   <- read.csv(datName,row.names=1)


datName <- "DataRisaralda_v2_R_Dulzor_utf-8.csv"
dataset_dulzor   <- read.csv(datName,row.names=1)

datName <- "DataRisaralda_v2_R_Acidez_utf-8.csv"
dataset_acidez   <- read.csv(datName,row.names=1)

datName <- "DataRisaralda_v2_R_Total_utf-8.csv"
dataset_total   <- read.csv(datName,row.names=1)

# Remove zero output values -> it doesn't reflect the output because it's not LIMPIA (don't need for category)
dataset_acidez   <- dataset_acidez[dataset_acidez$Acidez != 0,]
dataset_dulzor   <- dataset_dulzor[dataset_dulzor$Dulzor != 0,]
dataset_total   <- dataset_total[dataset_total$PuntajeTotal != 0,]

# Set the outputs - factor for RF and Numerical for ANN
dataset_categories$Category= as.factor(dataset_categories$Category)



##################################################################


set.seed(123)
##################################################################
# Category numerical dataset for neural network

# Remove too highly correlated variables for ANN training
# https://topepo.github.io/caret/pre-processing.html#corr

cutoff = .75

#
dataset_categories_numerical = dataset_categories[,!names(dataset_categories) %in% c("Variedad")]
dataset_categories_numerical$Category= as.numeric(as.character(dataset_categories_numerical$Category))

descrCor <- cor(dataset_categories_numerical)
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = cutoff)

dataset_categories_numerical <- dataset_categories_numerical[,-highlyCorDescr]
descrCor2 <- cor(dataset_categories_numerical)
summary(descrCor2[upper.tri(descrCor2)])

len = dim(dataset_categories_numerical)[2]



inTrain <- createDataPartition(y=dataset_categories_numerical[,len -1], p=0.9, list=F)
trainingCategoryNum <- dataset_categories_numerical[inTrain,]
testingCategoryNum  <- dataset_categories_numerical[-inTrain,]
x_catNum <- trainingCategoryNum[,1:len-1]
y_catNum <- trainingCategoryNum[,len]
print("ycatnum: ")
print(y_catNum)
mtry_catNum <- sqrt(ncol(x_catNum))
tunegrid_catNum <- expand.grid(.mtry=mtry_catNum)

##################################################################
# Preparing data and parameters for random forest
#
#
#
#
##################################################################
##################################################################
# Category dataset
inTrain <- createDataPartition(y=dataset_categories[,73], p=0.9, list=F)
trainingCategory <- dataset_categories[inTrain,]
testingCategory  <- dataset_categories[-inTrain,]
x_cat <- trainingCategory[,1:72]
y_cat <- trainingCategory[,73]
mtry_cat <- sqrt(ncol(x_cat))
tunegrid_cat <- expand.grid(.mtry=mtry_cat)

##################################################################
# Acidez dataset
inTrain <- createDataPartition(y=dataset_acidez[,73], p=0.9, list=F)
trainingAcidez <- dataset_acidez[inTrain,]
testingAcidez  <- dataset_acidez[-inTrain,]
x_Acidez <- trainingAcidez[,1:72]
y_Acidez <- trainingAcidez[,73]
mtry_Acidez <- sqrt(ncol(x_Acidez))
tunegrid_Acidez <- expand.grid(.mtry=mtry_Acidez)

##################################################################
# Dulzor dataset
inTrain <- createDataPartition(y=dataset_dulzor[,73], p=0.9, list=F)
trainingDulzor <- dataset_dulzor[inTrain,]
testingDulzor  <- dataset_dulzor[-inTrain,]
x_Dulzor <- trainingDulzor[,1:72]
y_Dulzor <- trainingDulzor[,73]
mtry_Dulzor <- sqrt(ncol(x_Dulzor))
tunegrid_Dulzor <- expand.grid(.mtry=mtry_Dulzor)

##################################################################
# Total Points dataset
inTrain <- createDataPartition(y=dataset_total[,73], p=0.9, list=F)
trainingTotal <- dataset_total[inTrain,]
testingTotal  <- dataset_total[-inTrain,]
x_Total <- trainingTotal[,1:72]
y_Total <- trainingTotal[,73]
mtry_Total <- sqrt(ncol(x_Total))
tunegrid_Total <- expand.grid(.mtry=mtry_Total)


# We now have 4 different datasets to train

##################################################################
# RANDOM FOREST
#
# IMPORTANT: http://www.listendata.com/2014/11/random-forest-with-r.html
##################################################################
print("Starting Random Forest Trainings")
RFmodelCategory=train(x_cat,y_cat,method="rf",tunegrid=tunegrid_cat,trControl=trainControl(method="repeatedcv",number=10,repeats=3),tuneLength=10,importance = TRUE, proximity=T)
print(".")
RFmodelTotal=train(x_Total,y_Total,method="rf",tunegrid=tunegrid_Total,trControl=trainControl(method="repeatedcv",number=10,repeats=3),tuneLength=10,importance = TRUE, proximity=T)
print(".")
RFmodelAcidez=train(x_Acidez,y_Acidez,method="rf",tunegrid=tunegrid_Acidez,trControl=trainControl(method="repeatedcv",number=10,repeats=3),tuneLength=10,importance = TRUE, proximity=T)
print(".")
RFmodelDulzor=train(x_Dulzor,y_Dulzor,method="rf",tunegrid=tunegrid_Dulzor,trControl=trainControl(method="repeatedcv", number=10,repeats=3),tuneLength=10,importance = TRUE, proximity=T)
print("Random Forest Done !")




set.seed(543)
cafe.rf <- randomForest(Acidez~., dataset_acidez)
partialPlot(cafe.rf, dataset_acidez, ASNM, "Altitude")
partialPlot(cafe.rf, dataset_acidez, Prec1, "Prec1")




















##################################################################
#  Classification with Neural Network
#  
#  Because ANN do not support highly correlated variables,
#  the set has been reduced (See above)
##################################################################


print("Starting ANN Training...")
print(y_catNum)
library(MASS)

nnetFit <- train(x_catNum,y_catNum,method="nnet",preProcess=c("center", "scale"),tuneLength=10,trace=FALSE,maxit=1000,linout=TRUE)
print("ANN Training DONE !")



print("Saving environnement...")
save(list = ls(),file = "TrainedModels.RData")
print("Done !")


