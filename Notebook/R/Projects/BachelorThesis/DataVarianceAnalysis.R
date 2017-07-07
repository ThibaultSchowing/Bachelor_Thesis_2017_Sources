# Training models after having removed the data with a low variability coefficient. 
library('randomForest')
library("caret")

rm(list=ls())

if(.Platform$OS.type == "unix") {
  library(doMC)
  registerDoMC(cores = 20)
}else{
  dirFol <- "C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/R/Projects/BachelorThesis/"
  setwd(dirFol)
}



#dirFol <- "C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/R/Projects/BachelorThesis/"

#setwd(dirFol)

datName <- "DataRisaralda_v2Numeric_Complete_cat_utf-8.csv"
dataset_categories   <- read.csv(datName,row.names=1)
dataset_categories$Variedad <- NULL

datName <- "DataRisaralda_v2_R_Dulzor_utf-8.csv"
dataset_dulzor   <- read.csv(datName,row.names=1)
dataset_dulzor$Variedad <- NULL

datName <- "DataRisaralda_v2_R_Acidez_utf-8.csv"
dataset_acidez   <- read.csv(datName,row.names=1)
dataset_acidez$Variedad <- NULL

datName <- "DataRisaralda_v2_R_Total_utf-8.csv"
dataset_total   <- read.csv(datName,row.names=1)
dataset_total$Variedad <- NULL

# Remove zero output values -> it doesn't reflect the output because it's not LIMPIA (don't need for category)
#dataset_acidez   <- dataset_acidez[dataset_acidez$Acidez != 0,]
#dataset_dulzor   <- dataset_dulzor[dataset_dulzor$Dulzor != 0,]
#dataset_total   <- dataset_total[dataset_total$PuntajeTotal != 0,]

# Set the outputs - factor for RF and Numerical for ANN
dataset_categories$Category= as.factor(dataset_categories$Category)




# Coeficient of variation - use for feature selection

# 
# 
# # Standart deviation
# apply(data,2,sd)
# # Idem à SD -> racine de la variance
# sqrt(apply(data,2,var))
# 
# # Variance http://www.r-tutor.com/elementary-statistics/numerical-measures/variance





########################################################################
########################################################################
#
# Subset with threshold 10
#
########################################################################
########################################################################

varCoeff <- round(apply(dataset_total,2,sd)/apply(dataset_total,2,mean)*100,2)
df10 = dataset_total[,varCoeff > 10]
# the output (total) is automaticaly dismissed (low variability) so we can 
# use the same table "varCoeff" for all sets. 

##############################################
# Output: PuntajeTotal
##############################################

df10$PuntajeTotal = dataset_total$PuntajeTotal
inTrain <- createDataPartition(y=df10[,dim(df10)[2]], p=0.9, list=F)
trainingTotal10 <- df10[inTrain,]
testingTotal10  <- df10[-inTrain,]
x_Total10 <- trainingTotal10[,1:dim(df10)[2]-1]
y_Total10 <- trainingTotal10[,dim(df10)[2]]
mtry_Total <- sqrt(ncol(x_Total10))
tunegrid_Total <- expand.grid(.mtry=mtry_Total)


##################################################################
print("Starting Random Forest Trainings with variability threshold at 10 and with Puntaje Total as output")
RFmodelPuntajeVariability10 = train(x_Total10,y_Total10,method="rf",tunegrid=tunegrid_Total,trControl=trainControl(method="repeatedcv",number=10,repeats=3),tuneLength=10,importance = TRUE, proximity=T)
print(".")
df10$PuntajeTotal = NULL

##############################################
# Output: Dulzor
##############################################

df10$Dulzor = dataset_dulzor$Dulzor
inTrain <- createDataPartition(y=df10[,dim(df10)[2]], p=0.9, list=F)
trainingDulzor10 <- df10[inTrain,]
testingDulzor10  <- df10[-inTrain,]
x_Dulzor10 <- trainingDulzor10[,1:dim(df10)[2]-1]
y_Dulzor10 <- trainingDulzor10[,dim(df10)[2]]
mtry_Dulzor10 <- sqrt(ncol(x_Dulzor10))
tunegrid_Dulzor10 <- expand.grid(.mtry=mtry_Dulzor10)


##################################################################
print("Starting Random Forest Trainings with variability threshold at 10 and with Dulzor as output")
RFmodelDulzorVariability10 = train(x_Dulzor10,y_Dulzor10,method="rf",tunegrid=tunegrid_Dulzor10,trControl=trainControl(method="repeatedcv",number=10,repeats=3),tuneLength=10,importance = TRUE, proximity=T)
print(".")
df10$Dulzor = NULL


##############################################
# Output: Categories
##############################################

df10$Category = dataset_categories$Category
inTrain <- createDataPartition(y=df10[,dim(df10)[2]], p=0.9, list=F)
trainingCategory10 <- df10[inTrain,]
testingCategory10  <- df10[-inTrain,]
x_Category10 <- trainingCategory10[,1:dim(df10)[2]-1]
y_Category10 <- trainingCategory10[,dim(df10)[2]]
mtry_Category10 <- sqrt(ncol(x_Category10))
tunegrid_Category10 <- expand.grid(.mtry=mtry_Category10)


##################################################################
print("Starting Random Forest Trainings with variability threshold at 10 and with Category as output")
RFmodelCategoresVariability10 = train(x_Category10,y_Category10,method="rf",tunegrid=tunegrid_Category10,trControl=trainControl(method="repeatedcv",number=10,repeats=3),tuneLength=10,importance = TRUE, proximity=T)
print(".")
df10$Category = NULL




















########################################################################
########################################################################
#
# Subset with threshold 20
#
########################################################################
########################################################################

varCoeff <- round(apply(dataset_total,2,sd)/apply(dataset_total,2,mean)*100,2)
df20 = dataset_total[,varCoeff > 20]


##############################################
# Output: PuntajeTotal
##############################################

df20$PuntajeTotal = dataset_total$PuntajeTotal
inTrain <- createDataPartition(y=df20[,dim(df20)[2]], p=0.9, list=F)
trainingTotal20 <- df20[inTrain,]
testingTotal20  <- df20[-inTrain,]
x_Total20 <- trainingTotal20[,1:dim(df20)[2]-1]
y_Total20 <- trainingTotal20[,dim(df20)[2]]
mtry_Total20 <- sqrt(ncol(x_Total20))
tunegrid_Total20 <- expand.grid(.mtry=mtry_Total20)


##################################################################
print("Starting Random Forest Trainings with variability threshold at 20 and with Puntaje Total as output")
RFmodelPuntajeVariability20 = train(x_Total20,y_Total20,method="rf",tunegrid=tunegrid_Total20,trControl=trainControl(method="repeatedcv",number=10,repeats=3),tuneLength=10,importance = TRUE, proximity=T)
print(".")
df20$PuntajeTotal = NULL

##############################################
# Output: Dulzor
##############################################

df20$Dulzor = dataset_dulzor$Dulzor

inTrain <- createDataPartition(y=df20[,dim(df20)[2]], p=0.9, list=F)
trainingDulzor20 <- df20[inTrain,]
testingDulzor20  <- df20[-inTrain,]
x_Dulzor20 <- trainingDulzor20[,1:dim(df20)[2]-1]
y_Dulzor20 <- trainingDulzor20[,dim(df20)[2]]
mtry_Dulzor20 <- sqrt(ncol(x_Dulzor20))
tunegrid_Dulzor20 <- expand.grid(.mtry=mtry_Dulzor20)


##################################################################
print("Starting Random Forest Trainings with variability threshold at 20 and with Dulzor as output")
RFmodelDulzorVariability20 = train(x_Dulzor20,y_Dulzor20,method="rf",tunegrid=tunegrid_Dulzor20,trControl=trainControl(method="repeatedcv",number=10,repeats=3),tuneLength=10,importance = TRUE, proximity=T)
print(".")

df20$Dulzor = NULL

##############################################
# Output: Categories
##############################################

df20$Category = dataset_categories$Category

inTrain <- createDataPartition(y=df20[,dim(df20)[2]], p=0.9, list=F)
trainingCategories20 <- df20[inTrain,]
testingCategories20  <- df20[-inTrain,]
x_Categories20 <- trainingCategories20[,1:dim(df20)[2]-1]
y_Categories20 <- trainingCategories20[,dim(df20)[2]]
mtry_Categories20 <- sqrt(ncol(x_Categories20))
tunegrid_Categories20 <- expand.grid(.mtry=mtry_Categories20)


##################################################################
print("Starting Random Forest Trainings with variability threshold at 20 and with Category as output")
RFmodelCategoresVariability20 = train(x_Categories20,y_Categories20,method="rf",tunegrid=tunegrid_Categories20,trControl=trainControl(method="repeatedcv",number=10,repeats=3),tuneLength=10,importance = TRUE, proximity=T)
print(".")

df20$Category = NULL




print("Saving environnement...")

save(RFmodelPuntajeVariability10, file = "RFmodelPuntajeVariability10.RData")
save(RFmodelDulzorVariability10, file = "RFmodelDulzorVariability10.RData")
save(RFmodelCategoresVariability10, file = "RFmodelCategoresVariability10.RData")

save(RFmodelPuntajeVariability20, file = "RFmodelPuntajeVariability20.RData")
save(RFmodelDulzorVariability20, file = "RFmodelDulzorVariability20.RData")
save(RFmodelCategoresVariability20, file = "RFmodelCategoresVariability20.RData")

save(list = ls(), file = "DataVariabilityRF.RData")
print("Done !")









# RFmodelPuntajeVariability10
# RFmodelDulzorVariability10
# RFmodelCategoresVariability10
# 
# RFmodelPuntajeVariability20
# RFmodelDulzorVariability20
# RFmodelCategoresVariability20












