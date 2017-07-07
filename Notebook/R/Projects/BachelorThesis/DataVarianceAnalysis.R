# Training models after having removed the data with a low variability coefficient. 



rm(list=ls())

if(.Platform$OS.type == "unix") {
  library(doMC)
  registerDoMC(cores = 20)
}else{
  dirFol <- "C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/R/Projects/BachelorThesis/"
  setwd(dirFol)
}



#dirFol <- "C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/R/Projects/BachelorThesis/"

setwd(dirFol)

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
dataset_acidez   <- dataset_acidez[dataset_acidez$Acidez != 0,]
dataset_dulzor   <- dataset_dulzor[dataset_dulzor$Dulzor != 0,]
dataset_total   <- dataset_total[dataset_total$PuntajeTotal != 0,]

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


##############################################
# Output: PuntajeTotal
##############################################

df10$PuntajeTotal = dataset_total$PuntajeTotal
inTrain <- createDataPartition(y=df10[,dim(df10)[2]], p=0.9, list=F)
trainingTotal <- df10[inTrain,]
testingTotal  <- df10[-inTrain,]
x_Total <- trainingTotal[,1:dim(df10)[2]-1]
y_Total <- trainingTotal[,dim(df10)[2]]
mtry_Total <- sqrt(ncol(x_Total))
tunegrid_Total <- expand.grid(.mtry=mtry_Total)


##################################################################
print("Starting Random Forest Trainings with variability threshold at 10 and with Puntaje Total as output")
RFmodelPuntajeVariability10 = train(x_Total,y_Total,method="rf",tunegrid=tunegrid_Total,trControl=trainControl(method="repeatedcv",number=10,repeats=3),tuneLength=10,importance = TRUE, proximity=T)
print(".")


##############################################
# Output: Dulzor
##############################################

varCoeff <- round(apply(dataset_dulzor,2,sd)/apply(dataset_dulzor,2,mean)*100,2)
df10 = dataset_dulzor[,varCoeff > 10]

inTrain <- createDataPartition(y=df10[,dim(df10)[2]], p=0.9, list=F)
trainingTotal <- df10[inTrain,]
testingTotal  <- df10[-inTrain,]
x_Total <- trainingTotal[,1:dim(df10)[2]-1]
y_Total <- trainingTotal[,dim(df10)[2]]
mtry_Total <- sqrt(ncol(x_Total))
tunegrid_Total <- expand.grid(.mtry=mtry_Total)


##################################################################
print("Starting Random Forest Trainings with variability threshold at 10 and with Puntaje Total as output")
RFmodelDulzorVariability10 = train(x_Total,y_Total,method="rf",tunegrid=tunegrid_Total,trControl=trainControl(method="repeatedcv",number=10,repeats=3),tuneLength=10,importance = TRUE, proximity=T)
print(".")



##############################################
# Output: Categories
##############################################

df10 = dataset_categories[,varCoeff > 10]

inTrain <- createDataPartition(y=df10[,dim(df10)[2]], p=0.9, list=F)
trainingTotal <- df10[inTrain,]
testingTotal  <- df10[-inTrain,]
x_Total <- trainingTotal[,1:dim(df10)[2]-1]
y_Total <- trainingTotal[,dim(df10)[2]]
mtry_Total <- sqrt(ncol(x_Total))
tunegrid_Total <- expand.grid(.mtry=mtry_Total)


##################################################################
print("Starting Random Forest Trainings with variability threshold at 10 and with Puntaje Total as output")
RFmodelCategoresVariability10 = train(x_Total,y_Total,method="rf",tunegrid=tunegrid_Total,trControl=trainControl(method="repeatedcv",number=10,repeats=3),tuneLength=10,importance = TRUE, proximity=T)
print(".")





















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

df20$PuntajeTotal = dataset$PuntajeTotal
inTrain <- createDataPartition(y=df20[,dim(df20)[2]], p=0.9, list=F)
trainingTotal <- df20[inTrain,]
testingTotal  <- df20[-inTrain,]
x_Total <- trainingTotal[,1:dim(df20)[2]-1]
y_Total <- trainingTotal[,dim(df20)[2]]
mtry_Total <- sqrt(ncol(x_Total))
tunegrid_Total <- expand.grid(.mtry=mtry_Total)


##################################################################
print("Starting Random Forest Trainings with variability threshold at 10 and with Puntaje Total as output")
RFmodelPuntajeVariability20 = train(x_Total,y_Total,method="rf",tunegrid=tunegrid_Total,trControl=trainControl(method="repeatedcv",number=10,repeats=3),tuneLength=10,importance = TRUE, proximity=T)
print(".")


##############################################
# Output: Dulzor
##############################################

varCoeff <- round(apply(dataset_dulzor,2,sd)/apply(dataset_dulzor,2,mean)*100,2)
df20 = dataset_dulzor[,varCoeff > 20]

inTrain <- createDataPartition(y=df20[,dim(df20)[2]], p=0.9, list=F)
trainingTotal <- df20[inTrain,]
testingTotal  <- df20[-inTrain,]
x_Total <- trainingTotal[,1:dim(df20)[2]-1]
y_Total <- trainingTotal[,dim(df20)[2]]
mtry_Total <- sqrt(ncol(x_Total))
tunegrid_Total <- expand.grid(.mtry=mtry_Total)


##################################################################
print("Starting Random Forest Trainings with variability threshold at 10 and with Puntaje Total as output")
RFmodelDulzorVariability20 = train(x_Total,y_Total,method="rf",tunegrid=tunegrid_Total,trControl=trainControl(method="repeatedcv",number=10,repeats=3),tuneLength=10,importance = TRUE, proximity=T)
print(".")



##############################################
# Output: Categories
##############################################

varCoeff <- round(apply(dataset_categories,2,sd)/apply(dataset_categories,2,mean)*100,2)
df20 = dataset_categories[,varCoeff > 20]

inTrain <- createDataPartition(y=df20[,dim(df20)[2]], p=0.9, list=F)
trainingTotal <- df20[inTrain,]
testingTotal  <- df20[-inTrain,]
x_Total <- trainingTotal[,1:dim(df20)[2]-1]
y_Total <- trainingTotal[,dim(df20)[2]]
mtry_Total <- sqrt(ncol(x_Total))
tunegrid_Total <- expand.grid(.mtry=mtry_Total)


##################################################################
print("Starting Random Forest Trainings with variability threshold at 10 and with Puntaje Total as output")
RFmodelCategoresVariability20 = train(x_Total,y_Total,method="rf",tunegrid=tunegrid_Total,trControl=trainControl(method="repeatedcv",number=10,repeats=3),tuneLength=10,importance = TRUE, proximity=T)
print(".")



