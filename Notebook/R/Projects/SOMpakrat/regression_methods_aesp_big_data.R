# file data-analysis-AEPS-BigData.R
# 
# This file contains a script to develop regressions with machine learning methodologies
#
#
# author: Hugo Andres Dorado 02-16-2015
#  
#This script is free: you can redistribute it and/or modify
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 

#-----------------------------------------------------------------------------------------------------------------


#SCRIPT BUILED FOR R VERSION 3.0.2 
#PACKAGES


#rm(list=ls())

require(gtools)
require(gridBase)
require(gridExtra)
require(relaimpo)
require(caret)
require(party)
require(randomForest)
require(snowfall)
require(earth)
require(agricolae)
require(cowplot)
require(reshape)
require(stringr)
require(gbm)
require(plyr)

#Load functions; Open  All-Functions-AEPS_BD.RData

load("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/R/Projects/SOMpakrat/All-Functions-AEPS_BD.RData")

#Work Directory

dirFol <- "C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/R/Projects/SOMpakrat"

setwd(dirFol)

#DataBase structure

datNam <- "C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/R/Projects/SOMpakrat/DataRisaralda_v2Numeric_Complete_utf-8_2.csv"

dataSet   <- read.csv(datNam,row.names=1)

#dataSet <- dataSet[,-82]

#segme <- as.factor(dataSet[,62])

#dataSet <- data.frame(dataSet[,1:81],segme,score=dataSet[,82])

#dataSet <- dataSet[,-1]

#head(dataSet)

namsDataSet <- names(dataSet)



inputs  <- 1:80  #inputs columns
segme   <- 81   #split column
output  <- 82   #output column


#Creating the split factors

contVariety <- table(dataSet[,segme])
variety0    <- names(sort(contVariety[contVariety>=30]))


if(length(variety0)==0){variety = variety0 }else{variety = factor(c(variety0,"All"))}


#creating folders
#variety=variety[1:2]

createFolders(dirFol,variety)

#Descriptive Analysis
for(var in variety[1:2]){
descriptiveGraphics(var,dataSet,inputs = inputs,segme = segme,output = output,
                    smooth=F,ylabel = "Score SCAA",smoothInd = NULL,
                    ghrp="box",res=80)
}
#DataSets ProcesosF

library(caret)
dataSetProces(variety,dataSet,segme,corRed="caret")

#LINEAR REGRESSION; only when all inputs are quantitative;  

lineaRegresionFun(variety,dirLocation=paste0(getwd(),"/"),ylabs="Score SCAA")

#MULTILAYER PERCEPTRON

multilayerPerceptronFun(var,dirLocation=paste0(getwd(),"/"),nb.it=30,
                        ylabs="Score SCAA",pertuRelevance=T,ncores=3)

#RANDOM FOREST

randomForestFun("All",nb.it=30,ncores = 3,saveWS=F)


#CONDITIONAL FOREST; especify if you have categorical variables

conditionalForestFun("All",nb.it=30, ncores= 3,saveWS=F)
#GENERALIZED BOOSTED REGRESSION MODELING 

boostingFun("All",nb.it=30,ncores=3,saveWS=F)


