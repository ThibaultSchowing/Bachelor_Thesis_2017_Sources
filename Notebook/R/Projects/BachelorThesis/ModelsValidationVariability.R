
# Bachelor Thesis 2017 - Coffee - Climat - Soils data analysis
# 
# HEIG-VD - University of Applied Sciences of Western Switzerland
# Route de Cheseaux 1, 1400 Yverdon-les-Bains, Switzerland
# 
# International Center for Tropical Agriculture (CIAT)
# Headquarters and Regional Office for Latin America and the Caribbean, 
# Km 17 Recta Cali-Palmira │C.P. 763537 │ A.A. 6713  Cali, Colombia
# 
# 
# Student: Thibault Schowing
# Teacher: Carlos Andrés Peña
# 
# File: 
# Objective: 
# 




rm(list=ls())

library('randomForest')
library("caret")
library(mlbench)
library(ggplot2)
library(reshape2)



if(.Platform$OS.type == "unix") {
  library(doMC)
  registerDoMC(cores = 20)
}else{
  dirFol <- "C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/R/Projects/BachelorThesis/"
  setwd(dirFol)
}

setwd("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/R/Projects/BachelorThesis/VariabilityAnalysis/models/")
load("RFmodelPuntajeVariability10.RData")
load("RFmodelDulzorVariability10.RData")
load("RFmodelCategoresVariability10.RData")

load("RFmodelPuntajeVariability20.RData")
load("RFmodelDulzorVariability20.RData")
load("RFmodelCategoresVariability20.RData")

load("DataVariabilityRF.RData")
setwd(dirFol)






########################################################################
# display infos and save Partial plots, the save parameter is useless
#
# The trick used with "eval" might be usefull to someone else (Thank's Hugo)
########################################################################
partPlots <- function(modele, save = TRUE){
  imp = randomForest::importance(modele$finalModel)
  print(imp)
  impvar = rownames(imp)[order(imp[, 1], decreasing=TRUE)]
  print(impvar)
  #setwd("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/R/Projects/BachelorThesis/PartialPlots/")
  setwd("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/R/Projects/BachelorThesis/VariabilityAnalysis/plots/")
  for (i in seq_along(impvar)) {
    file_name = paste(deparse(substitute(modele)),"_",impvar[i], "_PartialPlot.png", sep="")
    png(file_name, width=4, height=4, units="in", res=300)
    par(mar=c(4,4,1,1))
    exp = paste("partialPlot(modele$finalModel, modele$trainingData,x.var=",impvar[i],", xlab=impvar[i], main=paste('Partial Dependence on', impvar[i]))")
    
    eval(parse(text=exp))
    #partialPlot(modele$finalModel, modele$trainingData,x.var=impvar[i], xlab=impvar[i], main=paste("Partial Dependence on", impvar[i]))
    dev.off()
  }
  
  setwd(dirFol)
  
  #par(op)
}




# Models trained

# RFmodelPuntajeVariability10
# RFmodelDulzorVariability10
# RFmodelCategoresVariability10
# 
# RFmodelPuntajeVariability20
# RFmodelDulzorVariability20
# RFmodelCategoresVariability20

# Predictions with the continuous outputs
Total_pred10 <- predict(RFmodelPuntajeVariability10, testingTotal10)
Total_pred20 <- predict(RFmodelPuntajeVariability20, testingTotal20)

Dulzor_pred10 <- predict(RFmodelDulzorVariability10, testingDulzor10)
Dulzor_pred20 <- predict(RFmodelDulzorVariability20, testingDulzor20)

Category_pred10 <- predict(RFmodelCategoresVariability10, testingCategory10)
Category_pred20 <- predict(RFmodelCategoresVariability20, testingCategories20)







# Creating and saving the partial plots

partPlots(RFmodelPuntajeVariability10)
partPlots(RFmodelPuntajeVariability20)
partPlots(RFmodelDulzorVariability10)
partPlots(RFmodelDulzorVariability20)
partPlots(RFmodelCategoresVariability10)
partPlots(RFmodelCategoresVariability20)



plot(Total_pred10,testingTotal10$PuntajeTotal, xlim = c(0,80), xlab = "Prédiction", ylab = "Valeur actuelle")
plot(Total_pred20,testingTotal20$PuntajeTotal, xlim = c(0,80), xlab = "Prédiction", ylab = "Valeur actuelle")

plot(Category_pred10,testingCategory10$Category)
plot(Category_pred20,testingCategory20$Category)

