# Analysis of the RF training with low variability variables eliminated



rm(list=ls())

library(caret)
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
# display infos and save Partial plots
########################################################################
partPlots <- function(modele, save = TRUE, path = "C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/R/Projects/BachelorThesis/PartialPlotsVariability/"){
  imp = importance(modele$finalModel)
  print(imp)
  impvar = rownames(imp)[order(imp[, 1], decreasing=TRUE)]
  print(impvar)
  #setwd("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/R/Projects/BachelorThesis/PartialPlots/")
  setwd(path)
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
Total_pred10 <- predict(RFmodelPuntajeVariability10, testingTotal)
Total_pred20 <- predict(RFmodelPuntajeVariability20, testingTotal)

Acidez_pred <- predict(RFmodelAcidez, testingTotal)
Dulzor_pred <- predict(RFmodelDulzor, testingTotal)


# Prediction with the Category output
Category_pred <- predict(RFmodelCategory, testingTotal)


















