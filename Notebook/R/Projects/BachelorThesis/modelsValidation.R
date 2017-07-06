# Validation des modèles


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
load('TrainedModels.RData')


# http://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

# Models received from the script "modelsTraining.R"
#
# RFmodelCategory
# RFmodelTotal
# RFmodelAcidez
# RFmodelDulzor
# nnetFit

# REGARDER AVEC PYTHON: https://github.com/jazdev/genreXpose/blob/master/genreXpose/utils.py

# Predictions with the continuous outputs
Total_pred <- predict(RFmodelTotal, testingTotal)
Acidez_pred <- predict(RFmodelAcidez, testingAcidez)
Dulzor_pred <- predict(RFmodelDulzor, testingDulzor)

# Prediction with the nnet
nnetPred <- predict(nnetFit, testingCategoryNum)


# Prediction with the Category output
Category_pred <- predict(RFmodelCategory, testingCategory)

########################################################################







########################################################################


# Partial plots
partPlots <- function(modele){
  imp = importance(modele$finalModel)
  print(imp)
  impvar = rownames(imp)[order(imp[, 1], decreasing=TRUE)]
  print(impvar)
  setwd("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/R/Projects/BachelorThesis/PartialPlots/")
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


# Plot the results of classification
plotResultsPredictionClassification <- function(prediction, testdata,modele, filename){
  
  print(modele)
  varImp(modele)
  
  accKappa = postResample(pred = prediction, obs = testdata$Category)
  print(accKappa)
  
  confMat = confusionMatrix(prediction, testdata$Category)
  
  print(confMat)
  
  
  conf = as.data.frame(confMat$table)
  melted_conf <- melt(conf)
  
  #png(paste(filename, "test.png", sep=""), width=4, height=4, units="in", res=300)
  #par(mar=c(4,4,1,1))
  
  ggplot(data = melted_conf, aes(x=Prediction, y=Reference, fill=value)) + geom_tile()
  #dev.off()
  
}

plotResultsPredictionRegression <- function(prediction, testdata, modele, filename){
  print("Model")
  print(modele)
  
  RMSE_Kappa = postResample(pred = prediction, obs = testdata)
  print(RMSE_Kappa)
  
  print("Best tune:")
  print(modele$bestTune)
  print("Final model")
  print(modele$finalModel)
  
  
  plot(modele$finalModel, main = paste(deparse(substitute(modele)),"_regression"))
  
  varImp(modele)
  
  
  
}



plotResultsPredictionClassification(prediction = Category_pred,testdata = testingCategory, RFmodelCategory, "testclassification")


plotResultsPredictionRegression(Total_pred, testingTotal$PuntajeTotal, RFmodelTotal, "testRegression")
plotResultsPredictionRegression(Total_pred, testingTotal$PuntajeTotal, RFmodelAcidez, "testRegression")
plotResultsPredictionRegression(Total_pred, testingTotal$PuntajeTotal, RFmodelDulzor, "testRegression")

partPlots(RFmodelTotal)
partPlots(RFmodelAcidez)
partPlots(RFmodelCategory)







