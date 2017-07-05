# Validation des modèles

rm(list=ls())
if(.Platform$OS.type == "unix") {
  library(doMC)
  registerDoMC(cores = 20)
}else{
  dirFol <- "C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/R/Projects/BachelorThesis/"
  setwd(dirFol)
}
load('TrainedModels.RData')
library(caret)
library(mlbench)
library(ggplot2)

# http://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

# Models received from the script "modelsTraining.R"
#
# RFmodelCategory
# RFmodelTotal
# RFmodelAcidez
# RFmodelDulzor
# nnetFit



Total_pred <- predict(RFmodelTotal, testingTotal)
Acidez_pred <- predict(RFmodelAcidez, testingAcidez)
Dulzor_pred <- predict(RFmodelDulzor, testingDulzor)

nnetPred <- predict(nnetFit, testingCategoryNum)


# Prediction with the Category output
Category_pred <- predict(RFmodelCategory, testingCategory)

postResample(pred = Category_pred, obs = testingCategory$Category)
confusionMatrix_category = confusionMatrix(Category_pred, testingCategory$Category)
plot(confusionMatrix_category$table)



library(reshape2)

conf = as.data.frame(confusionMatrix_category$table)

melted_conf <- melt(conf)
ggplot(data = melted_conf, aes(x=Prediction, y=Reference, fill=value)) + geom_tile()






















