# Validation des modèles

rm(list=ls())
load('TrainedModels.RData')


# http://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

# Models received from the script "modelsTraining.R"
#
# RFmodelCategory
# RFmodelTotal
# RFmodelAcidez
# RFmodelDulzor
# nnetFit


Category_pred <- predict(RFmodelCategory, testingCategory)
Total_pred <- predict(RFmodelTotal, testingTotal)
Acidez_pred <- predict(RFmodelAcidez, testingAcidez)
Dulzor_pred <- predict(RFmodelDulzor, testingDulzor)
