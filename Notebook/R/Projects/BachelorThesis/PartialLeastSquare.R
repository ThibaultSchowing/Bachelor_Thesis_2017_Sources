
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



# Partial least squares
library(plsdepot)


dataset = read.csv("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/DataAnalysis/data/DataRisaralda_v2Numeric_Complete_utf-8.csv", 
                   header=T,
                   row.names = 1,
                   sep=",", 
                   stringsAsFactors = TRUE)

# remove the 0s
dataset   <- dataset[dataset$PuntajeTotal != 0,]
data = dataset




####################################################################################
# Avec Puntaje Total
# https://www.r-bloggers.com/partial-least-squares-regression-in-r/
#
####################################################################################

to.remove <- c("SICA","year","Category","PuntajeCatador","TazaLimpia","Balance","Uniformidad","Dulzor","SaborResidual","Sabor","Cuerpo","Acidez","Aroma.Fragancia","DefectosTotales")
`%ni%` <- Negate(`%in%`)
data = subset(dataset,select = names(dataset) %ni% to.remove)

summary(data)


pls1 = plsreg1(data[,1:ncol(data)-1], data[, ncol(data), drop = FALSE], comps = 3)



# Graphique super pour le rapport !!!
plot(pls1)

pls1$x.scores

# valeurs des composants extraits (paramètre comps)
pls1$R2



plot(data$PuntajeTotal, pls1$y.pred,  xlab="Original", ylab = "Predicted", ylim= c(40,82))
title("Comparison of responses", cex.main = 0.9)
abline(a = 0, b = 1, col = "gray85", lwd = 2)
#text(data$PuntajeTotal, pls1$y.pred, col = "#5592e3")

####################################################################################
# Avec Acidez
# https://www.r-bloggers.com/partial-least-squares-regression-in-r/
#
####################################################################################

to.remove <- c("SICA","year","Category","PuntajeCatador","TazaLimpia","Balance","Uniformidad","Dulzor","SaborResidual","Sabor","Cuerpo","PuntajeTotal","Aroma.Fragancia","DefectosTotales")
`%ni%` <- Negate(`%in%`)
data = subset(dataset,select = names(dataset) %ni% to.remove)

summary(data)


pls1 = plsreg1(data[,1:ncol(data)-1], data[, ncol(data), drop = FALSE], comps = 3)



# Graphique super pour le rapport !!!
plot(pls1)

pls1$x.scores

# valeurs des composants extraits (paramètre comps)
pls1$R2

pls1$

plot(data$Acidez, pls1$y.pred,  xlab="Original", ylab = "Predicted", ylim = c(5,8.7))
title("Comparison of responses", cex.main = 0.9)
abline(a = 0, b = 1, col = "gray85", lwd = 2)
#text(data$Acidez, pls1$y.pred, col = "#5592e3")



####################################################################################
# Avec Dulzor
# https://www.r-bloggers.com/partial-least-squares-regression-in-r/
#
####################################################################################

to.remove <- c("SICA","year","Category","PuntajeCatador","TazaLimpia","Balance","Uniformidad","Acidez","SaborResidual","Sabor","Cuerpo","PuntajeTotal","Aroma.Fragancia","DefectosTotales")
`%ni%` <- Negate(`%in%`)
data = subset(dataset,select = names(dataset) %ni% to.remove)

summary(data)


pls1 = plsreg1(data[,1:ncol(data)-1], data[, ncol(data), drop = FALSE], comps = 3)



# Graphique super pour le rapport !!!
plot(pls1)

pls1$x.scores

# valeurs des composants extraits (paramètre comps)
pls1$R2



plot(data$Dulzor, pls1$y.pred,  xlab="Original", ylab = "Predicted",ylim = c(0,11))
title("Comparison of responses", cex.main = 0.9)
abline(a = 0, b = 1, col = "gray85", lwd = 2)
#text(data$Acidez, pls1$y.pred, col = "#5592e3")


####################################################################################
# Avec la catégorie
# https://www.r-bloggers.com/partial-least-squares-regression-in-r/
#
####################################################################################


to.remove <- c("SICA","year","PuntajeTotal","PuntajeCatador","TazaLimpia","Balance","Uniformidad","Dulzor","SaborResidual","Sabor","Cuerpo","Acidez","Aroma.Fragancia","DefectosTotales")
`%ni%` <- Negate(`%in%`)
data = subset(dataset,select = names(dataset) %ni% to.remove)

summary(data)


pls1 = plsreg1(data[,1:ncol(data)-1], data[, ncol(data), drop = FALSE], comps = 3 )


# Graphique super pour le rapport -> corrélation 
plot(pls1)
#Perf
postResample(pls1$y,pls1$y.pred)

plot(data$Category, pls1$y.pred, xlab="Original", ylab = "Predicted", ylim = c(2,4))
title("Comparison of responses", cex.main = 0.9)
abline(a = 0, b = 1, col = "gray85", lwd = 2)
#text(data$Category, pls1$y.pred, col = "#5592e3")



####################################################################################
# Tentative avec un dataset plus compact 
# https://www.r-bloggers.com/partial-least-squares-regression-in-r/
#
####################################################################################

data = read.csv("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/DataAnalysis/data/DataRisaralda_v2_R_PCA_utf-8.csv",
                header=T,
                row.names = 1,
                sep=",",
                stringsAsFactors = TRUE)
# on enlève les 0
data   <- data[data$PuntajeTotal != 0,]

to.remove <- c("SICA","year","Category","PuntajeCatador","TazaLimpia","Balance","Uniformidad","Dulzor","SaborResidual","Sabor","Cuerpo","Acidez","Aroma.Fragancia","DefectosTotales")
`%ni%` <- Negate(`%in%`)
data = subset(dataset,select = names(dataset) %ni% to.remove)

pls1 = plsreg1(data[,1:ncol(data)-1], data[, ncol(data), drop = FALSE], comps = 3 )


# Graphique super pour le rapport -> corrélation 
plot(pls1)

plot(data$PuntajeTotal, pls1$y.pred,  xlab="Original", ylab = "Predicted", ylim= c(40,82))
title("Comparison of responses", cex.main = 0.9)
abline(a = 0, b = 1, col = "gray85", lwd = 2)

# Tentative de classification avec PLS - utile pour structure utilisation caret
###############################################################################

# set.seed(849)
# 
# set.seed(123)
# inTrain  <- createDataPartition(y=data[,72], p=0.7, list=F)
# training <- data[inTrain,]
# testing  <- data[-inTrain,]
# 
# x <- training[,1:71]
# y <- as.factor(training[,72])
# 
# 
# ctrl <- trainControl(method = "cv",number=2, repeats = 2)
# 
# tune <- expand.grid(.maxvar = 15, 
#                     .direction = "both")
# 
# plsFit <- train(x,  y,method = "stepQDA", tuneGrid = tune,   trControl = ctrl)
# plot(plsFit)
# plot(plsFit$finalModel)
