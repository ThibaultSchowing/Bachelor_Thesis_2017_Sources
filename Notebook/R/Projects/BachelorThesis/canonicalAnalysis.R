
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
# Objective: Perform Canonocal Analysis
# 





rm(list=ls())


dataset = read.csv("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/DataAnalysis/data/DataRisaralda_v2Numeric_Complete_utf-8.csv", 
                   header=T,
                   row.names = 1,
                   sep=",", 
                   stringsAsFactors = TRUE)

# remove the 0s
dataset   <- dataset[dataset$PuntajeTotal != 0,]
data = dataset

to.remove <- c("SICA","year","DefectosTotales")
`%ni%` <- Negate(`%in%`)
data = subset(data,select = names(data) %ni% to.remove)

varCoeff <- round(apply(data,2,sd)/apply(data,2,mean)*100,2)
df10 = data[,varCoeff > 10]


require(ggplot2)
require(GGally)
require(CCA)

inputs <- df10[,1:25]
outputs <- df10[,25:29]


#ggpairs(outputs)

cc1 <- cc(inputs, outputs)















