
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
# Objective: data exploration
# 

rm(list=ls())
path <- "C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/DataAnalysis/data/"

setwd(path)

filename <- "DataRisaralda_v2_utf-8.csv"

data = read.csv(filename, header=T, sep=",")

summary(data)

plot(data$ASNM, data$PuntajeTotal, ylab = "Points Totaux", xlab = "Altitude [m]")


plot(data$PrecTotalAvg, data$PuntajeTotal, ylab = "Points Totaux", xlab = "Moyenne des précipitations totales")




summary(data$PrecTotalAvg)



### on refait la PCA




rm(list=ls())



initPackages <- function(){
  
  library(devtools)
  install_github("vqv/ggbiplot")
  
  library(ggbiplot)
  
}

# Lecture du dataset compact pour plus de lisibilité. Les 10 moyenne de chaque mois sont remplacée par une moyenne sur les 10 mois. 

data = read.csv("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/DataAnalysis/data/DataRisaralda_v2_R_PCA_utf-8.csv",
                header=T,
                row.names = 1,
                sep=",",
                stringsAsFactors = TRUE)

data = read.csv("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/DataAnalysis/data/DataRisaralda_v2Numeric_Complete_utf-8.csv",
                header=T,
                row.names = 1,
                sep=",",
                stringsAsFactors = TRUE)

to.remove <- c("Variedad")
`%ni%` <- Negate(`%in%`)
data = subset(data,select = names(data) %ni% to.remove)


# on enlève les 0
data   <- data[data$PuntajeTotal != 0,]
print(dim(data))
datayear = data

to.remove <- c("SICA","year","Category")
`%ni%` <- Negate(`%in%`)
data = subset(data,select = names(data) %ni% to.remove)



# Petit problème avec le point
data$Aroma.Fragancia <- NULL

cafe.pca <- prcomp( data,
                    center = TRUE,
                    scale. = TRUE) 

cafe2.pca = PCA(data, scale.unit=TRUE, ncp=nbComp, graph=TRUE)

library(ggfortify)


autoplot(cafe.pca, data = datayear, colour = 'year', shape = TRUE, label.size = 3)
autoplot(cafe.pca, data = datayear, colour = 'Category', shape = TRUE, label.size = 3)



g <- ggbiplot(cafe2.pca, choices = c(1,2), obs.scale = 1, var.scale = 1, ellipse = TRUE, var.axes = FALSE, varname.size = 3,
              circle = FALSE)

#g <- g + scale_color_discrete(name = 'Coffee Classes (1 = Outstanding, 2 = excellent, 3 = very good, 4 = below specialty quality)')
g <- g + scale_color_discrete(name = 'PCA')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)










