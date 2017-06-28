# Analyse des Composantes Princpiales (ACP) ou Principal Component Analysis (PCA)
# 
# Les cafés sont représentés par leur classes 
#
#
#
#
#
#
#



data = read.csv("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/DataAnalysis/data/DataRisaralda_v2Numeric_Complete_class_utf-8.csv", 
                header=T,
                row.names = 1,
                sep=",", 
                stringsAsFactors = TRUE)


summary(data)

initPackages <- function(){
  
  library(devtools)
  install_github("vqv/ggbiplot")
  
  library(ggbiplot)
  
}


plotdata <- function(datos,data, ax1 = 1, ax2 = 2){

  
  #cafe.class = as.factor(data[,ncol(data)])
  cafe.class = as.factor(data[,'category'])
  
  cafe.pca <- prcomp( datos,
                      center = TRUE,
                      scale. = TRUE) 
  
  
  plot(cafe.pca, type = "l")
  
  g <- ggbiplot(cafe.pca,choices = c(ax1,ax2), obs.scale = 1, var.scale = 1, 
                groups = cafe.class, ellipse = TRUE, var.axes = TRUE, varname.size = 3,
                circle = FALSE)
  
  g <- g + scale_color_discrete(name = 'Coffee Classes (1 = Outstanding, 2 = excellent, 3 = very good, 4 = below specialty quality)')
  g <- g + theme(legend.direction = 'horizontal', 
                 legend.position = 'top')
  print(g)
  return(cafe.pca)
}




initPackages()



# Réalisation de la PCA sur toutes les variables pour analyse
# Pour réduire le nombre de variables et améliorer la lisibilité on ne prend que la première couche de texture
# Les matrices de corrélations nous ont montré que les points Ã©taient tous liés entre eux, on ne prendra donc que le total des points
# et des défauts physiques

climatData <- subset(data, select=c(PrecTotalAvg:DtrTotalAvg,ASNM,LUMINOSID,pH_avg,org_avg,franco_L1:cascajoso_L1,DefectosTotales,PuntajeTotal))

pca = plotdata(climatData, data, ax1 = 1, ax2 = 2)
print(pca$x)
print(pca$rotation)

write.csv(pca$rotation, file = "PCA_Rotation.csv")

# On observe que altitude blabla avec le climat


# avec les donnÃ©es de climat uniquement
# Subsample with climatic averages 
climatData <- subset(data, select=c(PrecTotalAvg:DtrTotalAvg,ASNM,PuntajeTotal,DefectosTotales))

pca = plotdata(climatData, data, ax1 = 1, ax2 = 2)
write.csv(pca$rotation, file = "PCA_Rotation.csv")



# Séparer par année - données climatiques

# 2011
# avec les données de climat uniquement
# Subsample with climatic averages 
climatData <- subset(data, select=c(PrecTotalAvg:DtrTotalAvg,ASNM,PuntajeTotal,DefectosTotales), year == '2011')

pca = plotdata(climatData, subset(data, year == '2011'), ax1 = 1, ax2 = 2)
write.csv(pca$rotation, file = "PCA_Rotation_2011.csv")

# 2016
# avec les données de climat uniquement
# Subsample with climatic averages 
climatData <- subset(data, select=c(PrecTotalAvg:DtrTotalAvg,ASNM,PuntajeTotal,DefectosTotales), year == '2016')

pca = plotdata(climatData, subset(data, year == '2016'), ax1 = 1, ax2 = 2)
write.csv(pca$rotation, file = "PCA_Rotation_2016.csv")
print(pca$rotation)


#````````````````````````````````````````````````````````````````````````````````````````````````
#================================================================================================
# Le dataset compact est composé des moyennes climatiques, d'un regroupement des caractéristiques
# du sol, de l'altitude, dela pente, de l'orientation (en degrés, attention au nord), de la
# luminosité, des défauts totaux et des points totaux. 


# PCA avec compact dataset 2016

plotdata2 <- function(datos,data, ax1 = 1, ax2 = 2){
  
  datos = subset(datos, select = -year)
  datos = subset(datos, select = -SICA)
  #cafe.class = as.factor(data[,ncol(data)])
  cafe.class = as.factor(data[,'Category'])
  
  cafe.pca <- prcomp( datos,
                      center = TRUE,
                      scale. = TRUE) 
  
  
  plot(cafe.pca, type = "l")
  
  g <- ggbiplot(cafe.pca,choices = c(ax1,ax2), obs.scale = 1, var.scale = 1, 
                groups = cafe.class, ellipse = TRUE, var.axes = TRUE, varname.size = 3,
                circle = FALSE)
  
  g <- g + scale_color_discrete(name = 'Coffee Classes (1 = Outstanding, 2 = excellent, 3 = very good, 4 = below specialty quality)')
  g <- g + theme(legend.direction = 'horizontal', 
                 legend.position = 'top')
  print(g)
  return(cafe.pca)
}

data = read.csv("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/DataAnalysis/data/df_compact_2_utf-8.csv", 
                header=T,
                row.names = 1,
                sep=",", 
                stringsAsFactors = TRUE)
data = subset(data, select = -VARIEDAD)

climatData <- subset(data, select= -Category, year == '2016')

summary(climatData)


# Complétion des données manquantes pour Orientation et Slope
#dataImputed = rfImpute(climatData, climatData['PuntajeTotal'][,1])
#summary(dataImputed[,-1])

#pca = plotdata2(dataImputed[,-1], subset(data, year == '2016'), ax1 = 1, ax2 = 2)

climatData <- climatData[complete.cases(climatData),]
pca = plotdata2(climatData, subset(data[complete.cases(data),], year == '2016'), ax1 = 1, ax2 = 2)
#write.csv(pca$rotation, file = "PCA_Rotation_2016.csv")
print(pca$rotation)



# PCA avec dataset compact 2011

climatData <- subset(data, select= -Category, year == '2011')

summary(climatData)


# Complétion des données manquantes pour Orientation et Slope
#dataImputed = rfImpute(climatData, climatData['PuntajeTotal'][,1])
#summary(dataImputed[,-1])
#pca = plotdata2(dataImputed[,-1], subset(data, year == '2011'), ax1 = 1, ax2 = 2)

# Si pas de completion, enlever les données manquantes (Dommage)
climatData <- climatData[complete.cases(climatData),]

pca = plotdata2(climatData, subset(data[complete.cases(data),], year == '2011'), ax1 = 1, ax2 = 2)
#write.csv(pca$rotation, file = "PCA_Rotation_2016.csv")
print(pca$rotation)






# PCA avec dataset compact complet


climatData <- subset(data, select= -Category)

summary(climatData)


# Complétion des données manquantes pour Orientation et Slope
dataImputed = rfImpute(climatData, climatData['PuntajeTotal'][,1])
summary(dataImputed[,-1])

pca = plotdata2(dataImputed[,-1], data, ax1 = 1, ax2 = 2)
#write.csv(pca$rotation, file = "PCA_Rotation_2016.csv")
print(pca$rotation)


