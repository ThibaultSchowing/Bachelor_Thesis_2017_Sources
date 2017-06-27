


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


plotdata <- function(climatdata,data, ax1 = 1, ax2 = 2){

  
  #cafe.class = as.factor(data[,ncol(data)])
  cafe.class = as.factor(data[,'category'])
  
  cafe.pca <- prcomp( climatData,
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
# Les matrices de corrélations nous ont montré que les points étaient tous liésentre eux, on ne prendra donc que le total des points
# et des défauts physiques

climatData <- subset(data, select=c(PrecTotalAvg:DtrTotalAvg,ASNM,LUMINOSID,pH_avg,org_avg,franco_L1:cascajoso_L1,DefectosTotales,PuntajeTotal))

pca = plotdata(climatData, data, ax1 = 1, ax2 = 2)
print(pca$x)
print(pca$rotation)

write.csv(pca$rotation, file = "PCA_Rotation.csv")

# On observe que altitude blabla avec le climat


# avec les données de climat uniquement
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









