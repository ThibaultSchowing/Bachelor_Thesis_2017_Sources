# Analyse des Composantes Princpiales (ACP) ou Principal Component Analysis (PCA)
# 
# Les cafés sont représentés par leur classes 
#
#
#
#
#
#
#DataRisaralda_v2Numeric_Complete_utf-8


rm(list=ls())
data = read.csv("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/DataAnalysis/data/DataRisaralda_v2_R_PCA_utf-8.csv", 
                header=T,
                row.names = 1,
                sep=",", 
                stringsAsFactors = TRUE)
data   <- data[data$PuntajeTotal != 0,]

summary(data)

initPackages <- function(){
  
  library(devtools)
  install_github("vqv/ggbiplot")
  
  library(ggbiplot)
  
}


plotdata <- function(datos,data, ax1 = 1, ax2 = 2){

  
  #cafe.class = as.factor(data[,ncol(data)])
  #cafe.class = as.factor(data[,'year'])
  
  cafe.pca <- prcomp( datos,
                      center = TRUE,
                      scale. = TRUE) 
  
  
  plot(cafe.pca, type = "l")
  
  #g <- ggbiplot(cafe.pca,choices = c(ax1,ax2), obs.scale = 1, var.scale = 1, groups = cafe.class, ellipse = TRUE, var.axes = FALSE, varname.size = 3, circle = FALSE)

  g <- ggbiplot(cafe.pca,choices = c(ax1,ax2), obs.scale = 1, var.scale = 1, ellipse = TRUE, var.axes = FALSE, varname.size = 3,
              circle = FALSE)
  
  #g <- g + scale_color_discrete(name = 'Coffee Classes (1 = Outstanding, 2 = excellent, 3 = very good, 4 = below specialty quality)')
  g <- g + scale_color_discrete(name = 'PCA')
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

climatData <- subset(data, select=c(-year))

summary(climatData)
pca = plotdata(climatData, data, ax1 = 1, ax2 = 2)
print(pca$x)
print(pca$rotation)

write.csv(pca$rotation, file = "PCA_Rotation.csv")







data = read.csv("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/DataAnalysis/data/DataRisaralda_v2Numeric_Complete_utf-8.csv", 
                header=T,
                row.names = 1,
                sep=",", 
                stringsAsFactors = TRUE)
data   <- data[data$PuntajeTotal != 0,]


to.remove <- c("SICA","year","Category","PuntajeTotal","PuntajeCatador","TazaLimpia","Balance","Uniformidad","Dulzor","SaborResidual","Sabor","Cuerpo","Acidez","Aroma.Fragrancia","DefectosTotales")
`%ni%` <- Negate(`%in%`)
data = subset(data,select = names(data) %ni% to.remove)


summary(climatData)
pca = plotdata(climatData, data, ax1 = 1, ax2 = 2)
print(pca$x)
print(pca$rotation)

write.csv(pca$rotation, file = "PCA_Rotation.csv")



####################################################################################
#
#
#
####################################################################################

dataset = read.csv("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/DataAnalysis/data/DataRisaralda_v2Numeric_Complete_utf-8.csv", 
                header=T,
                row.names = 1,
                sep=",", 
                stringsAsFactors = TRUE)

# remove the 0s
dataset   <- dataset[dataset$PuntajeTotal != 0,]
data = dataset
dim(summary(data))

to.remove <- c("SICA","year","PuntajeTotal","Category","PuntajeCatador","TazaLimpia","Balance","Uniformidad","Dulzor","SaborResidual","Sabor","Cuerpo","Acidez","Aroma.Fragancia","DefectosTotales")
`%ni%` <- Negate(`%in%`)
data = subset(data,select = names(data) %ni% to.remove)

dim(summary(data))

####################################################################################
#
# Clustering with PCA
#
####################################################################################
library(FactoMineR)

nbClusters = 9

res.pca = PCA(data, scale.unit=TRUE, ncp=nbClusters, graph=TRUE)
res.pca$ind

res.hcpc = HCPC(res.pca,nb.clust=7)

res.hcpc$data.clust


####################################################################################
#
# Plotting outputs vs clusters
# 
# Kruskal test
#
# http://www.r-tutor.com/elementary-statistics/non-parametric-methods/kruskal-wallis-test
#
# A collection of data samples are independent if they come from unrelated 
# populations and the samples do not affect each other. Using the Kruskal-Wallis 
# Test, we can decide whether the population distributions are identical without 
# assuming them to follow the normal distribution.
####################################################################################


boxplot(dataset$Category ~ as.numeric(res.hcpc$data.clust$clust))
table(dataset$Category,as.numeric(res.hcpc$data.clust$clust))
kruskal.test(dataset$Category ~ as.numeric(res.hcpc$data.clust$clust))
# ça montre que ça marche pas sauf pour le 4


boxplot(dataset$Acidez ~ as.numeric(res.hcpc$data.clust$clust))
table(dataset$Acidez,as.numeric(res.hcpc$data.clust$clust))
kruskal.test(dataset$Acidez ~ as.numeric(res.hcpc$data.clust$clust))

boxplot(dataset$Dulzor ~ as.numeric(res.hcpc$data.clust$clust))
table(dataset$Dulzor,as.numeric(res.hcpc$data.clust$clust))
kruskal.test(dataset$Dulzor,as.numeric(res.hcpc$data.clust$clust))


# à mettre dans le rapport: on a essayé avec des méthodes de clustering (ci-dessus avec la catégorie et ci-dessous avec d'autres variables)
# ça ne donne rien de visible -> on utilisera pas les clusters comme variables suplémentaires

####################################################################################
# Multiple Comparison Test After Kruskal-Wallis
# https://www.rdocumentation.org/packages/pgirmess/versions/1.6.5/topics/kruskalmc
# 
####################################################################################



# Variable importance by cluster -> p-value (The lower it is, the higher the importance of the variable)
res.hcpc$desc.var


resp = 
library(pgirmess)

# Diff entre clusters
kruskalmc(dataset$Category, res.hcpc$data.clust$clust, probs = 0.05, cont=NULL)
kruskalmc(dataset$Acidez, res.hcpc$data.clust$clust, probs = 0.05, cont=NULL)
kruskalmc(dataset$Dulzor, res.hcpc$data.clust$clust, probs = 0.05, cont=NULL)
kruskalmc(dataset$PuntajeTotal, res.hcpc$data.clust$clust, probs = 0.05, cont=NULL)


