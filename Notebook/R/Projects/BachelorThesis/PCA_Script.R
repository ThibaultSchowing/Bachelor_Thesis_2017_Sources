
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

  
  pdf("pca_simplified.pdf",width=6,height=4,paper='special') 
  g <- ggbiplot(cafe.pca,choices = c(ax1,ax2), obs.scale = 1, var.scale = 1, ellipse = TRUE, var.axes = FALSE, varname.size = 3,
              circle = FALSE)
  
  #g <- g + scale_color_discrete(name = 'Coffee Classes (1 = Outstanding, 2 = excellent, 3 = very good, 4 = below specialty quality)')
  g <- g + scale_color_discrete(name = 'PCA')
  g <- g + theme(legend.direction = 'horizontal', 
                 legend.position = 'top')
  print(g)
  dev.off()
  return(cafe.pca)
}




initPackages()



# Lecture du dataset compact pour plus de lisibilité. Les 10 moyenne de chaque mois sont remplacée par une moyenne sur les 10 mois. 

data = read.csv("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/DataAnalysis/data/DataRisaralda_v2_R_PCA_utf-8.csv",
                header=T,
                row.names = 1,
                sep=",",
                stringsAsFactors = TRUE)
# on enlève les 0
data   <- data[data$PuntajeTotal != 0,]
print(dim(data))


to.remove <- c("SICA","year","Category","PuntajeTotal","PuntajeCatador","TazaLimpia","Balance","Uniformidad","Dulzor","SaborResidual","Sabor","Cuerpo","Acidez","Aroma.Fragrancia","DefectosTotales")
`%ni%` <- Negate(`%in%`)
data = subset(data,select = names(data) %ni% to.remove)
# Petit problème avec le point
data$Aroma.Fragancia <- NULL


summary(data)

pca = plotdata(data, data, ax1 = 1, ax2 = 2)


print(pca$x)
print(pca$rotation)

# Fichier avec importance de chaque variable pour chaque dimensions. 
write.csv(pca$rotation, file = "PCA_Rotation_Minimal.csv")






# Fichier complet contenant toutes les informations climatiques 

data = read.csv("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/DataAnalysis/data/DataRisaralda_v2Numeric_Complete_utf-8.csv", 
                header=T,
                row.names = 1,
                sep=",", 
                stringsAsFactors = TRUE)

# On enlève les zéros
data   <- data[data$PuntajeTotal != 0,]

# On enlève les sorties
to.remove <- c("SICA","year","Category","PuntajeTotal","PuntajeCatador","TazaLimpia","Balance","Uniformidad","Dulzor","SaborResidual","Sabor","Cuerpo","Acidez","Aroma.Fragrancia","DefectosTotales")
`%ni%` <- Negate(`%in%`)
climatdata = subset(data,select = names(data) %ni% to.remove)
climatdata$Aroma.Fragancia <- NULL


summary(climatdata)
# graph avec toutes les données, sans les zéros
pca = plotdata(climatdata, data, ax1 = 1, ax2 = 2)
print(pca$x)
print(pca$rotation)

write.csv(pca$rotation, file = "PCA_Rotation_Complete.csv")




####################################################################################

setwd("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/R/Projects/BachelorThesis/PCA/")

####################################################################################
#
# Reading the datas and removing the 0-points coffee
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

# on enlève les sorties et les informations inutiles aux calculs

to.remove <- c("SICA","year","PuntajeTotal","Category","PuntajeCatador","TazaLimpia","Balance","Uniformidad","Dulzor","SaborResidual","Sabor","Cuerpo","Acidez","Aroma.Fragancia","DefectosTotales")
`%ni%` <- Negate(`%in%`)
data = subset(data,select = names(data) %ni% to.remove)

summary(data)
dim(summary(data))

####################################################################################
#
# Simple PCA
#
####################################################################################


cafe.pca <- prcomp( data,
                    center = TRUE,
                    scale. = TRUE) 
plot(cafe.pca, type = "l")

pdf("pca_complete.pdf",width=6,height=4,paper='special') 

g <- ggbiplot(cafe.pca,choices = c(1,2), obs.scale = 1, var.scale = 1, ellipse = TRUE, var.axes = FALSE, varname.size = 3,
              circle = FALSE)

#g <- g + scale_color_discrete(name = 'Coffee Classes (1 = Outstanding, 2 = excellent, 3 = very good, 4 = below specialty quality)')
g <- g + scale_color_discrete(name = 'PCA')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)


dev.off()

print(cafe.pca$rotation)

write.csv(cafe.pca$rotation, file = "PCA_Rotation_Complete.csv")

####################################################################################
#
# Clustering with PCA
#
####################################################################################
library(FactoMineR)

nbCluster = 3
nbComp = 9

res.pca = PCA(data, scale.unit=TRUE, ncp=nbComp,quali.sup = 63, graph=TRUE)
res.pca$ind

# On doit faire 9 kruskal si on choisi 9 component (on a vérifié et les 9 premiers ont une eigenvalue > 1 -> on en prend 9)
res.pca$eig

res.hcpc = HCPC(res.pca,nb.clust=nbCluster)

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
par(mar=c(4,4,1,1))

boxplot(dataset$Category ~ as.numeric(res.hcpc$data.clust$clust))

table(dataset$Category,as.numeric(res.hcpc$data.clust$clust))
kruskal.test(dataset$Category ~ as.numeric(res.hcpc$data.clust$clust))
# Une petite P-value montre une différence significative -> après on utilise un post kruskal pour savoir où est la différence


boxplot(dataset$Acidez ~ as.numeric(res.hcpc$data.clust$clust))
table(dataset$Acidez,as.numeric(res.hcpc$data.clust$clust))
kruskal.test(dataset$Acidez ~ as.numeric(res.hcpc$data.clust$clust))

kruskal.test(res.pca$ind$coord[,1] ~ as.numeric(res.hcpc$data.clust$clust))

# Inertia gain -> testing the optimal nb of cluster 
barplot(sort(res.hcpc$call$t$inert.gain ,decreasing=T)[1:20],names.arg=1:20,col="lightseagreen",main="Select the number of Cluster",xlab="Number of cluster",ylab="height") 

plot(res.hcpc, axes = c(1,2), choice = "3D.map", 
     draw.tree = TRUE, ind.names = TRUE, title = NULL,
     tree.barplot = TRUE, centers.plot = TRUE)
plot(res.hcpc, choice ="tree", cex = 0.6)
plot(res.hcpc, axes = c(2,3), choice ="map", draw.tree = FALSE)

boxplot(dataset$Dulzor ~ as.numeric(res.hcpc$data.clust$clust))
table(dataset$Dulzor,as.numeric(res.hcpc$data.clust$clust))
kruskal.test(dataset$Dulzor,as.numeric(res.hcpc$data.clust$clust))


boxplot(dataset$PuntajeTotal ~ as.numeric(res.hcpc$data.clust$clust))
table(dataset$PuntajeTotal,as.numeric(res.hcpc$data.clust$clust))
kruskal.test(dataset$PuntajeTotal,as.numeric(res.hcpc$data.clust$clust))

# à mettre dans le rapport: on a essayé avec des méthodes de clustering (ci-dessus avec la catégorie et ci-dessous avec d'autres variables)
# ça ne donne rien de visible -> on utilisera pas les clusters comme variables suplémentaires

####################################################################################
# Multiple Comparison Test After Kruskal-Wallis
# https://www.rdocumentation.org/packages/pgirmess/versions/1.6.5/topics/kruskalmc
# 
####################################################################################



# Variable importance by cluster -> p-value (The lower it is, the higher the importance of the variable)
res.hcpc$desc.var


library(pgirmess)

# Diff entre clusters
kruskalmc(dataset$Category, res.hcpc$data.clust$clust, probs = 0.05, cont=NULL)
kruskalmc(dataset$Acidez, res.hcpc$data.clust$clust, probs = 0.05, cont=NULL)
kruskalmc(dataset$Dulzor, res.hcpc$data.clust$clust, probs = 0.05, cont=NULL)
kruskalmc(dataset$PuntajeTotal, res.hcpc$data.clust$clust, probs = 0.05, cont=NULL)








