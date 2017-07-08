
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



# Self Organising Maps

rm(list=ls())
path <- "C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/DataAnalysis/data/"
pathSOM <- "C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/R/Projects/BachelorThesis/SOM/"
setwd(path)

filename <- "DataRisaralda_v2Numeric_Complete_utf-8.csv"

data = read.csv(filename, header=T, sep=",")


# Retirer la variable SICA qui n'a pas de sens numériquement parlant
to.remove <- c("SICA")
`%ni%` <- Negate(`%in%`)
data = subset(data,select = names(data) %ni% to.remove)


setwd(pathSOM)

summary(data)


# Load the kohonen package 
install.packages('kohonen')
require(kohonen)

## 75% of the sample size
#smp_size <- floor(0.75 * nrow(data))

## set the seed to make your partition reproductible
#set.seed(123)
#train_ind <- sample(seq_len(nrow(data)), size = smp_size)

#data_train <- data[train_ind, ]
#data_test <- data[-train_ind, ]


# Change the data frame with training data to a matrix
# Also center and scale all variables to give them equal importance during
# the SOM training process. 
data_matrix <- as.matrix(scale(data))
#data_matrix_2011 <- as.matrix(scale(subset(data[,-which( names(data) %in% c("year","Cristalizado","Reposado"))], year==2011)))
data_matrix_2011 <- as.matrix(scale(subset(data, year==2011), scale=FALSE))
data_matrix_2016 <- as.matrix(scale(subset(data, year==2016), scale=FALSE))

pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#99000b', '#99FF00', '#FFFF44', '#00caca','#beef00','#00beef', '#cacabe')
coolBlueHotRed <- function(n, alpha = 1) { rainbow(n, end=4/6, alpha=alpha)[n:1] }








#################### All #######################


# Create the SOM Grid - you generally have to specify the size of the 
# training grid prior to training the SOM. Hexagonal and Circular 
# topologies are possible
#som_grid <- somgrid(xdim = 25, ydim=25, topo='hexagonal')

# Finally, train the SOM, options for the number of iterations,
# the learning rates, and the neighbourhood are available
#som_model <- som(data_matrix, 
#                 grid=som_grid, 
#                 rlen=100, 
#                 alpha=c(0.05,0.01), 
#                 keep.data = TRUE)

# Plotthe map
#png(paste("All_Dist_neighbours.png"), width=4, height=4, units="in", res=300)
#par(mar=c(4,4,1,1))
#plot(som_model, type="dist.neighbours")
#dev.off()


#k = 5
#print(dim(data))



#wss <- (nrow(data_matrix)-1)*sum(apply(data_matrix,2,var)) 
#for (i in 2:15) {
#  wss[i] <- sum(kmeans(data_matrix, centers=i)$withinss)
#}
#plot(wss)
## use hierarchical clustering to cluster the codebook vectors
#som_cluster <- cutree(hclust(dist(som_model$codes)), k)
#som.hc <- cutree(hclust(dist(sommap$codes[[1]])), groups)
# plot these results:



#png(paste("All_cluster.png"), width=4, height=4, units="in", res=300)
#par(mar=c(4,4,1,1))
#plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
#add.cluster.boundaries(som_model, som_cluster)
#dev.off()

#sprintf("Clustering with  %d clusters",k)







####################2011#######################


# SOM MAP

data_som <- data_matrix_2011
summary(data_som)

som_grid <- somgrid(xdim = 15, ydim=20, topo='hexagonal')

som_model2 <- som(data_som, 
                 grid=som_grid, 
                 rlen=100, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE)


# Som Clusters

k= 5
som_cluster <- cutree(hclust(dist(som_model2$codes)), k)

png(paste("2011_cluster.png"), width=4, height=4, units="in", res=300)
par(mar=c(4,4,1,1))
plot(som_model2, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model2, som_cluster)
dev.off()


# Distances

png(paste("2011_distance.png"), width=4, height=4, units="in", res=300)
par(mar=c(4,4,1,1))
plot(som_model2, type="dist.neighbours")
dev.off()


# Plot By variables

variable = c("PrecTotalAvg","TminTotalAvg","TmaxTotalAvg","TmeanTotalAvg","DtrTotalAvg","PuntajeTotal", "Acidez", "Dulzor","DefectosTotales","ASNM","pH_avg", "org_avg","Arenoso", "Franco")

#variable = c("year","DefectosTotales", "ASNM","Luminosidad","prec1","prec2","prec3","prec4", "prec5","prec6","prec7","prec8","prec9","prec10","tmin1","tmin2","tmin3",
#             "tmin4","tmin5","tmin6","tmin7","tmin8","tmin9","tmin10","tmax1","tmax2", "tmax3","tmax4","tmax5","tmax6","tmax7","tmax8","tmax9","tmax10","tmean1"         
#             ,"tmean2","tmean3","tmean4","tmean5","tmean6","tmean7","tmean8","tmean9","tmean10", "dtr1","dtr2","dtr3","dtr4","dtr5","dtr6","dtr7","dtr8","dtr9" , "dtr10",
#             "PrecTotal","TminTotal","TmaxTotal","TmeanTotal","DtrTotal","PrecTotalAvg","TminTotalAvg","TmaxTotalAvg" ,"TmeanTotalAvg","DtrTotalAvg","OrientationNum",
#             "Slope","pH_avg","org_avg","Franco","Arcilloso","Limoso"  ,"Arenoso","Cascajoso","Aroma.Fragancia", "Acidez","Cuerpo","Sabor","SaborResidual" ,  "Dulzor",
#             "Uniformidad" ,"Balance","TazaLimpia","PuntajeCatador" , "PuntajeTotal" ,   "Category" )




for(var_str in variable) {
  # get the index from the name
  var <- grep(var_str, colnames(data_som))
  var_unscaled <- aggregate(as.numeric(data_som[,var]), by=list(som_model2$unit.classif), FUN=mean, simplify=TRUE)[,2] 
  
  
  # Saving the plots
  png(paste("2011_var_",var_str,".png"), width=4, height=4, units="in", res=300)
  par(mar=c(4,4,1,1))
  
  plot(som_model2, type = "property", property=var_unscaled, main=names(data)[var], palette.name=coolBlueHotRed)
  #plot(fit)
  dev.off()
}

###################2016########################


# SOM MAP

data_som <- data_matrix_2016

som_grid <- somgrid(xdim = 15, ydim=15, topo='hexagonal')

som_model2 <- som(data_som, 
                  grid=som_grid, 
                  rlen=100, 
                  alpha=c(0.05,0.01), 
                  toroidal = TRUE,
                  keep.data = TRUE)

# Som Clusters

k= 5
som_cluster <- cutree(hclust(dist(som_model2$codes)), k)

png(paste("2016_cluster.png"), width=4, height=4, units="in", res=300)
par(mar=c(4,4,1,1))
plot(som_model2, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model2, som_cluster)
dev.off()


# Distances

png(paste("2016_distance.png"), width=4, height=4, units="in", res=300)
par(mar=c(4,4,1,1))
plot(som_model2, type="dist.neighbours")
dev.off()

# By variables

variable = c("PrecTotalAvg","TminTotalAvg","TmaxTotalAvg","TmeanTotalAvg","DtrTotalAvg","pH_avg","org_avg","ASNM","PuntajeTotal","DefectosTotales")
variable = c("PrecTotalAvg","TminTotalAvg","TmaxTotalAvg","TmeanTotalAvg","DtrTotalAvg","PuntajeTotal", "Acidez", "Dulzor","DefectosTotales","ASNM","pH_avg", "org_avg","Arenoso", "Franco")

for(var_str in variable) {
  # get the index from the name
  var <- grep(var_str, colnames(data_som))
  var_unscaled <- aggregate(as.numeric(data_som[,var]), by=list(som_model2$unit.classif), FUN=mean, simplify=TRUE)[,2]
  
  
  # Saving the plots
  png(paste("2016_var_",var_str,".png",sep=""), width=4, height=4, units="in", res=300)
  par(mar=c(4,4,1,1))
  plot(som_model2, type = "property", property=var_unscaled, main=names(data)[var], palette.name=coolBlueHotRed)
  
  dev.off()
}



#========================================================================================
# SOM avec dataset complet numérique 


setwd(path)
filename <- "DataRisaralda_v2Numeric_Complete_utf-8.csv"

data = read.csv(filename, header=T, sep=",")
data <- data[complete.cases(data),]

setwd(pathSOM)


data_matrix <- as.matrix(scale(data))

som_grid <- somgrid(xdim = 25, ydim=25, topo='hexagonal')
data_som <- data_matrix


som_model2 <- som(data_som, 
                  grid=som_grid, 
                  rlen=100, 
                  alpha=c(0.05,0.01), 
                  keep.data = TRUE)


# Som Clusters
k= 5
som_cluster <- cutree(hclust(dist(som_model2$codes)), k)

# Saving the plots
png(paste("AllDataClusters.png"), width=4, height=4, units="in", res=300)
par(mar=c(4,4,1,1))
plot(som_model2, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model2, som_cluster)
dev.off()

# Distances
# Saving the plots
png(paste("AllDataDistances.png"), width=4, height=4, units="in", res=300)
par(mar=c(4,4,1,1))
plot(som_model2, type="dist.neighbours")
dev.off()

variable = c("PrecTotalAvg","TminTotalAvg","TmaxTotalAvg","TmeanTotalAvg","DtrTotalAvg","pH_avg","org_avg","ASNM","Slope","Orientation","PuntajeTotal","DefectosTotales")
variable = c("PrecTotalAvg","TminTotalAvg","TmaxTotalAvg","TmeanTotalAvg","DtrTotalAvg","PuntajeTotal","Slope","Orientation", "Acidez", "Dulzor","DefectosTotales","ASNM","pH_avg", "org_avg","Arenoso", "Franco")

for(var_str in variable) {
  # get the index from the name
  var <- grep(var_str, colnames(data_som))
  var_unscaled <- aggregate(as.numeric(data_som[,var]), by=list(som_model2$unit.classif), FUN=mean, simplify=TRUE)[,2] 
  
  # Saving the plots
  png(paste("AllCompactvar",var_str,".png",sep=""), width=4, height=4, units="in", res=300)
  par(mar=c(4,4,1,1))
  plot(som_model2, type = "property", property=var_unscaled, main=names(data)[var], palette.name=coolBlueHotRed)
  dev.off()
  
}


print('End')



