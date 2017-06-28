# Self Organising Maps


path <- "C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/DataAnalysis/data/"
setwd(path)

filename <- "DataRisaralda_v2Numeric_Complete_utf-8.csv"

data = read.csv(filename, header=T, sep=",")



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


# Create the SOM Grid - you generally have to specify the size of the 
# training grid prior to training the SOM. Hexagonal and Circular 
# topologies are possible
som_grid <- somgrid(xdim = 25, ydim=25, topo='hexagonal')

# Finally, train the SOM, options for the number of iterations,
# the learning rates, and the neighbourhood are available
som_model <- som(data_matrix, 
                 grid=som_grid, 
                 rlen=100, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE)


plot(som_model, type="dist.neighbours")

pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#99000b', '#99FF00', '#FFFF44', '#00caca','#beef00','#00beef', '#cacabe')
coolBlueHotRed <- function(n, alpha = 1) { rainbow(n, end=4/6, alpha=alpha)[n:1] }

k = 5
print(dim(data))



wss <- (nrow(data_matrix)-1)*sum(apply(data_matrix,2,var)) 
for (i in 2:15) {
  wss[i] <- sum(kmeans(data_matrix, centers=i)$withinss)
}
plot(wss)
## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(som_model$codes)), k)
#som.hc <- cutree(hclust(dist(sommap$codes[[1]])), groups)
# plot these results:
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model, som_cluster)


sprintf("Clustering with  %d clusters",k)

####################2011#######################


#
data_som <- data_matrix_2011
summary(data_som)

som_grid <- somgrid(xdim = 10, ydim=10, topo='hexagonal')

som_model2 <- som(data_som, 
                 grid=som_grid, 
                 rlen=100, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE)

# Som Clusters
k= 5
som_cluster <- cutree(hclust(dist(som_model2$codes)), k)
plot(som_model2, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model2, som_cluster)

# Distances
plot(som_model2, type="dist.neighbours")


variable = c("PrecTotalAvg","TminTotalAvg","TmaxTotalAvg","TmeanTotalAvg","DtrTotalAvg","PuntajeTotal","DefectosTotales","ASNM")
for(var_str in variable) {
  # get the index from the name
  var <- grep(var_str, colnames(data_som))
  var_unscaled <- aggregate(as.numeric(data_som[,var]), by=list(som_model2$unit.classif), FUN=mean, simplify=TRUE)[,2] 
  plot(som_model2, type = "property", property=var_unscaled, main=names(data)[var], palette.name=coolBlueHotRed)
  
}

###################2016########################


#
data_som <- data_matrix_2016

som_grid <- somgrid(xdim = 10, ydim=10, topo='hexagonal')

som_model2 <- som(data_som, 
                  grid=som_grid, 
                  rlen=100, 
                  alpha=c(0.05,0.01), 
                  keep.data = TRUE)

# Som Clusters
k= 5
som_cluster <- cutree(hclust(dist(som_model2$codes)), k)
plot(som_model2, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model2, som_cluster)

# Distances
plot(som_model2, type="dist.neighbours")

variable = c("PrecTotalAvg","TminTotalAvg","TmaxTotalAvg","TmeanTotalAvg","DtrTotalAvg","pH_avg","org_avg","ASNM","PuntajeTotal","DefectosTotales")
for(var_str in variable) {
  # get the index from the name
  var <- grep(var_str, colnames(data_som))
  var_unscaled <- aggregate(as.numeric(data_som[,var]), by=list(som_model2$unit.classif), FUN=mean, simplify=TRUE)[,2] 
  plot(som_model2, type = "property", property=var_unscaled, main=names(data)[var], palette.name=coolBlueHotRed)
  
  # Saving the plots
  png(filename="your/file/location/name.png")
  plot(fit)
  dev.off()
}

print('End')



