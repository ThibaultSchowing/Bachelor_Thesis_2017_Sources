# http://factominer.free.fr/docs/code_mca.r

install.packages('FactoMineR')
library(FactoMineR)
install.packages('devtools')
library("devtools")
install_github("kassambara/factoextra")
library("factoextra")




data = read.csv("C:/Users/thsch/Desktop/Bachelor_Thesis_2017_Sources/Notebook/DataAnalysis/data/DataRisaralda_v2Numeric_Complete_class_utf-8.csv", 
                header=T,
                row.names = 1,
                sep=",", 
                stringsAsFactors = TRUE)

summary(data)

res = PCA(data, graph=TRUE)
dimdesc(res)



# Subsample with soil data
subsetdata <- subset(data, select=c(PrecTotalAvg:DtrTotalAvg,ASNM,PuntajeTotal,DefectosTotales), year == '2011')
res = PCA(subsetdata, graph=TRUE)

dimdesc(res)
res
res$var
res$eig



