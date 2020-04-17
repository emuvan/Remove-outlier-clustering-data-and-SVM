library(NbClust)
set.seed(1000)
datashrek <-scale(whitewine[-12])
nbshrek <- NbClust(datashrek, min.nc = 2, max.nc = 7, method = "kmeans")

barplot(table(nbshrek$Best.n[1,]),
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters in nbshrek")
 



