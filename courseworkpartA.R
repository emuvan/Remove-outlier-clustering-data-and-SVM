shrek <- whitewine$`fixed acidity`
out1 <- boxplot(shrek, range = 6)$out
whitewine<-whitewine[-which(whitewine$`fixed acidity` %in% out1),] 

donkey <- whitewine$`volatile acidity`
out2 <- boxplot(donkey, range = 6)$out
whitewine<-whitewine[-which(whitewine$`volatile acidity` %in% out2),] 

fiona <- whitewine$`citric acid`
out3 <- boxplot(fiona, range = 6)$out
whitewine <-whitewine[-which(whitewine$`citric acid` %in% out3),]

puss <- whitewine$chlorides
out4 <- boxplot(puss, range = 6)$out
whitewine <-whitewine[-which(whitewine$chlorides %in% out4),]

charming <- whitewine$`free sulfur dioxide`
out5 <- boxplot(charming, range = 6)$out
whitewine <-whitewine[-which(whitewine$`free sulfur dioxide` %in% out5),]

datashrek <-scale(whitewine[-12])
set.seed(100)
clustershrek<-kmeans(datashrek,2, nstart = 25)
table(whitewine$quality, clustershrek$cluster)

datadonkey <-scale(whitewine[-12])
set.seed(100)
clusterdonkey<-kmeans(datadonkey,3, nstart = 25)
table(whitewine$quality, clusterdonkey$cluster)

datafiona <-scale(whitewine[-12])
set.seed(100)
clusterfiona<-kmeans(datafiona,4, nstart = 25)
table(whitewine$quality, clusterfiona$cluster)

datapuss <-scale(whitewine[-12])
set.seed(100)
clusterpuss<-kmeans(datapuss,5, nstart = 25)
table(whitewine$quality, clusterpuss$cluster)

datacharming <-scale(whitewine[-12])
set.seed(100)
clustercharming<-kmeans(datacharming,6, nstart = 25)
table(whitewine$quality, clustercharming$cluster)


