outshrek<-boxplot(whitewine$`fixed acidity`)$out
whitewine<-whitewine[-which(whitewine$`fixed acidity` %in% outshrek),]

outshrek2<-boxplot(whitewine$`volatile acidity`)$out
whitewine<-whitewine[-which(whitewine$`volatile acidity` %in% outshrek2),]

outshrek3<-boxplot(whitewine$`citric acid`)$out
whitewine<-whitewine[-which(whitewine$`citric acid` %in% outshrek3),]

outshrek4<-boxplot(whitewine$`residual sugar`)$out
whitewine<-whitewine[-which(whitewine$`residual sugar` %in% outshrek4),]

outshrek5<-boxplot(whitewine$chlorides)$out
whitewine<-whitewine[-which(whitewine$chlorides %in% outshrek5),]

outshrek6<-boxplot(whitewine$`free sulfur dioxide`)$out
whitewine<-whitewine[-which(whitewine$`free sulfur dioxide` %in% outshrek6),]

outshrek7<-boxplot(whitewine$`total sulfur dioxide`)$out
whitewine<-whitewine[-which(whitewine$`total sulfur dioxide` %in% outshrek7),]

outshrek8<-boxplot(whitewine$pH)$out
whitewine<-whitewine[-which(whitewine$pH %in% outshrek8),]

outshrek9<-boxplot(whitewine$sulphates)$out
whitewine<-whitewine[-which(whitewine$sulphates %in% outshrek9),]




data_shrek<-scale(whitewine[-12])
set.seed(100)
cluster2<-kmeans(data_shrek, 2, nstart = 20)
cluster3<-kmeans(data_shrek, 3, nstart = 20)
cluster4<-kmeans(data_shrek, 4, nstart = 20)
cluster5<-kmeans(data_shrek, 5, nstart = 20)
cluster6<-kmeans(data_shrek, 6, nstart = 20)
cluster7<-kmeans(data_shrek, 7, nstart = 20)
cluster8<-kmeans(data_shrek, 8, nstart = 20)
cluster9<-kmeans(data_shrek, 9, nstart = 20)
table(whitewine$quality, cluster2$cluster)
table(whitewine$quality, cluster3$cluster)
table(whitewine$quality, cluster4$cluster)
table(whitewine$quality, cluster5$cluster)
table(whitewine$quality, cluster6$cluster)
table(whitewine$quality, cluster7$cluster)
table(whitewine$quality, cluster8$cluster)
table(whitewine$quality, cluster9$cluster)

