set.seed(1000)
datavan <- scale(whitewine[-12])

k= 1:7

WSS = sapply(k, function(k) {kmeans(datavan, centers=k)$tot.withinss})
plot(k, WSS, type="l", xlab= "Number of k", ylab="Within sum of squares")

