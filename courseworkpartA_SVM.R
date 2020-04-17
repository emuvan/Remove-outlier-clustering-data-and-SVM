#package for the smv
require(e1071) 

data1 <- SVM_input3

summary(data1)

#structure the data
str(data1)
str(data1$output)

#getting the attribute
x<- subset(data1,select = output)

#selecting the last column and changing it to numeric
y<-as.numeric(data1$output)

str(y)

#classifying data making the putput column as a factor
data_factor<-cbind(x,output=as.factor(y))

str(data_factor)

data_train <- data1[1:397,]

data_test <- data1[398:497,]

x_factor <- subset(data_test,select = -output)
y_factor <- data_test$output

data_svm <- svm(output ~ Inputone + inputtwo + inputthree, data = data_test)
summary(data_svm)

pred <- predict(data_svm,x_factor)
table(pred,y_factor)

1-sum(pred==y_factor)/length(y_factor)

data_svm_tuned <- tune(svm,output~.,  data = data_train,
ranges = list(gamma = seq(.05,.11,.01),cost=seq(1,4,0.5)),
tunecontrol = tune.control(sampling = "cross"))

summary(data_svm_tuned)
plot(data_svm_tuned)

data_svm_tuned$best.parameters

data_svm2 <-svm(output~., data = data_train,gamma=0.07, cost=1.5)
data_predict<-predict(data_svm2,x_factor);
1-sum(data_predict==y_factor)/length(y_factor)

data_numeric<-cbind(x,output=y)
str(data_numeric)

data_train2<-data1[1:397,]
data_test2<-data1[398:497,]

x_factor2<- subset(data_test2,select = -output)
y_factor2<-data_test2$output

data_svm3<-svm(output~., data = data_train2)
summary(data_svm3)

data_factor_predict<- predict(data_svm3,x_factor2);
sqrt(sum((data_svm3$output-
data_factor_predict)^2))/length(data_factor_predict)

data_svm_tuned2 <- tune(svm,output~., data=data_train2,
ranges = list(gamma = seq(.05,.11,.01), cost=seq(1,4,0.5)),
tunecontrol = tune.control(sampling = "cross"))

summary(data_svm_tuned2)
plot(data_svm_tuned2)

