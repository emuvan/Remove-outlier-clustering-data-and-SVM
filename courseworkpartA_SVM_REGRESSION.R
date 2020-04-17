install.packages("e1071")

library("e1071")

dataset <-SVM_input3

#dividing dataset into two 
data_train <- dataset[1:397,]
data_test <- dataset[398:497,]

#creating linear model
linear_model <- lm(output ~ ., data = data_train)

#creating test prediction with linear model
linear_pred <- predict(linear_model, data_test[1:3])

#making the graph based on desired output and predicted output for linear model
par(mfrow=c(1,1))
plot(data_test$output, linear_pred, col='red',main='Real vs predicted Linear regression',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='Linear', pch=18,col='red', bty='n')

#searching rmse for linear prediction
rmse <- function(error)
{
  sqrt(mean(error^2))
}
linear_RMSE <- rmse(linear_model$residuals)
linear_RMSE

#searching mse for linear prediction
mse <- function(x, y){
  mean((x - y)^2)
}
linear_MSE <- mse(data_test$output, linear_pred)
linear_MSE

#finding mape for linear prediction
mape <- function(x, y)
{
  (100/length(x)) * sum((x - y)/x)
}
linear_mape <- mape(data_test$output, linear_pred)
linear_mape

#creating svm model
svm_model <- svm(output ~ ., data = data_train)

#declaring test prediction for svm model
svm_Pred <- predict(svm_model, data_test[1:3])

#making the graph based on desired output and predicted output for svm model
par(mfrow=c(1,1))
plot(data_test$output, svm_Pred, col='red',main='Real vs predicted SVM',pch=18,cex=0.7)
abline(0,1,lwd=3)
legend('bottomright',legend='SVM', pch=18,col='red', bty='n')

#finding rmse for svm prediction
svm_RSME <- rmse(svm_model$residuals)
svm_RSME

#finding mse for svm prediction
svm_MSE <- mse(data_test$output, svm_Pred)
svm_MSE

#finding mape for svm prediction
svm_MAPE <- mape(data_test$output, svm_Pred)
svm_MAPE

#checking svm model results to get cost & epsilon values
summary(svm_model)

#tuning the svm model to get better results
svm_Tune <- tune(svm, output ~ ., data = data_train, ranges = list(epsilon=seq(0,1,0.01), cost=2^(2:9)))

#creating graph based on tuning results
plot(svm_Tune)

 #getting best model from tuning
svm_best <- svm_Tune$best.model

#getting test prediction from best result
svm_best_Pred <- predict(svm_best, data_test[1:3])

#finding rmse for best svm prediction to compare with previous results
svm_best_rmse <- rmse(data_test$output - svm_best_Pred)
svm_best_rmse

#tuning deeper to get better results. epsilon and cost values are taken from plot(svm_tune) done above
svm_tune <- tune(svm, output ~ ., data = data_train, ranges = list(epsilon=seq(0,0.2,0.005), cost=seq(2,100,2)))
plot(svm_tune)

