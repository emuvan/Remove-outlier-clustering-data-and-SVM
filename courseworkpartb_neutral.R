neuralshrek <-X5parameter_exchange


normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}  


narmalshrek <- as.data.frame(lapply(neuralshrek, normalize))


data_train <- narmalshrek[1:396,]

data_test <- narmalshrek[397:495,]


set.seed(1000)

#developing a model for neuralnet

data_model <- neuralnet(output ~ one + two + three + four + five, hidden = 1, data = data_train)
plot(data_model)

#give out the results of the trained model
data_results <- compute(data_model, data_test[1:5])

#getting the predicted results from the trained results
predicted_outcome <- data_results$net.result

#comparing the predicted outcome and the test dataset
cor(predicted_outcome, data_test$output)

#declaring the desired output for the training and the tested dataset
data_train_output<-neuralshrek[1:397,]$output
data_test_output<-neuralshrek[398:496,]$output

#finding the maxinum and the mininum values
#using these values to unnormalise the predicted output
data_min <- min(data_train_output)
data_max <- max(data_train_output)
                    
#unnormalising the dataset
unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

#renormalising the dataset again
new_pred <- unnormalize(predicted_outcome,data_min,data_max)

#declaring the RMSE functions
rmse <- function(error)
{
  sqrt(mean(error^2))
}

#finding the performance index by using the RSME method
error <- (data_test_output - new_pred )
pred_RMSE <- rmse(error)  
pred_RMSE     

# showing the neutral desire data and the new prediced dataset are matching up
par(mfrow=c(1,1))
plot(data_test_output, new_pred ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN', pch=18,col='red', bty='n')


#declaring the MAE functions
mae <- function(error)
{
  mean(abs(error))
}

#running the mae function
pred_mae <- mae(error)
pred_mae

#creating The MAPE function
mape <- function(x, y)
{
  (100/length(x)) * sum((x + y)/x)
}

#testing mape function
pred_mape <- mape(data_test_output,new_pred)
pred_mape






