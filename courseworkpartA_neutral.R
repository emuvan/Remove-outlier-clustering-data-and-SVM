#install packages
install.packages("ggplot2")
install.packages("reshape2")
install.packages("gridExtra")
install.packages("grid")
install.packages("MASS")

neuralshrek <-new_exchange

#creating a normalizing function the dataset I have imported
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}  

#normalizing the data
narmalshrek <- as.data.frame(lapply(neuralshrek, normalize))

#creating training for the dataset
shrek_train <- narmalshrek[1:397,]
#creating for the dataset test
shrek_test <- narmalshrek[398:497,]


#reproducable results
set.seed(1000)

#developing a model for neuralnet

shrek_model <- neuralnet(output ~ one + two + three, hidden = 3, data = shrek_train)
plot(shrek_model)

#give out the results of the trained model
shrek_results <- compute(shrek_model, shrek_test[1:3])

#getting the predicted results from the trained results
predicted_outcome <- shrek_results$net.result

#comparing the predicted outcome and the test dataset
cor(predicted_outcome, shrek_test$output)

#declaring the desired output for the training and the tested dataset
data_train_output<-neuralshrek[1:397,]$output
data_test_output<-neuralshrek[398:497,]$output

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

#testing
pred_mape <- mape(data_test_output,new_pred)
pred_mae

