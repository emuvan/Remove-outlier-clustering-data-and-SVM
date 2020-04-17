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
