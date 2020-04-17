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