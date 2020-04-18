
library( e1071)

#load
x_test <- read.csv( "x_test.csv")[-1]
x_train <- read.csv( "x_train.csv")[-1]
y_test <- read.csv( "y_test.csv")[-1]
y_train <- read.csv( "y_train.csv")[-1]

#metric
accuracy <- function( table_in){
  sum( diag( table_in)) / sum( table_in)
}

#importance
train <- cbind( x_train, y_train)

nB <- naiveBayes( x~. , data = train)
pred_nB_raw <-  predict( nB, x_test, type = "raw")


pred_nB <- ifelse( pred_nB_raw[,1] > pred_nB_raw[,2] * e, 0, 1)

tab <- table( y_test[,1], pred_nB)

accuracy( tab)
tab

