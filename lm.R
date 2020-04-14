



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

train_lm <- lm( x~., train)
sum_train_lm <- summary( train_lm)

# p.val < 0.05
important_col_bool <- sum_train_lm$coefficients[-1,4] < 0.05
#plus label
important_col_bool <- c( important_col_bool, TRUE)
train_cut <- train[,important_col_bool]

#train on cuted
train_cut_lm <- lm( x~., train_cut)
summary(train_cut_lm)

#predict and plot values
pred_train_cut <- predict(  train_cut_lm, test[,important_col_bool])
plot( sort(pred_train_cut))

#looking for best cut
#library(ROCR)
#pred <- prediction( pred_train_cut, test$x)
#perf <- performance(pred,"tpr","fpr")
#plot( perf,colorize=TRUE)

#metric
accuracy <- function( table_in){
  sum( diag( table_in)) / sum( table_in)
}

acc <- rep(0, 21)

for( i in 0:20){
  predicted_lab <- ifelse( pred_train_cut > i*0.05, 1, 0)
  acc[i] <- accuracy( table( test$x, predicted_lab))
}

best_split <- which.max( acc) *0.05
best_split

predicted_lab <- ifelse( pred_train_cut > best_split, 1, 0)

#best split
tab <-table( test$x, predicted_lab)
accuracy( tab)
tab



