
library( e1071)

read.csv("german_credit_data_weka_dataset.csv")

data <- read.csv("german_credit_data_weka_dataset.csv")

levels(data[,1]) <- c("low", "fair", "high", "not_have") #DM low<0<fair<200<high
levels(data[,3]) <- c("all_paid", "all_paid_here", "paid_till_now", "delay", "critical")
levels(data[,4]) <- c("new_car", "used_car", "furniture/equipment", "radio/television", "domestic", "repairs", "education", "retraining", "business", "other") #note: 0 for vacation
levels(data[,6]) <- c("low","normal","high","very_high","not_have/unknown") #DM low<100<normal<500<high<1000<very_high
levels(data[,7]) <- c("unemployed", "less_than_year", "1-3_years", "4-6_yeras","7+_years")
levels(data[,9]) <- c("male_d/s", "female_d/s/m", "male_single", "male_m/w") #d = divorsed, s = seperated, m = married, w = widowed ,#note: 0 female single
levels(data[,10]) <- c("none", "co-applicant", "guarantor")
levels(data[,12]) <- c("real_estate", "building_savings", "car", "not_have/unknown")
levels(data[,14]) <- c("bank", "stores", "none")
levels(data[,15]) <- c("rent", "own", "for_free")
levels(data[,17]) <- c("unskilled_non_resident", "unskilled_resident", "skilled_employee", "highly_qualified_employee*") # also management, self-employed, officer
levels(data[,19]) <- c("no", "yes")
levels(data[,20]) <- c("yes", "no")
data[,21] <- as.factor(as.character(data[,21]))
levels(data[,21]) <- c("Good", "Bad")

#age
x_test$age <- cut( x_test$age, breaks = seq( 10, 80, by = 10))
x_train$age <- cut( x_train$age, breaks = seq( 10, 90, by = 10))

#credit_amount
x_test$credit_amount <- cut(  x_test$credit_amount, seq(0, 16000, length.out = 9))
x_train$credit_amount <- cut( x_train$credit_amount, seq(0, 16000, length.out = 9))

#duration
x_test$duration <- cut( x_test$duration, breaks = c(0,12,24,36,48,60,72))
x_train$duration <- cut( x_train$duration, breaks = c(0,12,24,36,48,60,72))

#as.facor
for (column in names(x_test)) {
  x_test[,column] <- as.factor( x_test[,column])
  x_train[,column] <- as.factor( x_train[,column])
}




#the same split
n <- which( names(data) == "customer_type")
X <- data[,-n]
y <- data[,n]

set.seed(3113)
rows <- sample(nrow(num_data))
num_data <- num_data[rows, ]

test_data <- head(num_data,n = 200)
train_data <- tail(num_data,n = 800)







#metric
accuracy <- function( table_in){
  sum( diag( table_in)) / sum( table_in)
}

f1 <- function( table_in) {
  recall <- table_in[2,2] / sum( table_in[2,])
  precicion <- table_in[2,2] / sum( table_in[,2])
  ( 2*recall*precicion) / (recall + precicion)
}


#importance
train <- cbind( x_train, y_train)

#training | laplance =1 for 0 probability
nB <- naiveBayes( x~. , data = train, laplace = 0)
nB
pred_nB_raw <-  predict( nB, x_test, type = "raw")

library( ROCR)
x <- prediction( pred_nB_raw[,2], y_test[,1])
ROC <- performance( x, "tpr", "fpr")
plot(ROC, col = as.list(1:10))
abline( 0 ,1, col = "blue")

for (i in 1:9) {
  pred_nB <- ifelse( pred_nB_raw[,1] > i/10, 0, 1)
  tab <- table( y_test[,1], pred_nB)
  cat( c( i,  ": "))
  #print(tab)
  cat(accuracy(tab))
  cat("\n \n")
}
#accuracy najlepsze dla 0.5: 0.78

for (i in 1:9) {
  pred_nB <- ifelse( pred_nB_raw[,1] > i/10, 0, 1)
  tab <- table( y_test[,1], pred_nB)
  cat( c( i,  ": "))
  #print(tab)
  cat(f1(tab))
  cat("\n \n")
}
#f1 też najlepsze dla 0.5: 0.85


pred_nB <- ifelse( pred_nB_raw[,1] > 0.5, 0, 1)
tab <- table( y_test[,1], pred_nB)
tab







