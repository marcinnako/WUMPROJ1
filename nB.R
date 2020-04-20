
library( e1071)

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
data$age <- cut( data$age, breaks = seq( 10, 80, by = 10))

#credit_amount
data$credit_amount <- cut(  data$credit_amount, seq(0, 16000, length.out = 9))

#duration
data$duration <- cut( data$duration, breaks = c(0,12,24,36,48,60,72))

#as.facor
for (column in names(data)) {
  data[,column] <- as.factor( data[,column])
}


n <-which( names( data) =="customer_type")

#the same split
set.seed(3113)
rows <- sample(nrow(data))
num_data <- data[rows, ]

test_data <- head(data,n = 200)
train_data <- tail(data,n = 800)







#metric
accuracy <- function( table_in){
  sum( diag( table_in)) / sum( table_in)
}

f1 <- function( table_in) {
  recall <- table_in[2,2] / sum( table_in[2,])
  precicion <- table_in[2,2] / sum( table_in[,2])
  ( 2*recall*precicion) / (recall + precicion)
}



true_labels <- test_data[,n]

#training | laplance =1 for 0 probability
nB <- naiveBayes( customer_type~. , data = train_data, laplace = 0)
pred_nB_raw <-  predict( nB, test_data[-n], type = "raw")

library( ROCR)
x <- prediction( pred_nB_raw[,1], true_labels)
ROC <- performance( x, "tpr", "fpr")
plot(ROC, col = as.list(1:10))
abline( 0 ,1, col = "blue")

for (i in 1:9) {
  pred_nB <- factor( ifelse( pred_nB_raw[,1] > i/10, "Good", "Bad"), levels = c("Good","Bad"))
  tab <- table( true_labels, pred_nB)
  cat( c( i,  ": "))
  #print(tab)
  cat(accuracy(tab))
  cat("\n \n")
}
#accuracy najlepsze dla 0.5: 0.78

for (i in 1:9) {
  pred_nB <- factor( ifelse( pred_nB_raw[,1] > i/10, "Good", "Bad"), levels = c("Good","Bad"))
  tab <- table( true_labels, pred_nB)
  cat( c( i,  ": "))
  #print(tab)
  cat(f1(tab))
  cat("\n \n")
}
#f1 najlepsze dla 0.7: 0.61

i=7
pred_nB <- factor( ifelse( pred_nB_raw[,1] > i/10, "Good", "Bad"), levels = c("Good","Bad"))
tab <- table( true_labels, pred_nB)
tab










# analiza modelu
library( DataExplorer)
DataExplorer::plot_correlation( data)
#mała korelacja

diff_list <- lapply( nB$tables, function(category){
  n <- dim(category)[2]
  out <- rep(-1,n)
  for (i in 1:n) {
    out[i] <- category[1,i] / category[2,i]
  }
  names(out) <- colnames(category)
  return(out)
})


epsilon <- 2

#how many times is likly to get a credit
diff_vec <- unlist(diff_list)
diff_vec_some <-  diff_vec[ diff_vec > epsilon | 1/ diff_vec > epsilon]
order_vec <- diff_vec_some
order_vec[ order_vec < 1] <- 1/order_vec[ order_vec < 1]
diff_vec_some_ordered <- diff_vec_some[ order( order_vec, decreasing = TRUE)]

case_num <- 80 #/800 cases
cases <- unlist( lapply(train_data[-n], function(column){ table(column)}))
cases_many <- cases[ cases >= case_num]

diff_vec_some_ordered[ names(diff_vec_some_ordered) %in% names(cases_many)]








#to show cases
plot_df <- data.frame( diff_vec, cases)

library( plotly)
library( dplyr)
p <- plot_ly( plot_df,
         x = ~diff_vec,
         y = ~cases,
         type = "scatter",
         text = rownames(plot_df)) %>%
  layout(xaxis = list(type = "log"),
         showlegend = FALSE) %>%
  add_lines( y = case_num) %>%
  add_lines( x = epsilon) %>%
  add_lines( x = 1/epsilon)
p









