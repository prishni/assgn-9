
#library for svm
library(e1071)

library(caret)
library(ggplot2)

#import the data frame
df <- read.csv("C:/Users/acer/Desktop/assgn 9/bank-full.csv", sep=";", stringsAsFactors=T)

df = df[,-c(10,11)]

#calculating 70% of the data frame
a = floor(0.7*nrow(df))

r =sample(1:nrow(df),a)

train_data = df[r,]

test_data = df[-r,]
 
#formula1 = y ~.

#tunesvm1 = tunesvm(svm,y ~.,data = df ,kernel = "linear",range = list(cost = seq(0.01, 1, by = 0.5),gamma = seq(0.1, 1 , by = 0.1)))

svmfit = svm(y ~. ,data = train_data ,kernel = "radial",cost = 0.75,gamma = 0.09)

#save ur model so that u can load it later on instead of training that again
save(svmfit, file = "svmfit.RData")

p = predict(svmfit, test_data)

truth = (p == test_data$y)

correct_predicton = 0
incorrect_prediction = 0

#finding number of correctly predicted genre (no. of TRUE entries in vector truth)
for(val in truth){
  if (val == TRUE)
    correct_predicton = correct_predicton+1
  else
    incorrect_prediction =incorrect_prediction +1
}

accuracy = correct_predicton/length(truth)
accuracy

