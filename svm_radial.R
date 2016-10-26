library(e1071)
library(caret)
library(ggplot2)
df <- read.csv("C:/Users/acer/Desktop/assgn 9/bank.csv", sep=";", stringsAsFactors=T)

set.seed(7)
df = df[,-c(10,11)]

train_data = df[1:3700,]
test_data = df[3701:nrow(df),]

#formula1 = y ~.

#tunesvm1 = tunesvm(svm,y ~.,data = df ,kernel = "linear",range = list(cost = seq(0.01, 1, by = 0.5),gamma = seq(0.1, 1 , by = 0.1)))
svmfit = svm(y ~.,data = train_data ,kernel = "radial",na.rm = T,cost = 1,gamma = 0.5)

#save ur model so that u can load it later on instead of training that again
save(svmfit, file = "svmfit.RData")

#
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

