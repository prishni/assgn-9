library(e1071) #svm library
library(caret)
library(ggplot2) #qplot library


df <- read.csv("C:/Users/acer/Desktop/assgn 9/household_power_consumption.txt", header=FALSE, sep=";", stringsAsFactors=FALSE)

#replace all na values by 0 in the data frame
for (i in 1:ncol(df)){
  df[is.na(df[,i]),i ] = 0
}

#creating 2 vectors of size 20 containing all zeros
ss = rep(0,20)
time = rep(0,20)

#applying kmeans 20 times
for (i in 1:20){
  start = Sys.time()
  K =kmeans(df,i)
  ss[i] = K$tot.withinss
  time[i] = Sys.time() - start
  
}
vec = c(1:20)
qplot(vec,ss)
qplot(vec,time)
