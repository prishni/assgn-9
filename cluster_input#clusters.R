library(e1071) #svm library
library(caret)
library(ggplot2) #qplot library


df <- read.csv("C:/Users/acer/Desktop/assgn 9/household_power_consumption.txt", sep=";", stringsAsFactors=FALSE)

#df = df[,-c(1,2)]

#replace all na values by 0 in the data frame
for (i in 1:ncol(df)){
  if(!is.numeric(df[,i])) {
    df[,i] = as.numeric(as.character(df[,i]))
  }
  df[is.na(df[,i]),i ] = 0
}

cols = as.numeric(strsplit(readline("Select columns: "), ",", fixed = T)[[1]])
clusters = as.numeric(readline("Enter the no. of clusters: "))

df= df[,cols] #Select cols specified by user


#Run kmeans algorithm
start = Sys.time()
K =kmeans(df,clusters)
K
