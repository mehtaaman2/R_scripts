data<- read.csv("waveform_with_noise.csv",header=FALSE)
normalize <- function(x)
  (
    return((x-min(x))/(max(x)-min(x)))
  )
data_feature<-data[,1:40]
data_n <- as.data.frame(lapply(data_feature[,1:40],normalize))
train_data <- data_n[1:4499,]
test_data <-data_n[4500:5000,]
train_data_class <- data[1:4499,41]
test_data_class <- data[4500:5000,41]
require(class)
library(class)
error<-NULL
m2 <- knn(train=train_data,test=test_data,cl=train_data_class,k=71)
x<-table(test_data_class,m2)
sum_diag <- sum(diag(x))
sum<-sum(x)
error <- c(error,1 - (sum_diag/sum))
cat("Error : ",error*100,"%")