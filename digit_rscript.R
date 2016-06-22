data<- read.csv("train.csv",header=TRUE)
data_feature<-data[,2:785]
data_n<-data_feature
train_data <- data_n[1:41000,]
test_data <-data_n[41001:42000,]
train_data_class <- data[1:41000,1]
test_data_class <- data[41001:42000,1]
require(class)
m2 <- knn(train_data,test_data,train_data_class,k=195)
x<-table(test_data_class,m2)
sum_diag <- sum(diag(x))
sum<-sum(x)
error <- 1 - sum_diag/sum
cat("Error : ",error*100,"%")