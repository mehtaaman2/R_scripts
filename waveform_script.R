data<- read.csv("waveform.csv",header=FALSE)
normalize <- function(x)
  (
    return((x-min(x))/(max(x)-min(x)))
  )
data_feature<-data[,1:21]
data_n <- as.data.frame(lapply(data_feature[,1:21],normalize))
train_data <- data_n[1:4499,]
test_data <-data_n[4500:5000,]
train_data_class <- data[1:4499,22]
test_data_class <- data[4500:5000,22]
require(class)
error<- NULL
for(j in 1:200)
{
  m1 <- knn(train=train_data,test=test_data,cl=train_data_class,k=j)
  x<-table(test_data_class,m1)
  sum_diag <- sum(diag(x))
  sum<-sum(x)
  error <- c(error,1 - sum_diag/sum)
}
cat("minimum error at k=",which.min(error)," and the minimum value is : ",min(error)*100,"%")
y<-(1:length(error))
plot(y,error)