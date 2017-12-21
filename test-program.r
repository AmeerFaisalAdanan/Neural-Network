# simple ANN execution
#http://www.kdnuggets.com/2016/08/begineers-guide-neural-networks-r.html
head(iris)
# Scaling the data before processing
max <- apply(iris[,1:4],2,max)
min <- apply(iris[,1:4],2,min)

iris_scaled <- scale(iris[,1:4],center = min,scale = max-min)
Species_num <- as.numeric(iris$Species)
data <- cbind(iris_scaled,Species_num)

#split train and test data
library(caret)
trainIndex <- createDataPartition(data[Species_num],p=0.8,list=FALSE,times = 1)
train_data <- data[trainIndex,]
test_data <- data[-trainIndex,]
params <- colnames(iris_scaled)
params <- paste(params,collapse = "+")
f <- paste("Species_num ~ ",params)

# Neural networks
library(neuralnet)
model <- neuralnet(f,data=train_data,hidden=5,linear.output = TRUE)
prediction <- compute(model,test_data[,1:4])
# checking the result
head(prediction$net.result)
pred_rounded <- sapply(prediction$net.result,round,digits=0)
confusionMatrix(test_data[,5],pred_rounded)
plot(model)