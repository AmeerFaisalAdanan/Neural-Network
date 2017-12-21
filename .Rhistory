weather_data5 <- read.csv("weather_data5.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
library("neuralnet")
nn<- neuralnet(RainTomorrow ~ weather_data5.MinTemp + weather_data5.MaxTemp+ weather_data5.Evaporation, weather_data5,hidden = c(2,1),learningrate =0.01, algorithm="backprop" )
nn<- neuralnet(RainTomorrow ~  weather_data5.MaxTemp+ weather_data5.Evaporation, weather_data5,hidden = c(2,1),learningrate =0.01, algorithm="backprop" )
head(weather_data5)
nn<- neural =neuralnet(Yes + No ~ weather_data5.Temp9am + weather_data5.Temp3pm + weather_data5.Cloud3pm)
wdata <- weather_data5
head(wdata)
dim(wdata)
fix(wdata)
index <- sample(2,nrow(wdata), replace = TRUE, prob=c(0.7,0.3))
trainset = wdata[index ==1]
trainset = wdata[index ==1,]
testset = wdata[index == 2,]
trainset
testset
dim(trainset)
dim(testset)
library(neuralnet)
trainset$Yes = trainset$RainTomorrow == "Yes"
trainset$No = trainset$RainTomorrow == "No"
#nn = neuralnet(Yes + No ~ MinTemp + WindSpeed9am + Humidity9am + Pressure9am + Cloud9am, trainset, hidden=3 )
nn = neuralnet(Yes + No ~ MinTemp + WindSpeed9am + Humidity9am + Pressure9am + Cloud9am + Temp9am, trainset, hidden=3 )
plot(nn)
nn2 = neuralnet(Yes + No ~ MinTemp + WindSpeed9am + Humidity9am + Pressure9am + Cloud9am + Temp9am, trainset, hidden=6 )


