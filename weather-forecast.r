install.packages("rwunderground")
install.packages("devtools")
rwunderground::set_api_key("8808b676dd1bd44f")
library("rwunderground")
history(set_location(territory="Malaysia", city="Alor Gajah"), date=20171215) #check the data inside the json file
library("rwunderground")
set.seed(500)
install.packages("rvest")
require("rvest")


#check for missing values
apply(data,2,function(x) sum(is.na(x)))

#check if the variable can be the factor or not
sapply(data,function(x) is.factor(x))







#new testing

library("rwunderground")
library("devtools")
require("rvest")
set.seed(500)
rwunderground::set_api_key("8808b676dd1bd44f") #setting api key for wunderground
data <- history(set_location(territory="Malaysia", city="Alor Gajah"), date=20171215) #set the data using history data
library("ggplot2")
library("neuralnet")
index <- sample(1:nrow(data), round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
history(set_location(territory="Malaysia", city="Alor Gajah"), date=20171215) #check the data
lm.fit <- glm(hum~., data=train)
apply(data,2,function(x) sum(is.na(x)))
sapply(data,function(x) is.factor(x))   


#new feed 2

library("rwunderground")
rwunderground::set_api_key("8808b676dd1bd44f")
data<- conditions(set_location(territory = "Malaysia", city = "Alor Gajah"))
library("ggplot2")
library("neuralnet")
index <- sample(1:nrow(data), round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(weather~., data=train)

 data<-history_range(set_location(territory = "Malaysia", city = "Alor Gajah"