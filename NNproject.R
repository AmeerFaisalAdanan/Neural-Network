# Import and install packages
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(gmodels))
suppressPackageStartupMessages(library(lattice))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(Kmisc))
suppressPackageStartupMessages(library(ROCR))
suppressPackageStartupMessages(library(corrplot))

#set.seed (starting point used in generation of random numbers)
set.seed(1023)
weather_data <- read.csv(url("https://www.biz.uiowa.edu/faculty/jledolter/datamining/weather.csv"), header = TRUE, sep = ",", stringsAsFactors = TRUE)
kable(head(weather_data));
colnames(weather_data)#display colnames
str(weather_data)#show data(numerical + categorical variables)
(n <- nrow(weather_data))
c(as.character(weather_data$Date[1]), as.character(weather_data$Date[n]))
all.equal(weather_data$RISK_MM > 1, weather_data$RainTomorrow == "Yes")
all.equal(weather_data$Rainfall > 1, weather_data$RainToday == "Yes")
#reduce redundant data
weather_data2 <- subset(weather_data, select = -c(Date, Location, RISK_MM, Rainfall, RainToday))
colnames(weather_data2)
(cols_withNa <- apply(weather_data2, 2, function(x) sum(is.na(x))))
weather_data3 <- weather_data2[complete.cases(weather_data2),]
#do chi square test
factor_vars <- names(which(sapply(weather_data3, class) == "factor"))
factor_vars <- setdiff(factor_vars, "RainTomorrow")
chisq_test_res <- lapply(factor_vars, function(x) { 
  chisq.test(weather_data3[,x], weather_data3[, "RainTomorrow"], simulate.p.value = TRUE)
})
names(chisq_test_res) <- factor_vars
chisq_test_res
#bar chart for WindGustDir (according to time)
barchart_res <- lapply(factor_vars, function(x) { 
  title <- colnames(weather_data3[,x, drop=FALSE])
  wgd <- CrossTable(weather_data3[,x], weather_data3$RainTomorrow, prop.chisq=F)
  barchart(wgd$prop.row, stack=F, auto.key=list(rectangles = TRUE, space = "top", title = title))
})
names(barchart_res) <- factor_vars
barchart_res$WindGustDir
barchart_res$WindDir9am
barchart_res$WindDir3pm
weather_data4 <- subset(weather_data2, select = -c(WindDir9am, WindDir3pm))
weather_data5 <- weather_data4[complete.cases(weather_data4),]
colnames(weather_data5)#16 attributes collected, 1 categorcal variable (WindGustDir) and 15 quatitative variable
factor_vars <- names(which(sapply(weather_data5, class) == "factor"))
numeric_vars <- setdiff(colnames(weather_data5), factor_vars)
numeric_vars <- setdiff(numeric_vars, "RainTomorrow")
numeric_vars
numeric_vars_mat <- as.matrix(weather_data5[, numeric_vars, drop=FALSE])
numeric_vars_cor <- cor(numeric_vars_mat)
#correlation plot (refer figure 1)
corrplot(numeric_vars_cor)
pairs(weather_data5[,numeric_vars], col=weather_data5$RainTomorrow)
gp <- invisible(lapply(numeric_vars, function(x) { 
  ggplot(data=weather_data5, aes(x= RainTomorrow, y=eval(parse(text=x)), col = RainTomorrow)) + geom_boxplot() + xlab("RainTomorrow") + ylab(x) + ggtitle("") + theme(legend.position="none")}))
grob_plots <- invisible(lapply(chunk(1, length(gp), 4), function(x) {
  marrangeGrob(grobs=lapply(gp[x], ggplotGrob), nrow=2, ncol=2)}))
grob_plots
gp <- invisible(lapply(numeric_vars, function(x) { 
  ggplot(data=weather_data5, aes(x=eval(parse(text=x)), col = RainTomorrow)) + geom_density() + xlab(x) + ggtitle(paste(x, "density", sep= " "))}))
grob_plots <- invisible(lapply(chunk(1, length(gp), 4), function(x) {
  marrangeGrob(grobs=lapply(gp[x], ggplotGrob), nrow=2, ncol=2)}))
grob_plots
#significativeness of a predictor by looking how much are overlapping the 
#predictor values set depending on the variable to be predicted (RainTomorrow).
write.csv(weather_data5, file="weather_data5.csv", sep=",", row.names=FALSE)
#Humidity3pm, Pressure9am, Pressure3pm, Cloud9am, Cloud3pm and Sunshine 
#are predictors to be considered.


#We are going to produce:
#tomorrow's weather forecast at 9am of the current day
#tomorrow's weather forecast at 3pm of the current day
#tomorrow's weather forecast at late evening time of the current day

#specific subsets of variables
#9am: MinTemp, WindSpeed9am, Humidity9am, Pressure9am, Cloud9am, Temp9am
#3pm: (9am variables) + Humidity3pm, Pressure3pm, Cloud3pm, Temp3pm, MaxTemp
#evening: (3pm variables) + Evaporation, Sunshine, WindGustDir, WindGustSpeed
weather_data5 <- read.csv("weather_data5.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
colnames(weather_data5)
nrow(weather_data5)
sum(weather_data5["RainTomorrow"] == "Yes")
sum(weather_data5["RainTomorrow"] == "No")
#70% for training, 30% for testing
train_rec <- createDataPartition(weather_data5$RainTomorrow, p = 0.7, list = FALSE)
training <- weather_data5[train_rec,]
testing <- weather_data5[-train_rec,]
sum(training["RainTomorrow"] == "Yes")/sum(training["RainTomorrow"] == "No")
sum(testing["RainTomorrow"] == "Yes")/sum(testing["RainTomorrow"] == "No")

#9am forecast Model
#k fold cross validation
trControl <- trainControl(method = "repeatedcv",  repeats = 5, number = 10, verboseIter = FALSE)
predictors_9am_c6 <- c("Cloud9am",  "Humidity9am", "Pressure9am", "Temp9am")
formula_9am_c1 <- as.formula(paste("RainTomorrow", paste(predictors_9am_c6, collapse="+"), sep="~"))
mod9am_c1_fit <- train(formula_9am_c1,  data=training, method="glm", 
                       family="binomial", trControl = trControl, metric = 'Accuracy')
mod9am_c1_fit$results$Accuracy
(summary_rep <- summary(mod9am_c1_fit$finalModel))
1 - pchisq(summary_rep$deviance, summary_rep$df.residual)
drop1(mod9am_c1_fit$finalModel, test="Chisq")
predictors_9am_c2 <- c("Cloud9am",  "Humidity9am", "Pressure9am", "MinTemp")
formula_9am_c2 <- as.formula(paste("RainTomorrow", paste(predictors_9am_c2, collapse="+"), sep="~"))
mod9am_c2_fit <- train(formula_9am_c2,  data=training, method="glm",family="binomial", trControl = trControl, metric = 'Accuracy')
mod9am_c2_fit$results$Accuracy                       
(summary_rep <- summary(mod9am_c2_fit$finalModel))                       
1 - pchisq(summary_rep$deviance, summary_rep$df.residual)
mod9am_pred <- predict(mod9am_c1_fit, testing)
confusionMatrix(mod9am_pred, testing[,"RainTomorrow"])
