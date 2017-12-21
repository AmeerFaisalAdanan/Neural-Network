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


####preprocessing finished####
#Humidity3pm, Pressure9am, Pressure3pm, Cloud9am, Cloud3pm and Sunshine 
#are predictors to be considered.
#We are going to produce:
#tomorrow's weather forecast at 9am of the current day
#tomorrow's weather forecast at 3pm of the current day
#tomorrow's weather forecast at late evening time of the current day
#####This time we are using neural network to predict the forecast event###


eather_data5 <- read.csv("weather_data5.csv", header = TRUE, sep = ",", str$
> head(weather_data5)
  MinTemp MaxTemp Evaporation Sunshine WindGustDir WindGustSpeed WindSpeed9am
1     8.0    24.3         3.4      6.3          NW            30            6
2    14.0    26.9         4.4      9.7         ENE            39            4
3    13.7    23.4         5.8      3.3          NW            85            6
4    13.3    15.5         7.2      9.1          NW            54           30
5     7.6    16.1         5.6     10.6         SSE            50           20
6     6.2    16.9         5.8      8.2          SE            44           20
  WindSpeed3pm Humidity9am Humidity3pm Pressure9am Pressure3pm Cloud9am
1           20          68          29      1019.7      1015.0        7
2           17          80          36      1012.4      1008.4        5
3            6          82          69      1009.5      1007.2        8
4           24          62          56      1005.5      1007.0        2
5           28          68          49      1018.3      1018.5        7
6           24          70          57      1023.8      1021.7        7
  Cloud3pm Temp9am Temp3pm RainTomorrow
1        7    14.4    23.6          Yes
2        3    17.5    25.7          Yes
3        7    15.4    20.2          Yes
4        7    13.5    14.1          Yes
5        7    11.1    15.4           No
6        5    10.9    14.8           No
> str(weather_data5)
'data.frame':   353 obs. of  17 variables:
 $ MinTemp      : num  8 14 13.7 13.3 7.6 6.2 6.1 8.3 8.8 8.4 ...
 $ MaxTemp      : num  24.3 26.9 23.4 15.5 16.1 16.9 18.2 17 19.5 22.8 ...
 $ Evaporation  : num  3.4 4.4 5.8 7.2 5.6 5.8 4.2 5.6 4 5.4 ...
 $ Sunshine     : num  6.3 9.7 3.3 9.1 10.6 8.2 8.4 4.6 4.1 7.7 ...
 $ WindGustDir  : Factor w/ 16 levels "E","ENE","ESE",..: 8 2 8 8 11 10 10 1 9 1 ...
 $ WindGustSpeed: int  30 39 85 54 50 44 43 41 48 31 ...
 $ WindSpeed9am : int  6 4 6 30 20 20 19 11 19 7 ...
 $ WindSpeed3pm : int  20 17 6 24 28 24 26 24 17 6 ...
 $ Humidity9am  : int  68 80 82 62 68 70 63 65 70 82 ...
 $ Humidity3pm  : int  29 36 69 56 49 57 47 57 48 32 ...
 $ Pressure9am  : num  1020 1012 1010 1006 1018 ...
 $ Pressure3pm  : num  1015 1008 1007 1007 1018 ...
 $ Cloud9am     : int  7 5 8 2 7 7 4 6 7 7 ...
 $ Cloud3pm     : int  7 3 7 7 7 5 6 7 7 1 ...
 $ Temp9am      : num  14.4 17.5 15.4 13.5 11.1 10.9 12.4 12.1 14.1 13.3 ...
 $ Temp3pm      : num  23.6 25.7 20.2 14.1 15.4 14.8 17.3 15.5 18.9 21.7 ...
 $ RainTomorrow : Factor w/ 2 levels "No","Yes": 2 2 2 2 1 1 1 1 2 1 ...
> summary(weather_data5)
    MinTemp          MaxTemp       Evaporation        Sunshine
 Min.   :-5.300   Min.   : 7.60   Min.   : 0.200   Min.   : 0.000
 1st Qu.: 2.400   1st Qu.:15.10   1st Qu.: 2.400   1st Qu.: 5.900
 Median : 7.500   Median :19.80   Median : 4.200   Median : 8.700
 Mean   : 7.357   Mean   :20.61   Mean   : 4.565   Mean   : 7.926
 3rd Qu.:12.500   3rd Qu.:25.50   3rd Qu.: 6.400   3rd Qu.:10.600
 Max.   :20.900   Max.   :35.80   Max.   :13.800   Max.   :13.600

  WindGustDir  WindGustSpeed    WindSpeed9am     WindSpeed3pm    Humidity9am
 NW     : 72   Min.   :13.00   Min.   : 0.000   Min.   : 0.00   Min.   :36.00
 NNW    : 42   1st Qu.:31.00   1st Qu.: 6.000   1st Qu.:11.00   1st Qu.:64.00
 E      : 36   Median :39.00   Median : 7.000   Median :17.00   Median :72.00
 WNW    : 35   Mean   :40.06   Mean   : 9.683   Mean   :18.02   Mean   :71.87
 ENE    : 30   3rd Qu.:46.00   3rd Qu.:13.000   3rd Qu.:24.00   3rd Qu.:80.00
 ESE    : 23   Max.   :98.00   Max.   :41.000   Max.   :52.00   Max.   :99.00
 (Other):115
  Humidity3pm     Pressure9am      Pressure3pm        Cloud9am
 Min.   :13.00   Min.   : 996.5   Min.   : 996.8   Min.   :0.000
 1st Qu.:32.00   1st Qu.:1015.2   1st Qu.:1012.7   1st Qu.:1.000
 Median :43.00   Median :1020.0   Median :1017.1   Median :4.000
 Mean   :44.45   Mean   :1019.5   Mean   :1016.7   Mean   :3.912
 3rd Qu.:55.00   3rd Qu.:1024.4   3rd Qu.:1021.4   3rd Qu.:7.000
 Max.   :96.00   Max.   :1035.7   Max.   :1033.2   Max.   :8.000

    Cloud3pm        Temp9am         Temp3pm      RainTomorrow
 Min.   :0.000   Min.   : 0.10   Min.   : 5.10   No :289
 1st Qu.:1.000   1st Qu.: 7.70   1st Qu.:14.30   Yes: 64
 Median :4.000   Median :12.60   Median :18.60
 Mean   :4.028   Mean   :12.44   Mean   :19.27
 3rd Qu.:7.000   3rd Qu.:17.00   3rd Qu.:24.00
 Max.   :8.000   Max.   :24.70   Max.   :34.50

> newdata = weather_new1[, c(1:6]
Error: unexpected ']' in "newdata = weather_new1[, c(1:6]"
> newdata = weather_new1[, c(1:6)]
Error: object 'weather_new1' not found
> newdata = weather_data5[, c(1:6)]
> summary(newdata)
    MinTemp          MaxTemp       Evaporation        Sunshine
 Min.   :-5.300   Min.   : 7.60   Min.   : 0.200   Min.   : 0.000
 1st Qu.: 2.400   1st Qu.:15.10   1st Qu.: 2.400   1st Qu.: 5.900
 Median : 7.500   Median :19.80   Median : 4.200   Median : 8.700
 Mean   : 7.357   Mean   :20.61   Mean   : 4.565   Mean   : 7.926
 3rd Qu.:12.500   3rd Qu.:25.50   3rd Qu.: 6.400   3rd Qu.:10.600
 Max.   :20.900   Max.   :35.80   Max.   :13.800   Max.   :13.600

  WindGustDir  WindGustSpeed
 NW     : 72   Min.   :13.00
 NNW    : 42   1st Qu.:31.00
 E      : 36   Median :39.00
 WNW    : 35   Mean   :40.06
 ENE    : 30   3rd Qu.:46.00
 ESE    : 23   Max.   :98.00
 (Other):115
> plot(newdata, pch=36, col="blue", main="Matrix scatterplot of MinTemp, MaxTe$
> set.seed(1)
> MinTemp.c = scale(newdata$MinTemp, center=TRUE, scale=FALSE)
> MaxTemp.c = scale(newdata$MaxTemp, center=TRUE, scale=FALSE)
> WindGustDir.c = scale(newdata$WindGustDir, center=TRUE, scale=FALSE)
Error in colMeans(x, na.rm = TRUE) : 'x' must be numeric
> WindGustSpeed.c = scale(newdata$WindGustSpeed, center=TRUE, scale=FALSE)
> Evaporation.c = scale(newdata$Evaporation, center=TRUE, scale=FALSE)
new.c.vars = cbind(MinTemp.c, MaxTemp.c, WindGustSpeed.c, Evaporation.c)
newdata = cbind(newdata, new.c.vars)
names(newdata)[5:7] = c("MinTemp.c","MaxTemp.c","WindGustSpeed.c","Evaporati$

names(newdata)[7:10] = c("MinTemp.c","MaxTemp.c","WindGustSpeed.c","Evaporat$
summary(newdata)
model1 = lm(Sunshine ~ MinTemp.c + MaxTemp.c + WindGustSpeed.c +Evaporation.$
require("nnet")

feed_net = nnet(Sunshine/50 ~ MinTemp.c + MaxTemp.c + WindGustSpeed.c +Evapo$

nnet.predict <- predict(feed_net)*50

summary(nnet.predict)
summary(model1)
