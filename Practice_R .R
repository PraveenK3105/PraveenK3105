#multiple linear regression 1

getwd()
setwd("C:/Users/Praveen K/Desktop")
regression<-read.csv("1. Multiple Linear Regression.csv")
View(regression)
input <- regression[,c("State","Profit","R.D.Spend","Administration")]
print(head(input))

# Create the relationship model.
model <- lm(Profit~State+R.D.Spend+Administration, data = input)
# Show the model.
print(model)

a <- coef(model)[1]
print(a)

XState <- coef(model)[2]
XR.D.Spend <- coef(model)[3]
XAdministration <- coef(model)[4]

print(XState)
print(XR.D.Spend)
print(XAdministration)

plot(model)
plot(XState)
plot(XR.D.Spend)
plot(XAdministration)
plot(input)

#correlation 2

getwd()
setwd("C:/Users/Praveen K/Desktop")
correlation<-read.csv("3. Correlation - kc_house_data.csv")
View(correlation)
data<-correlation
head(data,5)
library("ggplot2")
cor(data$sqft_living,data$bedrooms)
Tes<- cor.test(correlation$sqft_living,correlation$bedrooms,method = "pearson")
Tes

ggplot(data) +aes(x = sqft_living , y = bedrooms) +
  geom_point(colour = "Red") +
  theme_minimal()
plot(data$sqft_living, data$bedrooms)


#decision tree 3

install.packages('datasets')
install.packages('caTools')
install.packages('party')
install.packages('dplyr')
install.packages('magrittr')
library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)
library(rpart.plot)

data("readingSkills")
head(readingSkills)
sample_data = sample.split(readingSkills, SplitRatio = 0.8)
train_data <- subset(readingSkills, sample_data == TRUE)
test_data <- subset(readingSkills, sample_data == FALSE)
mo<-rpart(nativeSpeaker ~ .,train_data)
mo
rpart.plot(mo)
model<- ctree(nativeSpeaker ~ .,train_data)
plot(model)

#Linear Regression 4

View(cars)
?cars
library(tidyverse)
qplot(speed,dist,data=cars,geom="point")
qplot(speed,dist,data=cars,geom = "point")+geom_smooth(method = lm,se = "FALSE") #se = standard error
lm(dist~speed,data=cars)
model<-lm(dist~speed,data=cars)
summary(model)
model$residuals
cars$residuals<-model$residuals
cars$predicted<-model$fitted.values
View(cars)
predict(model, data.frame(speed=c(12.5,15.5,17,19)))

#linear regression 2

mouse.data <- data.frame(
  weight=c(0.9, 1.8, 2.4, 3.5, 3.9, 4.4, 5.1, 5.6, 6.3),
  size=c(1.4, 2.6, 1.0, 3.7, 5.5, 3.2, 3.0, 4.9, 6.3))

mouse.data # print the data to the screen in a nice format

## plot a x/y scatter plot with the data
plot(mouse.data$weight, mouse.data$size)

## create a "linear model" - that is, do the regression
mouse.regression <- lm(size ~ weight, data=mouse.data)
## generate a summary of the regression
summary(mouse.regression)

mouse.regression$residuals

## add the regression line to our x/y scatter plot
abline(mouse.regression, col="Red")
predict(mouse.regression, data.frame(weight=c(12,54,34,24,33)))
View(mouse.data)

#time series 5

getwd()
setwd("C:/Users/Praveen K/Desktop")
gdp<-read.csv("time series GDP.csv")
View(gdp)
class(gdp)
gdptime<-ts(gdp$GDP,start = min(gdp$DATE),end = max(gdp$DATE),frequency = 4)
class(gdptime)
library(forecast)
library(tseries)
plot(gdptime)
acf(gdptime)
pacf(gdptime)
adf.test(gdptime)
gdpmodel<-auto.arima(gdptime,ic="aic",trace = TRUE)
acf(ts(gdpmodel$residuals))
pacf(ts(gdpmodel$residuals))
mygdpforecast=forecast(gdpmodel,level = c(10),h=10*4)
mygdpforecast
plot(mygdpforecast)
Box.test(mygdpforecast$resid, lag=5, type= "Ljung-Box")
Box.test(mygdpforecast$resid, lag=15, type= "Ljung-Box")
Box.test(mygdpforecast$resid, lag=50, type= "Ljung-Box")

#airpassengers

data("AirPassengers")
install.packages("ROCR")
install.packages("forecast")
library(tseries)
library(forecast)
class(AirPassengers)
start(AirPassengers)
end(AirPassengers)
sum(is.na(AirPassengers))
summary(AirPassengers)
plot(AirPassengers)
tsdata<-ts(AirPassengers,frequency = 10)
ddata<-decompose(tsdata,"multiplicative")
plot(ddata)
plot(ddata$trend)
plot(ddata$random)
plot(ddata$seasonal)
mymodel<-auto.arima(AirPassengers)
forecast<-forecast(mymodel,level = c(95),h=10*12)
plot(forecast)


#neural network 6

#creating training data  set
Sci = c(70,71,72,73,68,69,65,69,80,68)
Mat = c(91,92,93,94,65,69,61,55,91,79)
Eng = c(82,83,84,85,73,66,50,62,95,68)
Pass = c(1,1,1,1,0,0,0,0,1,0)
df = data.frame(Sci,Mat,Eng,Pass)
#install Packages
install.packages("neuralnet")
library("neuralnet")
#Fit Neural Network
nn = neuralnet(Pass ~ Sci + Mat + Eng , data = df,hidden = 3,act.fct = "logistic",linear.output = FALSE)
plot(nn)
#create test data set
sci = c(80,75,65,68)
mat = c(95,92,69,45)
eng = c(85,83,55,50)
test = data.frame(sci,mat, eng)
test
predict = compute(nn,test)
predict $ net.result
probab<-predict$net.result
pre<-ifelse(probab>0.5,1,0)
pre


#logistic regression 7


# Load the dataset
View(Titanic)
data(Titanic)
head(Titanic)
data<-data.frame(Titanic)

# Fit the logistic regression model
model <- glm(Survived ~ Class + Sex + Age, family = binomial, data = data)

# View the summary of the model
summary(model)
# Install and load the required packages
install.packages("ROCR")
library(ROCR)

# Fit the logistic regression model
model <- glm(Survived ~ Class + Sex + Age, family = binomial, data = data)

# Make predictions on the dataset
predictions <- predict(model, type = "response")

# Create a prediction object for ROCR
prediction_objects <- prediction(predictions, data$Survived)

# Create an ROC curve object
roc_object <- performance(prediction_objects, measure = "tpr", x.measure = "fpr")

# Plot the ROC curve
plot(roc_object, main = "ROC Curve", col = "Blue", lwd = 2)

# Add labels and a legend to the plot
legend("bottomright", legend =
         paste("AUC =", round(performance(prediction_objects, measure = "auc")
                              @y.values[[1]], 2)), col = "Blue", lwd = 2)

