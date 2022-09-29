#Install Packages
library(leaps) #for subset selection
library(gains) #for gains and lift chart
library(forecast) #for accuracy measures

#load boston housing data and eliminate unecessary variable
setwd("C:/Users/nicho/OneDrive/Documents/PredAnalyticsMachineLearning/MultiLinearRegressionLab")

boston.df = read.csv("BostonHousing.csv")
boston.df = boston.df[,-14]

#Fit initial model for Median Value price using crime rate, rooms, and dummy variable
housing.lm = lm(MEDV ~ CRIM + CHAS + RM, data = boston.df)
options(scipen = 999)
print(summary(housing.lm))


set.seed(1)  
getwd()
#split into training and validation sets (using 60-40 split on this data)
train.index = sample(c(1:506), (506 * 0.6))
train.df = boston.df[train.index, ]
valid.df = boston.df[-train.index, ]

#Use exhaustive search to find best predictors
search = regsubsets(MEDV ~ ., data = train.df, nbest = 1, nvmax = dim(train.df[]), method = "exhaustive")
sum.exhaustive = summary(search)
sum.exhaustive$which
sum.exhaustive$adjr2  

#find top 3 models
models.exhaustive <-  order(sum.exhaustive$adjr2, decreasing = T)[1:3]
models.exhaustive

## create a fitted model using the training data set (including all variables)
reg2 <- lm(MEDV ~ ., data = train.df)

#use backward stepwise function to determine best regression
housing.lm.step.backward = step(reg2, direction = "backward")
summary(housing.lm.step.backward)

#predict values with stepwise model and check accuracy
housing.lm.step.pred = predict(housing.lm.step.backward, valid.df)
accuracy(housing.lm.step.pred, valid.df$MEDV)  

