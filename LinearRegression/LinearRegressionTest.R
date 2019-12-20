library(e1071)  # for skewness function

#Test for linear regression
#ref-> https://www.machinelearningplus.com/machine-learning/complete-introduction-linear-regression-r/

"Linear regression is used to predict the value of a
continuous variable Y based on one or more input predictor variables X. 
The aim is to establish a mathematical formula between the 
the response variable (Y) and the predictor variables (Xs)."
#Using the cars dataset to find a corealtion between dist and speed
head(cars)

"Scatter plot: Visualise the linear relationship between the predictor and response"

scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  # scatterplot
#a linear and positive relationship between the 'dist' and 'speed'.


"Box plot: To spot any outlier observations in the variable. Having outliers in your predictor can drastically affect 
the predictions as they can affect the direction/slope of the line of best fit."



"Generally, an outlier is any datapoint that lies outside the
  1.5 * inter quartile range (IQR).
  IQR is calculated as the distance between the 25th percentile and
  75th percentile values for that variable."
# divide graph area in 2 columns
par(mfrow=c(1, 2))  
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  # box plot for 'distance'

"Density plot: To see the distribution of the predictor variable. Ideally,
a close to normal distribution (a bell shaped curve), 
without being skewed to the left or right is preferred."

par(mfrow=c(1, 2))  # divide graph area in 2 columns

plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'

polygon(density(cars$speed), col="red")

plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'

polygon(density(cars$dist), col="red")



# calculate correlation between speed and distance 
cor(cars$speed, cars$dist) 
"This is an ideal co-relation anything between -1 to +1"

"Building the linear regression model"
# build linear regression model on full data
linearMod <- lm(dist ~ speed, data=cars) 
print(linearMod)

"Coefficients:
(Intercept)        speed  
    -17.579        3.932  "

"This essentially gives the Beta for speed and the constant in a linear
formula B = X + xA
$$dist = Intercept + (?? ??? speed)$$

=> dist = ???17.579 + 3.932???speed"


"Validating the statistical significance of our model"
summary(linearMod)
#The more the stars beside the variable's p-Value, the more significant the variable.

#Testing the model
#Create train and test data set

set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

# build the model
lmMod <- lm(dist ~ speed, data=trainingData)  
# predict distance
distPred <- predict(lmMod, testData)

# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

"you can compute all the error metrics in one 
go using the regr.eval() function in DMwR package."

DMwR::regr.eval(actuals_preds$actuals, actuals_preds$predicteds)
