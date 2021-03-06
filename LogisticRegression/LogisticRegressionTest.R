"Logistic regression will be applied on datasets where classifcation output is needed."
"The source of the data is from UCLA which has 4 variable
called admit, GRE score, GPA and rank of their undergrad school. 
Our aim is to build a model so that predict the probability
of that student getting admit if we are given his profile."

df <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
str(df)

#We see that variable are either integer or number.

summary(df)

#We can notice that there are a greater number of rejects than there are acceptance 
#since the mean of variable admit is less than "0.5".
"We do this to check if the admits are distributed well 
enough in each category of rank. If let's say one rank has only 5 admit or reject 
information, then it will not be necessary to include that rank in analysis."
xtabs(~ admit +rank ,data=df)

#getting a sense of data of what we are predicting.
#if the data set has balanced stuff.
table(df$admit)

set.seed(167) 

####split dataset 

n=nrow(df) 
indexes = sample(n,n*(80/100)) 
trainset = df[indexes,] 
testset = df[-indexes,] 

#Fitting the  reg Model  with family binomial
#determining admit with all the other data
full.model = glm(trainset$admit~., data= trainset, family='binomial') 
#summary of model
summary(full.model) 

#getting the predicted value
phat_i=predict(full.model,testset, type="response")  # phat_i 

predictedvalues=rep(0,length(phat_i)) 
#
predictedvalues[phat_i>0.5]=1   
actual=testset$admit 

dfPredicted=data.frame(actual,predictedvalues) 

#getting the confuction matrix and accuracy
confusion_matrix=table( predictedvalues, actualvalues=actual) #confusion matrix
"You can manually calculate the accuracy from confusion matrix values.
"
confusion_matrix
accuracy=mean(predictedvalues == actual) # accuary  
accuracy
  
####################################################################################

"Another example of simple logistic regression using PimaIndiansDiabetics2 dataset"

#install.packages("mlbench")

"tidyverse for easy data manipulation and visualization
caret for easy machine learning workflow"
library(tidyverse)
library(caret)

data("PimaIndiansDiabetes2", package = "mlbench")

"Data cleanup should include the following-
Remove potential outliers
Make sure that the predictor variables are normally distributed.
If not, you can use log, root, Box-Cox transformation.
Remove highly correlated predictors to minimize overfitting.
The presence of highly correlated predictors might lead to an unstable model 
solution."
#This just omits the empty values. Just 1 part of data analysis/creation
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
# Inspect the data
sample_n(PimaIndiansDiabetes2, 3)
# Split the data into training and test set
set.seed(123)
training.samples <- PimaIndiansDiabetes2$diabetes %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- PimaIndiansDiabetes2[training.samples, ]
test.data <- PimaIndiansDiabetes2[-training.samples, ]

# Fit the model
model <- glm( diabetes ~., data = train.data, family = binomial)
# Summarize the model
summary(model)

"Estimate: the intercept (b0) and the beta coefficient estimates associated to 
each predictor variable

Std.Error: the standard error of the coefficient estimates. 
This represents the accuracy of the coefficients. 
<b>The larger the standard error, the less confident we are about the estimate.<b>

z value: the z-statistic, which is the coefficient estimate (column 2) divided by the standard error of the estimate (column 3)
Pr(>|z|): The p-value corresponding to the z-statistic. $$ The smaller the p-value, the more significant the estimate is. $$"

# Make predictions
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# Model accuracy
mean(predicted.classes == test.data$diabetes)

"Doing the same thing with 1 variable, diabetics ~ glucose"

modelSingle <- glm( diabetes ~ glucose, data = train.data, family = binomial)
summary(modelSingle)


#using a random sample data, not the test set.
newdata <- data.frame(glucose = c(20,  180))
probabilitiesgl <- modelSingle %>% predict(newdata, type = "response")
predicted.classesgl <- ifelse(probabilitiesgl > 0.5, "pos", "neg")
predicted.classesgl


#change train data diab to 1,0 to plot, just to show corealtion
mutatedStuff <- mutate(train.data, prob = ifelse(diabetes == "pos", 1, 0)) 

ggplot(mutatedStuff,aes(glucose, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Plasma Glucose Concentration",
    y = "Probability of being diabete-pos"
  )
  


"Multiple logistic regression
The multiple logistic regression is used to predict the probability of class membership based on multiple predictor variables, as follow:"

modelmultiple <- glm( diabetes ~ glucose + mass + pregnant, 
              data = train.data, family = binomial)
summary(modelmultiple)

"Or just doing it with all the values"


modelmultipleAll <- glm( diabetes ~ ., 
                      data = train.data, family = binomial)
summary(modelmultipleAll)

"Estimate: the intercept (b0) and the beta coefficient estimates associated to each predictor variable
Std.Error: the standard error of the coefficient estimates. This represents the accuracy of the coefficients. The larger the standard error, the less confident we are about the estimate.
z value: the z-statistic, which is the coefficient estimate (column 2) divided by the standard error of the estimate (column 3)
Pr(>|z|): The p-value corresponding to the z-statistic. The smaller the p-value, the more significant the estimate is."


"It can be seen that only 5 out of the 8 predictors are significantly associated to the outcome. These include: pregnant, glucose, pressure, mass and pedigree.
The coefficient estimate of the variable glucose is b = 0.045, which is positive. This means that an increase in glucose is associated with increase
in the probability of being diabetes-positive. However the coefficient
for the variable pressure is b = -0.007, which is negative. 
This means that an increase in blood pressure will be associated 
with a decreased probability of being diabetes-positive."

"The regression coefficient for glucose is 0.042. 
This indicate that one unit increase in the glucose concentration
will increase the odds of being diabetes-positive by exp(0.042) 1.04 times."


" it can be noticed that some variables - triceps, insulin and age -
are not statistically significant. Keeping them in the model may contribute 
to overfitting. Therefore, they should be eliminated. This can be done 
automatically using statistical techniques, including stepwise regression
and penalized regression methods."


modelReduced <- glm( diabetes ~ pregnant + glucose + pressure + mass + pedigree, 
              data = train.data, family = binomial)
"Making predictions
We'll make predictions using the test data in order to evaluate the performance of our logistic regression model.

The procedure is as follow:
  
  Predict the class membership probabilities of observations based on predictor variables
Assign the observations to the class with highest probability score (i.e above 0.5)
The R function predict() can be used to predict the probability of being diabetes-positive, given the predictor values.

Predict the probabilities of being diabetes-positive:"
  
  probabilities <- modelReduced %>% predict(test.data, type = "response")
head(probabilities)

predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)
