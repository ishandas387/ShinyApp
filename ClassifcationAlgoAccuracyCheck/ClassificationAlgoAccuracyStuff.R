#load required library 

library(e1071) 
library(nnet) 
library(ggplot2)
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
#load built-in iris dataset 
df <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
dataset=PimaIndiansDiabetes2  
acc=c(SVM =0,NB= 0, MLR =0); 
mc=100 

ggplot(dataset, aes(x = glucose, y = age)) +
  geom_point(aes(color = factor(diabetes)))

table(dataset$diabetes)
#split into training and test sets 
for(i in 1:mc){ 
  
  n=nrow(dataset) 
  
  indexes = sample(n,n*(80/100)) 
  
  trainset = dataset[indexes,] 
  
  testset = dataset[-indexes,] 
  
  #build model linear kernel and C-classification (soft margin) with default cost (C=1) 
  
  svm_model <- svm(diabetes~ ., data=trainset, method="nu-classification", kernel="linear") 
  
  actual=testset$diabetes
  
  pred_svm= predict(svm_model, testset, type='response') 
  
  accuracy_svm=mean(pred_svm==actual) 
  accuracy_svm
  pred_svm
  # NB 
  
  nb_model <- naiveBayes(diabetes~ ., data=trainset) 
  
  pred_nb= predict(nb_model, testset) 
  pred_nb
  confusionMatrix(pred_nb,actual)
  accuracy_nb=mean(pred_nb==actual) 
  accuracy_nb
  # Multinomial Logistic reg 
  
  
  mlr_model <- multinom(diabetes~ ., data=trainset) 
  
  pred_mlr= predict(mlr_model, testset) 
  pred_mlr
  
  accuracy_mlr=mean(pred_mlr==actual) 
  accuracy_mlr
  
  vect_accuracy=c(accuracy_svm,accuracy_nb,accuracy_mlr) 
  
  acc=acc+(1/mc)*vect_accuracy 
  
} 
acc
pdf <- data.frame(acc)
pdf
plotdf <- as.data.frame(acc) 
plotdf

acc <- acc*100
table(acc)

barplot(acc, main="Accuracy",
        xlab="Machine learning model", col = rainbow(5),xlim=c(0,100),
        width = 0.1,horiz = TRUE)

p <-ggplot(plotdf, aes(x=seq_along(acc),y=acc))+
geom_bar(stat="identity")+theme_minimal()


# 
  # ggplot(acc, aes(x=acc, y=acc, fill=acc)) +
#   geom_bar(stat="identity")+theme_minimal()
# 
# plot(plotdf, acc,ylim=c(0.93,.98),col='blue', type='b')
# 
# lines(x, M[,2],col='red', type='b')
# 
# lines(x, M[,3],col='green', type='b')
