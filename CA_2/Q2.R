#Logistic regression for multiple independent variables
# different scores on Athlete to Weight and Weight
library(caret)
#Q2<-read.csv(file.choose(),header=TRUE)
link='http://users.stat.ufl.edu/~winner/data/nfl2008_fga.csv' 
Q2=read.csv(link) 

#View the Dataset
#View(Q2)

#Check the dimension of data (Row & Columns)
dim(Q2)

# create new dataset without missing data 
cleanQ2 <- na.omit(Q2)
dim(cleanQ2)
# library(tidyverse)
# selectivecol = cleanQ2 %>% select(10, 11,12,15)
# View(selectivecol)
# 
# #Summary of data
# summary(selectivecol)
# 
# #Stats by group
# library(psych)
# describe(selectivecol)


######################  GLM  ############################################333
x1=cleanQ2$togo	
x2=cleanQ2$kicker
x3=cleanQ2$ydline

y=cleanQ2$homekick

cleanQ2 <- na.omit(data.frame( x1,x2,x3,y)) # remove missing values
cleanQ2
dim(cleanQ2)

# library(caTools) # useful to split data to training and test datasets
# split=sample.split(cleanQ2[,2],SplitRatio=.8)
# trainset=subset(cleanQ2, split==T)  # training dataset
# testset=subset(cleanQ2,split==F)   # test dataset
set.seed(200)
n=nrow(cleanQ2)

indexes = sample(n,n*(80/100))

trainset = cleanQ2[indexes,] # Train set

testset = cleanQ2[-indexes,]

dim(testset)
dim(trainset)

trainset.glm <- glm(y ~.,trainset, family="binomial") # ~. shows that we include all ind. variables
summary(trainset.glm) # both variable are sig.

res=predict(trainset.glm,testset, type="response")  # prediction
#View(res)
# dim(testset[,1])
# dim(trainset[,1])
predictedvalues=rep(0,133)
predictedvalues
predictedvalues[res>0.05]=1   # probability of Gender being 1, if p<0.5 then gender=0
table( predictedvalues, actualvalues=testset[,4])
mean(predictedvalues == testset[,4]) # correctness of prediction


