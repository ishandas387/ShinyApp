#part A
#load the dataset
link='http://users.stat.ufl.edu/~winner/data/nfl2008_fga.csv' 
data=read.csv(link) 



#togo, ydline, kicker
y=data$homekick
x1=data$togo
x2=data$ydline
x3=data$kicker
df=data.frame(x1,x2,x3,y)



#part B
#data cleansing: removing missing values, removing outliers......
dataset1 <- na.omit(df)#removing outliers



set.seed(167) 
####split dataset 
n=nrow(dataset1) 
indexes = sample(n,n*(80/100)) 
trainset = dataset1[indexes,] 
testset = dataset1[-indexes,] 
full.model= glm(trainset$y~., data= trainset, family='binomial') #Fitting the lineaer reg Model  
summary(full.model) 



#part C
phat_i=predict(full.model,testset, type="response")  # phat_i 



predictedvalues=rep(0,length(phat_i)) 
predictedvalues[phat_i>0.5]=1   # probability of gender being 1, if p<0.5 then gender=0 
actual=testset$y
df=data.frame(actual,predictedvalues) 



#part D
confusion_matrix=table( predictedvalues, actualvalues=actual) #confusion matrix 
accuracy=mean(predictedvalues == actual) # accuary
accuracy
