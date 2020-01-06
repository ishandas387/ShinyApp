"
 BOSTON DATA SET 
 MONTECARLO RUNS FOR RMSE AVG
 LINEAR REGRESSION(GLM)
 SVM (SVR - type -eps-regress  kernel - radial -- linear,poly, sigmoid )
 
 IN COMMENTS - REDUCED MODEL RMSE CALCULATION (stepAIC)
 
"

library(MASS) 
library(e1071)
vect_rmse=c(0,0) 
dataset1=Boston 

sum_svr =0
sum=0
# Monte Carlo runs 
for(j in 1:100) {
  
  ####split dataset 
  n=nrow(dataset1) 
  indexes = sample(n,n*(80/100)) 
  trainset = dataset1[indexes,] 
  testset = dataset1[-indexes,] 
  
  # full model 
  full.model = glm(trainset$medv~., data= trainset, family='gaussian') #Fitting the lineaer reg Model  
  
  full.model_svr = svm(trainset$medv~., data=trainset, type='eps-regress', kernel="radial")  
  
  
  actual=testset$medv 
  
  yhat=predict( full.model, testset) 
  yhat_svr = predict(full.model_svr,testset)
  
  rmse1=sqrt((sum(yhat -actual)^2)/(nrow(testset))) 
  rmse_svr_1 = sqrt((sum(yhat_svr -actual)^2)/(nrow(testset))) 
  
  # reduced  model 
  #reduced.model=stepAIC(full.model) 
  #yhat2=predict( reduced.model, testset) 
  #rmse2=sqrt((sum(yhat2 -actual)^2)/(nrow(testset))) 
  
  #average and put it in the vector
  vect_rmse=vect_rmse+(1/100)*c(rmse1, rmse_svr_1) 
  sum = sum + rmse1
  sum_svr = sum_svr+ rmse_svr_1
  
} 

rmseavg <-sum/100
rmseavg_svr <- sum_svr /100

rmseavg 
rmseavg_svr
vect_rmse
