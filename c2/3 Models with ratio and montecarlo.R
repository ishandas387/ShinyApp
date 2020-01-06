library(class) 
library(e1071) 
library(MASS) 
data("iris")
Tit=iris

M=matrix(NA,3,3) 

ratio=c(40,60,80) 

for (j in 1:3){ 
  
  
  
  acc=c(0,0) 
  
  mc=1000 
  
  for(i in 1:mc){ 
    
    n=nrow(Tit) 
    
    indexes = sample(n,n*(ratio[j]/100)) 
    
    trainset = Tit[indexes,] 
    
    testset = Tit[-indexes,] 
    
    #NB 
    
    m <- naiveBayes(Species ~ ., data = trainset) 
    
    actual=testset$Species 
    
    pred= predict(m, testset) 
    
    tab=table(pred,actual)  # confusion matrix  
    
    accuracy=mean(pred==actual) 
    
    # Mulitnomial Logistic Regression 
    
    library(nnet) 
    
    mlr<- multinom(Species ~ ., data = trainset) 
    
    pred_mlr= predict(mlr, testset) 
    
    accuracy_mlr=mean(pred_mlr==actual) 
    # SVM Logistic Regression 
    
    
    svm_model<- svm(Species ~ ., data = trainset,kernel='linear') 
    
    pred_svm= predict(svm_model, testset) 
    
    accuracy_svm=mean(pred_svm==actual) 
    
    
    vector_accuracy = c(accuracy,accuracy_mlr,accuracy_svm) 
    acc=acc+(1/mc)*vector_accuracy
    
  } 
  
  M[j,]= acc 
  
} 

M

x=ratio 

plot(x, M[,1],ylim=c(0.93,.98),col='red', type='b') 

lines(x, M[,2],col='blue', type='b') 

lines(x, M[,3],col='green', type='b') 
