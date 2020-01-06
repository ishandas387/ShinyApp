#MANOVA  

# test of normality: Shapiro-Wilk test   

# load package mvnormtest

#library(mvnormtest)
#mshapiro.test()   



dataset <- iris  



sepl <- iris$Sepal.Length 

petl <- iris$Petal.Length 

# MANOVA test 

res.man <- manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris) 

summary(res.man) 
