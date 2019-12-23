link = 'http://users.stat.ufl.edu/~winner/data/nfl2008_fga.csv'
ds=read.csv(link)
#data= data(ds, package='rattle') 



#attach(wine) 
View(ds)
library(MASS) #LDA 



dataset.lda <- lda(qtr~togo+kicker+ydline, data=ds) 



dataset.lda  
