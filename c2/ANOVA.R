
# ANOVA & MANOVA 

###################### Example 1 

datafilename="http://personality-project.org/R/datasets/R.appendix1.data" 

dataset1=read.table(datafilename,header=T)   #read the data into a table 


dataset1
aov.set1 = aov(Alertness~Dosage,data=dataset1)  #do the analysis of variance 

summary(aov.set1)                                    #show the summary table 

print(model.tables(aov.set1,"means"),digits=3)       #report the means and the number of subjects/cell 

boxplot(Alertness~Dosage,data=dataset1)        #graphical summary 



########################## Example 2 

#two ways: 
  
  
  
datafilename="http://personality-project.org/r/datasets/R.appendix2.data" 

data.set2=read.table(datafilename,header=T)   #read the data into a table 

data.set2                                      #show the data 

aov.set2 = aov(Alertness~Gender*Dosage,data=data.set2)         #do the analysis of variance 

summary(aov.set2)                                    #show the summary table 

print(model.tables(aov.set2,"means"),digits=3)       #report the means and the number of subjects/cell 

boxplot(Alertness~Dosage*Gender,data=data.set2) #graphical summary of means of the 4 cells 
