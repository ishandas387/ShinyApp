#Import Air Quality Dataset
data("airquality")

#View the Dataset
View(airquality)

#Check the dimension of data (Row & Columns)
dim(airquality)

# create new dataset without missing data 
cleandata <- na.omit(airquality)
dim(cleandata)

#Summary of data
summary(cleandata)

#Stats by group
library(psych)
describe(cleandata)

#Mean
meantemp = mean(cleandata$Temp)
print(meantemp)

#Median
mediantemp = median(cleandata$Temp)
print(mediantemp)

#Mode
library(DescTools)
modetemp = Mode(cleandata$Temp)
print(modetemp)

#range
range(cleandata$Temp)

#InterQuartile Range - The interquartile range of an observation variable is the difference of its upper and lower quartiles.
IQR(cleandata$Temp)

#Variance - Variance tells you how far a data set is spread out, but it is an abstract number that really is only useful for calculating the Standard Deviation.
var(cleandata$Temp)

#Standard Deviation
sdtemp = sd(cleandata$Temp)
print(sdtemp)

# compute 3sigma and find the outliers:
xL=meantemp-3*sdtemp
xU=meantemp+3*sdtemp
outliertemp=c(xL,xU)
boxplot(cleandata$Wind, main="Wind", sub=paste("Outlier rows: ", boxplot.stats(cleandata$Wind)$out))

print(outliertemp)

#Skewness - negative skewness indicates that the mean of the data values is less than the median, and the data distribution is left-skewed.
library(e1071)
tempdata = cleandata$Temp
skewness(tempdata)
hist(tempdata)
par(mfrow=c(1, 2))  # divide graph area in 2 columns

plot(density(cleandata$Wind), main="Density Plot: Wind", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cleandata$Wind), 2)))  # density plot for 'speed'
polygon(density(cleandata$Wind), col="blue")

plot(density(tempdata), main="Density Plot: Temp", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(tempdata),2)))  # density plot for 'speed'
polygon(density(tempdata), col="red")

#kurtosis - 
kurtosis(tempdata)
hist(tempdata)

#Standard Error of Mean (Standard error of the mean may not be appropriate for skewed data)
setemp = sdtemp/sqrt(length(cleandata$Temp))
print(setemp)

#From psych library
describe(cleandata$Temp)

#Summary, Quartiles, Percentiles
summary(cleandata$Temp)
sort(cleandata$Temp)
quantile(cleandata$Temp)










