x=read.csv("My11.csv")
x
View(x)
scatter.smooth(x=x$Runs,y=x$Ave)
scatter.smooth(x=x$Mat,y=x$Runs)
scatter.smooth(x=x$HS,y=x$Ave)
scatter.smooth(x=x$SR,y=x$X100)
scatter.smooth(x=x$Inns,y=x$X100)
# in the graph we can see that the releation ship is not linear
cor(x$Ave, x$Runs)
cor(x$Inns,x$X100)
cor(x$Runs,x$Mat)
cor(x$Inns,x$Runs)

#Correlation can take values between -1 to +1.
#build linear model
linearMod <- lm(Runs~Ave, data=x)
print(linearMod)
linearMod2 <- lm(Mat~Runs, data=x)
print(linearMod2)
linearMod3 <- lm(HS~Ave, data=x)
print(linearMod3)
linearMod4 <- lm(Inns~X100, data=x)
print(linearMod4)

#The function used for building linear models is lm().
#The lm() function takes in two main arguments, namely: 1. Formula 2. Data.

summary(linearMod)
summary(linearMod2)
summary(linearMod3)
summary(linearMod4)
#The p-Values are very important because, We can consider a linear model to be statistically significant
#only when both these p-Values are less that the pre-determined statistical significance level, 
#which is ideally 0.05.
#when p Value is less than significance level (< 0.05), the model is significant.
#training of dataset
trainingRowIndex <- sample(1:nrow(x), 0.8*nrow(x)) 
trainingData <- x[trainingRowIndex, ]  # model training data for average
testData  <- x[-trainingRowIndex, ] 
lmMod <- lm(Ave~Runs, data=trainingData)  # build the mode
AvePred=predict(lmMod, testData)
summary (lmMod)

trainingRowIndex <- sample(1:nrow(x), 0.8*nrow(x)) 
trainingData <- x[trainingRowIndex, ]  # model training data runs
testData  <- x[-trainingRowIndex, ] 
lmMod1 <- lm(Runs~Mat, data=trainingData)  # build the mode
RunsPred=predict(lmMod1, testData)
summary (lmMod1)



trainingRowIndex <- sample(1:nrow(x), 0.8*nrow(x)) 
trainingData <- x[trainingRowIndex, ]  # model training data strike rate
testData  <- x[-trainingRowIndex, ] 
lmMod2 <- lm(Inns~SR, data=trainingData)  # build the mode
SRPred=predict(lmMod1, testData)
summary (lmMod2)

trainingRowIndex <- sample(1:nrow(x), 0.8*nrow(x)) 
trainingData <- x[trainingRowIndex, ]  # model training data for Inns
testData  <- x[-trainingRowIndex, ] 
lmMod2 <- lm(Inns~Mat, data=trainingData)  # build the mode
InnsPred=predict(lmMod1, testData)
summary (lmMod2)





#Calculate prediction accuracy and error rates
#for Average
actuals_preds <- data.frame(cbind(actuals=testData$Ave, predicteds=AvePred)) 
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy
head(actuals_preds)


# For Runs
actuals_preds2 <- data.frame(cbind(actuals=testData$Runs, predicteds=RunsPred)) 
correlation_accuracy <- cor(actuals_preds2)
correlation_accuracy
head(actuals_preds2)

# For Strike rate
actuals_preds4 <- data.frame(cbind(actuals=testData$SR, predicteds=SRPred)) 
correlation_accuracy <- cor(actuals_preds4)
correlation_accuracy
head(actuals_preds4)

# For Inns 

actuals_preds5 <- data.frame(cbind(actuals=testData$Inns, predicteds=InnsPred)) 
correlation_accuracy <- cor(actuals_preds5)
correlation_accuracy
head(actuals_preds5)






#A higher correlation accuracy implies that the actuals and predicted values have similar directional movement, i.e.
#when the actuals values increase the predicteds also increase and vice-versa.

#min max Accuracy
min_max_accuracy <- (mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)))*100 
min_max_accuracy

min_max_accuracy1 <- (mean(apply(actuals_preds2, 1, min) / apply(actuals_preds2, 1, max)))*100 
min_max_accuracy1

min_max_accuracy2 <- (mean(apply(actuals_preds4, 1, min) / apply(actuals_preds4, 1, max)))*100 
min_max_accuracy2

min_max_accuracy3 <- (mean(apply(actuals_preds5, 1, min) / apply(actuals_preds5, 1, max)))*100 
min_max_accuracy3

#BoxPlot - Check for outliers
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(x$Ave, main="Ave", sub=paste("Outlier rows: ", boxplot.stats(x$Ave)$out))  # box plot for 'Ave'
boxplot(x$Runs, main="Runs", sub=paste("Outlier rows: ", boxplot.stats(x$Runs)$out))  # box plot for 'Runs'
boxplot(x$SR, main="Strike Rate", sub=paste("Outlier rows: ", boxplot.stats(x$SR)$out))  # box plot for 'Strike rate'
boxplot(x$Inns, main="Inns", sub=paste("Outlier rows: ", boxplot.stats(x$Inns)$out))
#Generally, any datapoint that lies outside 
#the 1.5 * interquartile-range (1.5*IQR) is considered an outlier,
#IQR is calculated as the distance between the 
#25th percentile and 75th percentile values for that variable.

#Density plot
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(x$Ave), main="Density Plot: Ave", ylab="Runs", sub=paste("Skewness:", round(e1071::skewness(x$Ave), 2)))  # density plot for 'speed'
polygon(density(x$Ave), col="red")
plot(density(x$Runs), main="Runs", ylab="Math", sub=paste("Skewness:", round(e1071::skewness(x$Runs), 2)))  # density plot for 'dist'
polygon(density(x$Runs), col="green")
plot(density(x$Runs), main="Strike Rate", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(x$SR), 2)))  # density plot for 'dist'
polygon(density(x$Runs), col="yellow")

#skewness is a measure of the asymmetry of the probability distribution
rank(x)
#Model Formula
model.matrix( Ave~Runs, data=x)
model.matrix( Inns~X100, data=x)
AIC(linearMod)
AIC(linearMod2)
AIC(linearMod3)
AIC(linearMod4)

