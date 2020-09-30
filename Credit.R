library(ISLR)
Credit<-as.data.frame(Credit)
attach(Credit)
head(Credit)
pairs(Credit,col="blue",pch=20)
#Make a model with just Gender predicting average credit card balance
head(Credit)
str(Credit)
Gender <- as.numeric(Gender)
Gender <- (Gender-1)
head(Gender)
model<-lm(Balance ~ Gender, data=Credit)
summary(model)
#The average credit card debt for males is estimated to be $509.80
#The average credit card debt for females is $529.53
#The large p-values indicate there is insufficient statistical evidence the slope for gender category is statistically different from zero.
head(Ethnicity)
Ethnicity<-as.numeric(Ethnicity)
Ethnicity<-Ethnicity-2
model1<-lm(Balance ~ Ethnicity, data = Credit)
summary(model1)
#The estimated balance for the baseline, African American, is $531.00
#It is estimated the Asian category will have $$18.69 less than the baseline
#It is estimated the Caucasian category will have #12.50 less than the baseline
#The large p-values indicate there is insufficient statistical evidence the slope for ethnicities is statistically different from zero.


Student<-as.numeric(Student)
Student<-Student-1
model2<-lm(Balance ~ Income + Student, data = Credit)
summary(model2)

model3<-lm(Balance ~ Income + Student + Student*Income,data=Credit)
summary(model3)

par(mfrow=c(1,1))
plot(Income,Balance,col="blue",pch=16,ylab="Balance",main="Average Credit Card Balance vs. Income")
abline(211.1430,5.9843,col="black",lwd=3)
abline(593.8135,5.9843,col="red",lwd=3)
plot(Income,Balance,col="blue",pch=16,ylab="Balance",main="Average Credit Card Balance vs. Income")
abline(200.6232,6.2182,col="black",lwd=3)
abline(677.299,4.219,col="red",lwd=3)
