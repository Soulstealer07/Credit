getwd()
Advertising <- read.csv("C:/Users/micha/Onedrive/Documents/Credit/Credit/Advertising.csv")
Advertising <- as.data.frame(Advertising)
attach(Advertising)
model <- lm(sales ~ TV + radio + TV:radio, data = Advertising)
summary(model)
model7<-lm(sales~TV+radio,data=Advertising)
summary(model7)
summary(model)
#Interpretation of the interaction coefficient measures the increase in the effectiveness of TV advertising for a one unit increase in the radio advertising and visa versa
library(scatterplot3d)
plot3d <-scatterplot3d(TV,radio,sales,angle=10,color="green",pch=16,main="Regression Plane")
plot3d$plane3d(model7,lty.box="solid",col="blue")
