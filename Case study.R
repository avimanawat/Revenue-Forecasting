getwd()
setwd("C:/Users/avman/OneDrive/Desktop/R classes/excel files")

options(max.print=1000000)
airport = read.csv("Den1.csv", header = TRUE)
airport=airport[,-1]
airport

airport$Cannabis. = ifelse(airport$Cannabis.== 'hype',1,0)

for(cut in unique(airport$Month)) {
  airport[paste("Month",cut,sep="_")]= ifelse(airport$Month==cut,1,0)
}

trainingData=airport[1:62,]
testData=airport[63:66,]

cor(trainingData[,-2])

trainingData$Enplaned_tr = (trainingData$Enplaned - mean(trainingData$Enplaned))/sd(trainingData$Enplaned)
testData$Enplaned_tr = (testData$Enplaned - mean(testData$Enplaned))/sd(testData$Enplaned)

lin_mod=lm(log(Ground)~Years+exp(Enplaned_tr)+log(Origin...Destin),data = trainingData)

summary(lin_mod)
anova(lin_mod)

library(car)
vif(lin_modGr)
AIC(lin_modGr)
BIC(lin_modGr)

exp(predict(lin_modGr,newdata = testData))
testData$Ground

plot(exp(predict(lin_modGr,newdata = testData)),testData$Ground)

par(mfrow= c(2,2))
plot(lin_mod)

plot(lin_mod$fitted.values,lin_mod$residuals)
abline(h=0, col = 'Red')

plot(log(trainingData$Origin...Destin),lin_mod$residuals)
abline(h=0, col = 'Red')

plot(trainingData$Years,lin_mod$residuals)
abline(h=0, col = 'Red')

plot(exp(trainingData$Enplaned_tr),lin_mod$residuals)
abline(h=0, col = 'Red')

qqnorm(lin_mod$residuals)
qqline(lin_mod$residuals, col = 2)

library(ggplot2)
library(dplyr)
bymonth = group_by(trainingData,Month)
parking_bymonth = summarize(bymonth, sumof1 = sum(Parking))
class(parking_bymonth)
ggplot(parking_bymonth,aes(x=Month, y=sumof1))+ geom_bar(stat="identity")+labs(x="Months",y="Revenue")+ggtitle("Parking") +
  theme(plot.title = element_text(hjust = 0.5))

rental_bymonth = summarize(bymonth, sumof2 = sum(Rental.Car))
class(rental_bymonth)
ggplot(rental_bymonth,aes(x=Month, y=sumof2))+ geom_bar(stat="identity")+labs(x="Months",y="Revenue")+ggtitle("Rental Cars") +
  theme(plot.title = element_text(hjust = 0.5))

concession_bymonth = summarize(bymonth, sumof3 = sum(Concession))
class(concession_bymonth)
ggplot(concession_bymonth,aes(x=Month, y=sumof3))+ geom_bar(stat="identity")+labs(x="Months",y="Revenue")+ggtitle("Concession") +
  theme(plot.title = element_text(hjust = 0.5))

ground_bymonth = summarize(bymonth, sumof4 = sum(Ground))
class(ground_bymonth)
ggplot(ground_bymonth,aes(x=Month, y=sumof4))+ geom_bar(stat="identity")+labs(x="Months",y="Revenue")+ggtitle("Ground") +
  theme(plot.title = element_text(hjust = 0.5))

byyear = group_by(trainingData,Year)
byyear2012 = summarize(byyear, sumof6 = sum(Ground))

#####################################


trainingData$Concession = abs(trainingData$Concession)
lin_mod1=lm(log(Concession)~Years+OD_tr+Month_Jan+Rental.Car,data = trainingData)
vif(lin_mod1)
summary(lin_mod1)

trainingData$OD_tr = (trainingData$Origin...Destin - mean(trainingData$Origin...Destin))/sd(trainingData$Origin...Destin)
testData$OD_tr = (testData$Origin...Destin - mean(testData$Origin...Destin))/sd(testData$Origin...Destin)

summary(lin_mod1)
anova(lin_mod1)

vif(lin_mod1)
AIC(lin_mod1)
BIC(lin_mod1)

exp(predict(lin_mod1,newdata = testData))
testData$Concession

library(MASS)
library(car)
stepAIC(lin_mod1)
?stepAIC
