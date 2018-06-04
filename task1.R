

setwd("F:/SOFTWARE/R/")
data <- read.csv("Accelerometer.csv")
View(data)
library(caTools)
split <- sample.split(data, SplitRatio=0.8)

training <- subset(data,split=="TRUE")
testing <- subset(data,split=="FALSE")

model <- lm(Theta~. ,data=training)

summary(model)
plot(training$Theta,type="l",lty=1.8,col="green")
result <- predict(model,testing)
#print(result)
plot(testing$Theta,type="l",lty=1.8,col="green")
lines(result,type="l",col="blue")
a <- data.frame(X=-2,Y=3,Z=4,R=0)
result1 <- predict(model,a)
print(result1)
