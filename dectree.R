data <- read.csv("decex.csv")
library(caTools)
split <- sample.split(data, SplitRatio=0.8)

training <- subset(data, split=="TRUE")
testing <- subset(data, split=="FALSE")

library(rpart)
colnames(data)
model <- rpart(Class~. ,data)
model
plot(model, margin=0.1)
text(model,use.n="TRUE")
predic <- predict(model,newdata=testing,typle="class")
predic
predic <- predict(model,newdata=testing,typle="class")
library(caret)
confusionMatrix(table(predic,testing))

