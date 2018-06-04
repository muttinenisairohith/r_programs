data <- read.csv(file="pimalog.csv")

library(caTools)
split <- sample.split(data, SplitRatio=0.8)
split
training <- subset(data,split=="TRUE")
testing <- subset(data,split=="FALSE")

model <- glm(type~.,training,family="binomial")
summary(model)

model <- glm(type~.-skin,training,family="binomial")
summary(model)

res <- predict(model,training,type="response")

library(ROCR)

ROCRpred = prediction(res,training$type)
ROCRpref <- performance(ROCRpred,"tpr","fpr")
png(file="logreg.png")
plot(ROCRpref,colorize=TRUE,print.cutoffs.at=seq(0.1,by = 0.1))

table(Actualvalue=testing$type,predictedvalue=res>0.5)
dev.off()
