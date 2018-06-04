require('ggvis');
data <- read.csv("~/CHANDU WORK/drinking_stand_wrist.csv")
data1 <- read.csv("~/CHANDU WORK/priyaD.csv")
data2 <- read.csv("~/CHANDU WORK/ramyaD.csv")
data3 <- read.csv("~/CHANDU WORK/sahithiD.csv")
data1 <- subset(data1, data1$time > 10 & data1$time < 20)
data2 <- subset(data2, data2$time > 10 & data2$time < 20)
data3 <- subset(data3, data3$time > 10 & data3$time < 20)
plot <- ggvis(data1,x= ~ time, y= ~ pitch)
plot1 <- ggvis(data2,x= ~ time, y= ~ pitch)
plot2 <- ggvis(data3,x= ~ time, y= ~ pitch)
png(file="plot1.png")
layer_points(plot)
layer_smooths(plot)
png(file="plot2.png")
layer_points(plot1)
layer_smooths(plot1)
png(file="plot3.png")
layer_points(plot2)
layer_smooths(plot2)

dev.off()
#-------------------------0.3
library(caTools)
split <- sample.split(data1 ,SplitRatio=0.3)

data2 <- subset(data1,split=="TRUE")
plot2 <- ggvis(data2,x= ~time, y= ~pitch)
png(file="plot2.png")
layer_points(plot2)
  layer_lines()
dev.off()
#-----------------------------true values
data3 <- subset(data2, data2$time < 75)
plot3 <- ggvis(data3,x= ~time, y= ~pitch)
png(file="plot3.png")
layer_points(plot3)%>%
  layer_lines()
dev.off()
#comparision of data1 and data3
png(file="plot4.png")
par(mfrow=c(3,1)) 
plot(data1$time,data1$roll,type='o')
plot(data2$time,data2$pitch,type='o')
plot(data3$time,data3$pitch,type='o')
dev.off()
relation <- lm(data3$pitch~data3$time)
print(relation)
print(summary(relation))
data4 <- subset(data2, data2$time < 15)
data5 <- predict(relation,2)
png(file="plot5.png")
plot(data2$pitch,type="l",col="green")
lines(data5,type="l",col="blue")
dev.off()
#------------------------------------cycles

#-----------------------------1st
cyc3 <- subset(data3,data3$time>1&data3$time<5.5)
plot(cyc3$time,cyc3$pitch,type='o')
relation <- lm(cyc3$pitch~cyc3$time)
print(relation)
#------------------------------2nd
cyc4 <- subset(data3,data3$time>7.5&data3$time<9.5)
plot(cyc4$time,cyc4$pitch,type='o')
relation <- lm(cyc4$pitch~cyc4$time)
print(relation)
#- ---------------------------------3rd
cyc5 <- subset(data3,data3$time>10.7&data3$time<13.5)
plot(cyc5$time,cyc5$pitch,type='o')
relation <- lm(cyc5$pitch~cyc5$time)
print(relation)
#-------------------------------------4th
cyc6 <- subset(data3,data3$time>14.29&data3$time<17.39)
plot(cyc6$time,cyc6$pitch,type='o')
relation <- lm(cyc6$pitch~cyc6$time)
print(relation)
#------------------------------5th

cyc1 <- subset(data3,data3$time>25.70&data3$time<29.10)
plot(cyc1$time,cyc1$pitch,type='o')
min1 <- min(cyc1$pitch)
relation <- lm(cyc1$pitch~cyc1$time)
print(relation)
 #-----------------------------6th
cyc2 <- subset(data3,data3$time>21.70&data3$time<25.70)
plot(cyc2$time,cyc2$pitch,type='o')
min1 <- min(cyc2$pitch)
relation <- lm(cyc2$pitch~cyc2$time)
print(relation)
#----------------------------7th
cyc7 <- subset(data3,data3$time>29.10&data3$time<31.09)
plot(cyc7$time,cyc7$pitch,type='o')
min1 <- min(cyc7$pitch)
relation <- lm(cyc3$pitch~cyc3$time)
print(relation)
#---------------------------8th
cyc8 <- subset(data3,data3$time>31.09&data3$time<33)
plot(cyc8$time,cyc8$pitch,type='o')
min1 <- min(cyc8$pitch)
relation <- lm(cyc8$pitch~cyc8$time)
print(relation)
#----------------------------9th
cyc9 <- subset(data3,data3$time>44.79&data3$time<46.80)
plot(cyc9$time,cyc9$pitch,type='o')
min1 <- min(cyc9$pitch)
relation <- lm(cyc9$pitch~cyc9$time)
print(relation)
#----------------------------10th
cyc10 <- subset(data3,data3$time>47.60&data3$time<50)
plot(cyc10$time,cyc10$pitch,type='o')
View(cyc10)
relation <- lm(cyc10$pitch~cyc10$time)
print(relation)
#----------------------------11th
cyc11 <- subset(data3,data3$time>50.44&data3$time<52.45)
plot(cyc11$time,cyc11$pitch,type='o')
View(cyc11)
relation <- lm(cyc11$pitch~cyc11$time)
print(relation)
#---------------------------12th
cyc12 <- subset(data3,data3$time>52.99&data3$time<55.30)
plot(cyc12$time,cyc12$pitch,type='o')
View(cyc12)
relation <- lm(cyc12$pitch~cyc12$time)
print(relation)
#----------------------------13th
cyc13 <- subset(data3,data3$time>56&data3$time<58.3)
plot(cyc13$time,cyc13$pitch,type='o')
View(cyc13)
relation <- lm(cyc13$pitch~cyc13$time)
print(relation)
#----------------------------14th
cyc14 <- subset(data3,data3$time>58.95&data3$time<60.96)
plot(cyc14$time,cyc14$pitch,type='o')
View(cyc14)
relation <- lm(cyc14$pitch~cyc14$time)
print(relation)
#----------------------------15th
cyc15 <- subset(data3,data3$time>62&data3$time<64.1)
plot(cyc15$time,cyc15$pitch,type='o')
View(cyc15)
relation <- lm(cyc15$pitch~cyc15$time)
print(relation)
#----------------------------16th
cyc16 <- subset(data3,data3$time>64.64&data3$time<66.61)
plot(cyc16$time,cyc16$pitch,type='o')
View(cyc16)
relation <- lm(cyc16$pitch~cyc16$time)
print(relation)
#---------------------------17th
cyc17 <- subset(data3,data3$time>67.66&data3$time<69.32)
plot(cyc17$time,cyc17$pitch,type='o')
View(cyc17)
relation <- lm(cyc17$pitch~cyc17$time)
print(relation)
#----------------------------18th
cyc18 <- subset(data3,data3$time>69.72&data3$time<72.5)
plot(cyc18$time,cyc18$pitch,type='o')
View(cyc18)
relation <- lm(cyc18$pitch~cyc18$time)
print(relation)
#----------------------------------
t <- 29.10:31.64;
dataa <- -16.93005556*t+172






