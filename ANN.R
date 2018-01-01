rm(list = ls())

########  Project: CS513-Final Project ########
########  Name: DDMUH                  ########
########  Date: 12/12/2016             ########
########  Function: ANN                ########

test = read.csv("all.csv")
View(test)

library("neuralnet")
?neuralnet

#Choose columns of variable which I will use
#They are Bedrooms, bathrooms, Squares, Level and Distance
input = test[, c(3,4,5,8,9)]
output = test[, c(6)]
data <- cbind(input,output)

#Ignore the NA rows
data.new = na.omit(data)

View(data.new)

#Normalize data
normalize <- function(x){ return ((x-min(x))/(max(x)-min(x)))}
data_norm <- as.data.frame(lapply(data.new,normalize))

#Add title of each column
colnames(data_norm) <- c("Bed","Bath","Sqr","Level","Dis","Rent")
View(data_norm)

#Set training data and test data
data_train <- data_norm[1:446,]
data_test <- data_norm[447:478,]
View(data_test)


#First time, use simple ANN model
data_model <- neuralnet(Rent ~ Bed+Bath+Sqr+Level+Dis,data=data_train,hidden=5)
plot(data_model)

#Test linear correlation
model_results <- compute(data_model,data_test[1:5])
predicted_rent <- model_results$net.result
cor(predicted_rent,data_test$Rent)



#Improve the ANN model: Back Propagation, 2 Hidden Layers, Repeat 3 times.
data_bp <- neuralnet(
  Rent ~ Bed+Bath+Sqr+Level+Dis,
  data=data_train,hidden=c(10,5), 
  linear.output = FALSE, learningrate = 0.01, rep = 1,
  algorithm = "backprop")

plot(data_bp)


#Test linear correlation
model_results <- compute(data_bp,data_test[1:5])
predicted_rent <- model_results$net.result
cor(predicted_rent,data_test$Rent)













