rm(list = ls())

########  Project: CS513-Final Project  ########
########  Name: DDMUH                  ########
########  Date: 12/12/2016             ########
########  Function: Decision Tree                ########


# RandomForest
# Classification Tree with rpart
library(rpart)
library(randomForest)
# grow tree 
all <- read.csv("NewAll.csv")
new<-all[,c(1,2,3,6,7,8,9)]
fit <- randomForest(average ~ location+bedroom+bathroom+pet+style+distance, data = new)
print(fit) # view results 
importance(fit) # importance of each predictor

# Choose some factors which students think are important
# Classification Tree with rpart
library(rpart)
library(randomForest)
# grow tree 
all <- read.csv("NewAll.csv")
new<-all[,c(1,2,3,6,7,8,9)]
fit <- randomForest(average ~ bathroom+pet+style+distance, data = new)
print(fit) # view results 
importance(fit) # importance of each predictor



# Classification tree
rm(list=ls())
library(rpart)
raw.orig <- read.csv("NewAll.csv")
# Keep the dataset small and tidy
raw = raw.orig[,c(1,2,3,6,7,8,9)]
row.names(raw) = raw.orig$CASNumber
raw = na.omit(raw);
frmla = average ~ bathroom+pet+style+distance
fit = rpart(frmla, method="class", data=raw)
# display the results
printcp(fit)
# visualize cross-validation results
plotcp(fit) 
# detailed summary of splits
summary(fit) 
# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  	
# plot tree 
plot(fit, uniform=TRUE, main="Regression Tree for rent ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)



#Conditional inference trees(Choose some factors which students think are important)
rm(list=ls())
library("party") 
all <- read.csv("NewAll.csv")
new<-all[,c(1,2,3,6,7,8,9)]
str(new)
data_ctree<-ctree(average ~ bathroom+pet+style+distance, data=new )
print(data_ctree)
plot(data_ctree)
plot(data_ctree, type="simple")

#Conditional inference trees
rm(list=ls())
library("party") 
all <- read.csv("NewAll.csv")
new<-all[,c(1,2,3,6,7,8,9)]
str(new)
data_ctree<-ctree(average ~ location+bedroom+bathroom+pet+style+distance, data=new )
print(data_ctree)
plot(data_ctree)
plot(data_ctree, type="simple")



#C4.5(Choose some factors which students think are important)
rm(list=ls())
library(RWeka)
library(party)
hoboken <- read.csv("NewAll.csv")
newH<-hoboken[,c(1,2,3,6,7,8,9)]
data_j48 <- J48(average ~ bathroom+pet+style+distance, data = newH)
summary(data_j48)
plot(data_j48)

#C4.5
rm(list=ls())
library(RWeka)
library(party)
hoboken <- read.csv("NewAll.csv")
newH<-hoboken[,c(1,2,3,6,7,8,9)]
data_j48 <- J48(average ~ location+bedroom+bathroom+pet+style+distance, data = newH)
summary(data_j48)
plot(data_j48)