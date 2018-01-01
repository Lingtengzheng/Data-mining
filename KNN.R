#prepare
rm(list=ls())

#load file
dsn <- read.csv("ha.csv")
View(dsn)

#calculate a avg value to substitute na value
dsn2 = na.omit(dsn)
View(dsn2)

#normalize the data
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}
#normalize <- function(x){ return ((x-min(x)) / (max(x)-min(x)))}
#newdata <- as.data.frame(lapply(dsn,normalize))

#create new file (use the number of bedroom and bathroom and distance to school and rent to speculate the level of house)
newdata<-cbind(  bed=mmnorm(dsn2[,3],min(dsn2[,3]),max(dsn2[,3]))
                 ,bath=mmnorm(dsn2[,4],0,max(dsn2[,4] ))
                 ,distance=mmnorm(dsn2[,9],min(dsn2[,9]),max(dsn2[,9] ))
                 ,rent=mmnorm(dsn2[,6],min(dsn2[,6]),max(dsn2[,6] ))
                 ,dsn2[,8]
)

#create train and test seq
idx=seq(from=1,to=nrow(newdata),by=15)

test<-newdata[idx,]
training<-newdata[-idx,]

#introduct library
library(class)

#use knn
predict<-knn(training[,-5],test[,-5],training[,5],k=1)

#combine the prediction with the test data
results<-cbind(test, predict)

table(Actual=results[,5],Prediction=results[,6])

#calculate the wrong rate
wrong<-results[,5]!=results[,6]

rate<-sum(wrong)/length(wrong)

rate

#use knn
predict<-knn(training[,-5],test[,-5],training[,5],k=3)

#combine the prediction with the test data
results<-cbind(test, predict)

table(Actual=results[,5],Prediction=results[,6])

#calculate the wrong rate
wrong<-results[,5]!=results[,6]

rate<-sum(wrong)/length(wrong)

rate

#use knn
predict<-knn(training[,-5],test[,-5],training[,5],k=5)

#combine the prediction with the test data
results<-cbind(test, predict)

table(Actual=results[,5],Prediction=results[,6])

#calculate the wrong rate
wrong<-results[,5]!=results[,6]

rate<-sum(wrong)/length(wrong)

rate

#create new file(use the number of bedroom and bathroom and the distance to school and the level of the house
#to speculate the rent)
newdata<-cbind(  bed=mmnorm(dsn2[,3],min(dsn2[,3]),max(dsn2[,3]))
                 ,bath=mmnorm(dsn2[,4],0,max(dsn2[,4] ))
                 ,distance=mmnorm(dsn2[,9],min(dsn2[,9]),max(dsn2[,9] ))
                 ,level=mmnorm(dsn2[,8],min(dsn2[,8]),max(dsn2[,8] ))
                 ,dsn2[,6]
)

idx=seq(from=1,to=nrow(newdata),by=15)

test<-newdata[idx,]
training<-newdata[-idx,]

#use knn
predict<-knn(training[,-5],test[,-5],training[,5],k=1)

#combine the prediction with the test data
results<-cbind(test, predict)

table(Actual=results[,5],Prediction=results[,6])

#calculate the wrong rate
wrong<-results[,5]!=results[,6]

rate<-sum(wrong)/length(wrong)

rate

#use knn
predict<-knn(training[,-5],test[,-5],training[,5],k=3)

#combine the prediction with the test data
results<-cbind(test, predict)

table(Actual=results[,5],Prediction=results[,6])

#calculate the wrong rate
wrong<-results[,5]!=results[,6]

rate<-sum(wrong)/length(wrong)

rate

#use knn
predict<-knn(training[,-5],test[,-5],training[,5],k=5)

#combine the prediction with the test data
results<-cbind(test, predict)

table(Actual=results[,5],Prediction=results[,6])

#calculate the wrong rate
wrong<-results[,5]!=results[,6]

rate<-sum(wrong)/length(wrong)

rate



