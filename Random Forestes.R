setwd("C:/Users/pt2/Documents/Personal/Study/Data Science Specialist/Module 5")
#Read in Nursery Application Data from file system

#Data source:http://archive.ics.uci.edu/ml/datasets/Nursery

nurseryData=read.csv("Nursary Data.csv",as.is=TRUE)

#convert all features to factors
#this is needed for the Random forest algorithum to work in R
nurseryData[sapply(nurseryData,is.character)]=
  lapply(nurseryData[sapply(nurseryData, is.character)], as.factor)

###################################
#explore the dataset
###################################
summary(nurseryData)

#this reveals that there are only 2 observation with an apllication_rank of "recommend".
#To simplify the analysis these two observations will be combined with "very_recom"
levels(nurseryData$application_rank)[match("recommend",levels(nurseryData$application_rank))]="very_recom"
summary(nurseryData)

###################################
#Build a random forest
###################################
#install.packages("tree")
#install.packages("caret")
#install.packages("ggplot2")
library(tree)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

#setting a seed allows us to have reproductible results
set.seed(1260)

#build a forest of 100 trees with the number of variables at each split
fit=randomForest(application_rank ~ ., data=nurseryData, ntree=100)

help("randomForest")

print(fit)

#before we define a plotting configuration we need to set tha default
old.par = par(mar=c(0,0,0,0))

#plot OOB error estimate for the model
par(mar=c(4,4,4,4))
plot(fit,main=paste("Error Rate vs. #Trees (mtry=",fit$mtry,")"),
     type="l",
     col.main="black",
     lwd=2,
     lty=1)
legend(60,0.25,colnames(fit$err.rate),col=1.4,
       cex=0.8, fill=1:5, lwd=1, bty="n")

###################################
#Specify the value of the mtry=3
###################################
#increase the number of variables in each split ,cansider 3 as numbers of variable in each decistion
#tree split
fit2=randomForest(application_rank ~ ., data=nurseryData, mtry=3, ntree=100)
print(fit2)
par(mar=c(4,4,4,4))
plot(fit2,main=paste("Error Rate vs. #Trees (mtry=",fit2$mtry,")"),
     type="l",
     col.main="black",
     lwd=2,
     lty=1)
legend(60,0.25,colnames(fit2$err.rate),col=1.4,
       cex=0.8, fill=1:5, lwd=1, bty="n")

#Retrin the forest but this time we will strtify the data to ensure that each application ranking
#is equally represented in each tree Here we also introduce the "importance" parameter in the 
#building of the model. This will allow us to look at which features had the biggest influenace 
#on the buliding of the model
set.seed(1260)
fit3=randomForest(application_rank ~ ., data=nurseryData,importance=TRUE, ntree=100, mtry=3,
                  samsize=c('not_recom'=4320,'priority'=4266,
                            'very_recom'=330,'spec_prior'=4044))
print(fit3)

######################################
#explore the importance of the model
######################################
fit3$importance

###Health mhas_nurs,parents has more importance in the model



