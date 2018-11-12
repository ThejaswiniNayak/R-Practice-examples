# set the working directry
# install the library
install.packages("rpart.plot")
library("rpart")
library("rpart.plot")
# predict in given condition play or not (drivers are temperature,humidty and wind)
Play_decision=read.table("DTdata.csv",header=TRUE,sep=",")
Play_decision
summary(Play_decision)
#Build the tree to "fit" the model
fit=rpart(Play ~ Outlook+Temperature+Humidity+Wind, method="class",data=Play_decision,
          control=rpart.control(mindsplit=1))
summary(fit)
#plot the tree
rpart.plot(fit,type=4,extra=1)
