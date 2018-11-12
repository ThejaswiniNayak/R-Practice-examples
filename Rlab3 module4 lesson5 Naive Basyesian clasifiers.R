install.packages("e1071")
library("e1071")
# read the data into a table from the file
sample=read.table("sample1.csv",header=TRUE,sep=",")
# we will now define the data frames to use the NB classifier
traindata=as.data.frame(sample[1:14,])
testdata=as.data.frame(sample[15,])
# disply data frames
traindata
testdata
#build the model manually
tprior=table(traindata$Enrolls)
tprior=tprior/sum(tprior)
tprior
ageCounts=table(traindata[,c("Enrolls","Age")])
ageCounts=ageCounts/rowSums(ageCounts)
ageCounts
incomeCounts=table(traindata[,c("Enrolls","Income")])
incomeCounts=incomeCounts/rowSums(incomeCounts)
jsCounts=table(traindata[c("Enrolls","Jobsatisfaction")])
jsCounts=jsCounts/rowSums(jsCounts)
desireCounts=table(traindata[c("Enrolls","Desire")])
desireCounts=desireCounts/rowSums(desireCounts)
#predict -Compute the probabilities for age<=30,Income=Medium
#jobsatifaction   =yes and Dsire =Fair
pyes=
     ageCounts["Yes","<=30"]*
     incomeCounts["Yes","Medium"]*
     jsCounts["Yes","Yes"]*
     desireCounts["Yes","Fair"]*
     tprior["Yes"]

pno=
    ageCounts["No","<=30"]*
    incomeCounts["No","Medium"]*
    jsCounts["No","Yes"]*
    desireCounts["No","Fair"]*
    tprior["No"]
print(pyes)
print(pno)
print(max(pyes,pno))
# use the NB classifier
model=naiveBayes(Enrolls~.,traindata)
# display model
model
#predict with test data
results=predict(model,testdata)
#display results
results
# use the NB classifier lapalce smoothing
model1=naiveBayes(Enrolls~.,traindata,laplace = .01)
# display model
model1
#predict with test data
results1=predict(model1,testdata)
#display results
results1
   
     
