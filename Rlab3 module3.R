setwd("C:/Users/pt2/Documents/Personal/R prcatice/module3")

# ANova
offers=sample(c("noffer","offer1","offer2"),size=500,replace=T)
head(offers)
purchasesize=ifelse (offers=="noffer",rlnorm(500,meanlog=log(25)),
                     ifelse (offers=="offer1",rlnorm(500,meanlog=log(50)),
                             rlnorm(500,meanlog = log(55))))
head(purchasesize)
offertest=data.frame(offer=offers,purchase_amt=purchasesize)
head(offertest)
summary(offertest)

aggregate(x=offertest$purchase_amt,by=list(offertest$offer),FUN="mean")
# plot how purchase size is differs with 3 groups
boxplot(purchase_amt~as.factor(offers),data=offertest,log="y")

#use lm() to do the anova
model=lm(log10(purchase_amt)~as.factor(offers),data=offertest)
summary(model)

# use Tukeys's test to check all the differnece of means
TukeyHSD(aov(model))

# conclusions from above anova is use offers to increase the sales
# But thier is no diffrence in sales which we are going to get in diferent offers
#(offer1 and offer2)


#--------------------------------------------------------------------
#--------------------------------------------------------------------
#ploting with ggplot() and lattice()

#Lattice()
library(lattice)
densityplot(~purchase_amt,group=offers,data=offertest, auto.key=T)#left skewed
densityplot(~log10(purchase_amt),group=offers,data=offertest, auto.key=T)#plot comming middle

densityplot(~purchase_amt|offers,data=offertest)
densityplot(~log(purchase_amt)|offers,data=offertest)
barplot(~log(purcahse_amt)|offers,data=offertest)

#ggplot grammer of grafix
library(ggplot2)
ggplot(data=offertest,aes(x=as.factor(offers),y=purchase_amt))+
  geom_point(position = "jitter",alpha=0.2)+
  geom_boxplot(alpha=0.1,outlier.size=0)+
  scale_y_log10()

#--------------------------------------------------------------------------------------------
#doing a hypothesis 
#step1 generate the example data
x=rnorm(10)#distributions centered at 0
y=rnorm(2,10)#distributions centered at 2

#step2 create a function to caliculate the pooled variance
pooled.var=function(x,y){
  nx=length(x)
  ny=length(y)
  stdx=sd(x)
  stdy=sd(y)
  num=(nx-1)*stdx^2+(ny-1)*stdy^2
  denom=nx+ny-2 #degrees of freedom
  (num/denom)*(1/nx+1/ny)
}


#step3 examine the data
mx=mean(x)
my=mean(y)
mx-my
pooled.var(x,y)

#step4 caliculate the t statistic for student's t-test
tstat=(mean(x)-mean(y))/sqrt(pooled.var(x,y))
tstat

#step5 Calculate the degrees of feedom for out problem
dof=length(x)+length(y)-2
dof

#step6 the function pt(x,dof) gives the area under the curve

tailarea=pt(tstat,dof)

pvalue=2*tailarea

#step7 do students t-test directly ,and compare the results
t.test(x,y,var.equal=T)














