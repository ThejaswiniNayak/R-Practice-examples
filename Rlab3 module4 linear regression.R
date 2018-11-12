x=runif(100,0,10) #generates 100 numbers betwenn 0 and 10
y=5+6*x+rnorm(100) # y is 5+6x+rnorm(100) so beta=[5,6] and 
#sigma=1(rnorm create variables with mean 0 and SD 1)
plot(x,y)
#the OLS model
d=lm(y~x)
str(d) #to see the structure of data
print(d) # to get the Coefficients

par(mfrow=c(2,2))
plot(d)

ypred=predict(d)
par(mfrow=c(1,1))
plot(y,y,xlab="true y" ,ylab="predicted y")
points(y,ypred)
d1=summary(d)
#detailed model results
print(d1)
cat("OLS gave slope of",d1$coefficients[2,1],"and R square of",d1$r.squared,"\n")

#introducing slight non linearity and test the model
x1=runif(100)
y1=5+6*x+0.1*x1*x1+rnorm(100)
m=lm(y1~x1)
#craeting real values
x2=runif(100)
y2=5+6*x+0.1*x2*x2+rnorm(100)
# creating predicted values 
y2pred=predict(m,data.frame(x2))
par(mfrow=c(1,1))
plot(y2,y2,xlab="true y" ,ylab="predicted y")
points(y2,y2pred)





