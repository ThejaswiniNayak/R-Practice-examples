mcs= c(1,2,4,8,16,24)
cesd= c(3,4,5,6,7,8)


plot(mcs ~ cesd)
abline(lm(mcs ~ cesd))
lines(lowess(mcs ~ cesd))
