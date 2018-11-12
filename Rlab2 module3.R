setwd("C:/Users/pt2/Documents/Personal/R prcatice/module3")
ls()
library(RODBC)
thejaswini_db<-odbcConnect("mysql",uid="root")
result1 <- data.frame(sqlQuery(thejaswini_db, "SELECT * from hp.rq_rol_x_buss1"))

ds=result1
ls()

summary(result1$opps_sum)
range(result1$opps_sum)
sd(result1$opps_sum)
var(result1$opps_sum)
plot(density(result1$opps_sum))

plot(as.factor(result1$opps_sum))

# removing outlair
(m=mean(result1$opps_sum, trim=0.10))
ds=subset(result1,result1$opps_sum < 47)
summary(ds)
quantile(ds$opps_sum, seq(from=0, to=1, length=11))

#startifing/arranging/clasifing a variable
breaks = c(0,2,10,20,30,47)
range_grid = c("Very less","less","normal","high","Very high")
range_type = cut(ds$opps_sum,breaks,range_grid) # catogorize Opps_sum into range_grid
# add wealth  as a column to ds
ds = cbind(ds,range_type)

print(ds)

wt=table(range_type)
precent=wt/sum(wt)*100
wt=rbind(wt,precent)
wt
plot(wt)

nt=table(range_type,ds$Open_1st_shift)
print(nt)
plot(nt)

rm(range_type,range_grid,breaks,wealth,labels)
save(ds,wt,nt,file="categorising desnsity and frequency distribution.Rdata")

#histogarm and distributions
library(Mass)
with (ds, {
  hist(opps_sum, main="distribution of houseold Income", freq= FALSE)
  lines(density(opps_sum), lty=2, lwd=2)
  xvals=seq(from=min(opps_sum), to=max(opps_sum), length=100)
  param=fitdistr(opps_sum,"lognormal")
  lines(xvals, dlnorm(xvals, meanlog=param$estimate[1],
                      sdlog=param$estimate[2]), col="blue")
})

# same with log10(opps_sum)
logincome=log10(ds$opps_sum)
hist(logincome, main="distribution of houseold Income", freq= FALSE)
lines(density(logincome), lty=2, lwd=2)
xvals=seq(from=min(logincome), to=max(logincome), length=100)
param=fitdistr(logincome,"normal")
lines(xvals, dlnorm(xvals, meanlog=param$estimate[1],param$estimate[2]), lwd=2, col="blue")

#corilation
with(ds, cor(opps_sum,sqrt(opps_sum)))
with(ds, cor(log(opps_sum),opps_sum))
n=length(ds$opps_sum)
with(ds,cor(runif(n),opps_sum))

# plotting
with(ds,
     boxplot(opps_sum~ as.factor(Requestor_Role),data=ds, range=0,outline=F,log="y",
             xlab="#Requestor_role", ylab="Opps_sum"))

with(ds,
     boxplot(opps_sum~ range_type,data=ds,
             main="Opps_sum by range_type", xlab="#Requestor_role", ylab="Opps_sum"))





