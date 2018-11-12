setwd("C:/Users/pt2/Documents/Personal/R prcatice/module3")
ls()
library(RODBC)
thejaswini_db<-odbcConnect("mysql",uid="root")
result1 <- data.frame(sqlQuery(thejaswini_db, "SELECT * from hp.rq_rol_x_buss1"))


head(result1,n=10)

summary(result1)
nlab1 = result1[,2:4]

nlab1=data.frame(result1$Requestor_Role,result1$Open_1st_shift)
names(nlab1)=c("Requestor_Role","Open_1st_shift")
dim(nlab1)
typeof(nlab1)
class(nlab1)

summary(nlab1)
cor(result1)
rm(result1)
result1 = nlab1
save(result1, file="result.csv")
rm(nlab1)
ls()

n=1 #scalar variable


