library(RODBC)
thejaswini_db<-odbcConnect("VCE_RAAS",uid="root")
result1 <- data.frame(sqlQuery(thejaswini_db, "SELECT * from hp.rq_rol_x_buss1"))
print(result1)

library(ggplot2)
# Generate data
library(plotly)
c <- ggplot(result1, aes(Requestor_Role,opps_sum))

# By default, uses stat="bin", which gives the count in each category
c + geom_bar()
counts <- table(mtcars$gear)
barplot(counts, main="Car Distribution", 
        xlab="Number of Gears")

counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)