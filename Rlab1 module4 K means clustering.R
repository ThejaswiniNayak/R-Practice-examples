setwd("~/Personal/Study/R prcatice/Module4")

library(RODBC)

thejaswini_db<-odbcConnect("mysql",uid="root")
result1 <- data.frame(sqlQuery(thejaswini_db, "select * from hp.rq_rol_x_buss1"))
sqlDrop(thejaswini_db,hp.Role_wise_Opportunities)
sqlQuery(thejaswini_db,
         "create table hp.Role_wise_Opportunities as
         select Requestor_Role,sum(opps_sum) as Oppsum
         from hp.rq_rol_x_buss1 group by Requestor_Role;")
sqlColumns(thejaswini_db,"hp.Role_wise_Opportunities")
# put the data in numaric data
sum_of_ticket = as.matrix(sqlFetch(thejaswini_db,"hp.Role_wise_Opportunities",
                                   rownames="Requestor_Role"))
summary(sum_of_ticket)
sum_of_ticket <- sort(sum_of_ticket)

help("kmeans")
#fit the k-means clusters with 3 initial cluster centers
km=kmeans(sum_of_ticket,3,15)
km
km$cluster
km$size
km$centers
#plot clusters 
plot(sum_of_ticket,col=km$cluster)
#plot centers
plot(km$centers,col=1:3,pch=8)

#plot the within_group_Sum of squares
#look for elbow of the plot
#the "elbow" of the plot tells what the appropriate(optimum) value of clustors.
wss=numeric(10)
for(i in 1:10) wss[i] =sum(kmeans(sum_of_ticket,centers = i)$withinss)
plot(1:10,wss,type = "b",xlab="Number of clusters",
     ylab="within group sum of squares")

# we get the elbow at 3 so 3 is the optimal number of clusters which we can use
# based on elbow values we can change the K value to get appropriate number of clusters

odbcClose(thejaswini_db)








