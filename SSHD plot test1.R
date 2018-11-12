setwd("C:/Users/pt2/Documents/Personal/R prcatice/module3")
ls()
library(RODBC)
thejaswini_db<-odbcConnect("MySQL",uid="root")
result1 <- data.frame(sqlQuery(thejaswini_db, "SELECT * from hp.rq_rol_x_buss1"))

summary(result1)

counts <- table(result1$Requestor_Role, result1$Open_Report_Date)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

nlab1 = result1[1:25,1:7]

# Useful link:http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/

library(ggplot2)
c=ggplot(data=nlab1,aes(Open_Report_Date))
c+geom_bar(aes(fill = Requestor_Role),position = "dodge")

ggplot(data=nlab1, aes(x=Open_Report_Date,y=opps_sum,fill=Requestor_Role))+
  geom_bar(colour="black",width=.4,stat="identity",position=position_dodge(width=.5))+
  xlab("Open_date") + ylab("number of tickets") +
  ggtitle("shift x plot")

ggplot(data=nlab1, aes(x=Open_Report_Date,y=opps_sum,fill=Requestor_Role,label=opps_sum))+
  geom_bar(colour="black",width=.8,stat="identity",position = "dodge")+
  xlab("Open_date") + ylab("number of tickets") +
  ggtitle("shift x plot")+
  geom_text(aes(opps_sum = opps_sum + 0.05), position = position_dodge(0.9), vjust = 0)+
  scale_x_discrete()


nlab1 <- transform(nlab1, 
      mid_y = ave(nlab1$opps_sum, nlab1$Open_Report_Date,
                  FUN = function(val) cumsum(val) - (0.5 * val)))

ggplot(data = nlab1, aes(Open_Report_Date, opps_sum, fill = Requestor_Role, label = opps_sum)) +
  geom_bar(stat = "identity") +
  geom_text(aes(opps_sum = mid_y), vjust = 0)

