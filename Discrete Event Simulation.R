 setwd("C:/Users/pt2/Documents/Personal/Study/Data Science Specialist/Module 5")
 
 #using the rnorm function 
 #generate the vector of 10,000 random normaly distributed 
 #values with a mean of 10 and a standard deviation of 5
 
 set.seed(654655)
 v1 = rnorm(10000,10,5)
 
 #validate the normality of the values
 #with a histogram and normal Q-Q plot
 
 hist(v1)
 qqnorm(v1)
 
 #generate a second vector of 10,000 random normally distibuted
 #values using the same mean and standard deviations.
 #but with a different seed 
 set.seed(54635756)
 v2 = rnorm(10000,10,5)
 
 #examine the relationship between these two vectors 
 plot(v1,v2)
 sum(v1==v2)
 cor(v1,v2)
 
 #observe that the same data set is generated 
 #when the same seed is used to generate each set
 set.seed(654655)
 v3 = rnorm(10000,10,5)
 set.seed(654655)
 v4 = rnorm(10000,10,5)
 
 sum(v3==v4)
 plot(v3,v4)
 cor(v3,v4)
 
 #observe that when the same seed is used to generate each 
 #but the distribution have different means and standard deviations
 #the random numbers are perfectly corilated 
 set.seed(654655)
 v5=rnorm(10000,10,5)
 set.seed(654655)
 v6=rnorm(10000,0,2)
 
 sum(v5==v6)
 hist(v5)
 hist(v6)
 plot(v5,v6)
 cor(v5,v6)
 
 # display the current content of the seed
 .Random.seed
 
 #display the help information relative to random number generator
 ?Random
 
#######################################################
# model a single server queue
# Such as for an ATM machine
#######################################################

#specify the operating characteristics of the queue

#Time between customer arrivals are exponentialy distributed with the following 
#customer arrival rate(customer arrival per hour)
arrival_rate = 10
 
#the time to perform the service is uniformly distributed
#-- between 1 and 6 minutes converted to hours to keep units aligned with the arrival_rate
min_service_time=1/60
max_service_time=6/60

#number of customers for the simulation 
num_of_custs = 50000

#randomly generate the time between customer arrivals
set.seed(238947123)
arrivals=rexp(num_of_custs,arrival_rate)

#view the distribution of the arrival times
hist(arrivals*60)

#randomly generate the service times for each customer
set.seed(34556463)
serv_times=runif(num_of_custs,min=min_service_time,max=max_service_time)

#view the distibution of the service times
hist(serv_times)
hist(serv_times,breaks=((2:12)*.5)/60)

#accumulate the arrival times
#to obtain the orderd arrival timestamps
cumul_arrivals=cumsum(arrivals)
arrivals[1:5]
cumul_arrivals[1:4]

#build the vector to store the completion time of customor the compleation time will be determined
#by the customer's cumulative arrival time,time in queue, and the customer's service time
compl_time=vector(mode='numeric',length=num_of_custs)

#add first customer to the system
compl_time[1]=cumul_arrivals[1]+serv_times[1]

#determine the remaining customers' completion times if a customer arrives after all previous 
#customers have been serviced then the customer is serviced right away otherwise the customer
#begin service after the previous customer

for (i in 2:num_of_custs) {
  compl_time[i]=max(cumul_arrivals[i],compl_time[i-1])+serv_times[i]
}
 
#determine time spent in the queue
time_in_queue = compl_time-(cumul_arrivals+serv_times)

#determine the queue size after each customer's arrival
queue_size=vector(mode="integer",length=num_of_custs)
queue_size[1]=0
for (i in 2:num_of_custs){
 queue_size[i]=sum(compl_time[1:i-1]>cumul_arrivals[i])
}

#analyze the queue size 
summary(queue_size)
hist(queue_size,breaks = 0:15, right=FALSE)
#probabilty that a customer will not have to wait in the queue
sum(queue_size==0)/num_of_custs

#analyze the customer's time in a queue (in minutes)
summary(time_in_queue*60)
hist(time_in_queue*60)

#analyze the customers' non-zero time in a queue (in minutes)
summary(time_in_queue[time_in_queue>0]*60)
hist(time_in_queue[time_in_queue>0]*60)

#analyze the time each customer spent in the system(in minutes)
summary((time_in_queue+serv_times)*60)
hist((time_in_queue+serv_times)*60)
plot(density((time_in_queue+serv_times)*60))

#determine the probabilty a customer will have to wait more than 10 minutes to complete a 
#transaction 
sum(((time_in_queue+serv_times)*60)>10)/num_of_custs











 
 
 
 
 
 
 