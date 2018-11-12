#https://campus.datacamp.com/courses/introduction-to-spark-in-r-using-sparklyr/light-my-fire-starting-to-use-spark-with-dplyr-syntax?ex=4
setwd("~/Personal/Study/Data science Associate/R prcatice/Introduction to Spark in R using Sparklyr")

#----------------------------------------------------------------------------------
#The connect-work-disconnect pattern
#----------------------------------------------------------------------------------
install.packages("sparklyr")

# Load sparklyr
library(sparklyr)

# Connect to your Spark cluster
spark_conn <- spark_connect("local")

# Print the version of Spark
sc=spark_version(spark_conn)

# Disconnect from Spark
sc= spark_disconnect(spark_conn)
#----------------------------------------------------------------------------------
#Copying data into Spark
#----------------------------------------------------------------------------------
