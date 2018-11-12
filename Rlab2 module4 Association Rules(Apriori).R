#install the package and load the libraries

#install.packages("arules")
library('arules')
txn= read.transactions("MBAdata.csv",rm.duplicates = FALSE,format="single",sep=",",cols=c(1,2))
#inspect transaction data
head(txn,n=5)
txn
txn@itemInfo
image(txn)
#mine association rules
basket_rules = apriori(txn,parameter = list(sup=0.5,conf=0.9,target="rules"))
inspect(basket_rules)
#read in groceries data
data(Groceries)
Groceries
Groceries@itemInfo
#mine rules
rules=apriori(Groceries,parameter = list(support=0.001,confidence=1))
#Extract rules with confidane =0.8
#subrules=rules[quality(rules)$confidance>0.8]
inspect(subrules)

subrules=apriori(Groceries,parameter = list(support=0.001,confidence=0.5))
inspect(subrules)

#extract the top 3 rulkes with high lift
rules_high_lift = head(sort(rules,by="lift"),3)
inspect(rules_high_lift)
