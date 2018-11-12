income = rlnorm(5000, meanlog=log(40000), sdlog=log(5))
#Plot for the top graphic:
plot(density(log10(income), adjust=0.5), main="Distribution of account values (log10 scale)")
rug(log10(income))