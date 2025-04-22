# Density Estimation 
# Ex 15
a = read.csv("SpatialAbility.csv")

# empirical distribution plot 
plot.ecdf(a$Score)

# histograms
par(mfrow=c(2,2))
hist(a$Score, freq = FALSE)
hist(a$Score, breaks="fd", freq = FALSE)
hist(a$Score, breaks="scott", freq = FALSE)
hist(a$Score, breaks=seq(0.1,0.9,0.1), freq = FALSE)
