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

# density estimation r code, notice bandwitdth for r need to be adjusted
try1 = density(a$Score, kernel="r",bw=0.1/sqrt(12))
try1$x[180]
try1$y[180]
plot(try1)
points(try1$x[180],try1$y[180], col = "red", pch = 19)

# plots for bandwidth difference 
try1 = density(a$Score, kernel="r",bw=0.1/sqrt(12))
try2 = density(a$Score, kernel="r",bw=0.2/sqrt(12))
try3 = density(a$Score, kernel="r",bw=0.5/sqrt(12))
hist(a$Score, freq = FALSE, main = "different bandwidth plots", ylim = c(0,5))
lines(try1, col="black")
lines(try2, col = "red")
lines(try3, col = "green")
legend("topright", legend = c("bw 0.1", "bw 0.2","bw 0.5"), 
       col = c("black", "red", "green"), lwd=c(2,2))

# plots for kernel difference 
try4 = density(a$Score, kernel="g",bw=0.25/sqrt(12))
try5 = density(a$Score, kernel="e",bw=0.25/sqrt(12))
try6 = density(a$Score, kernel="t",bw=0.25/sqrt(12))
hist(a$Score, freq = FALSE, main = "different kernel plots", ylim = c(0,5))
lines(try4, col="black")
lines(try5, col = "red")
lines(try6, col = "green")
legend("topright", legend = c("gaussian", "epanechnikov","triangular"), 
       col = c("black", "red", "green"), lwd=c(2,2))

# plots try different bandwidth selection
try7 = density(a$Score, kernel="g",bw="nrd")
try8 = density(a$Score, kernel="g",bw="bcv")
try9 = density(a$Score, kernel="g",bw="sj")
hist(a$Score, freq = FALSE, main = "different bw selection plots", ylim = c(0,5))
lines(try7, col="black")
lines(try8, col = "red")
lines(try9, col = "green")
legend("topright", legend = c("nrd", "bcv","sj"), 
       col = c("black", "red", "green"), lwd=c(2,2))
