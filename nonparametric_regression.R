# Nonparametric Regression
library(lattice)
data(ethanol)

# a, varying span 
plot(ethanol$E, ethanol$NOx, main="Local Average with Different Spans", 
     xlab="Equivalence ratio", ylab = "nitrogen oxides")
lines(supsmu(x=ethanol$E, y = ethanol$NOx, bass = 0, span = 0.1), lwd =2, col="black", lty=1)
lines(supsmu(x=ethanol$E, y = ethanol$NOx, bass = 0, span = 0.4), lwd =2, col="red", lty=3)
lines(supsmu(x=ethanol$E, y = ethanol$NOx, bass = 0, span = 0.7), lwd =2, col="blue", lty=4)
lines(supsmu(x=ethanol$E, y = ethanol$NOx, bass = 0, span = "cv"), lwd =2, col="green", lty=5)
legend("topright", legend = c("span=0.1", "span=0.4", "span=0.7", "span=cv"), 
       col = c("black","red","blue", "green"), lwd=2, lty=c(1,3,4,5))
try1 = supsmu(x=ethanol$E, y = ethanol$NOx, bass = 0, span = 0.4)
length(try1$x)
length(try1$y)

# varying bass (larger bass, larger span)
plot(ethanol$E, ethanol$NOx, main="Local Average with Different Spans", 
     xlab="Equivalence ratio", ylab = "nitrogen oxides")
lines(supsmu(x=ethanol$E, y = ethanol$NOx, bass = 0, span = "cv"), lwd =2, col="black", lty=1)
lines(supsmu(x=ethanol$E, y = ethanol$NOx, bass = 6, span = "cv"), lwd =2, col="red", lty=2)
lines(supsmu(x=ethanol$E, y = ethanol$NOx, bass = 10, span = "cv"), lwd =2, col="blue", lty=3)

# local regression 
# plot weight function 
f1 = function(x){(1-abs(x^3))^3}
curve(f1, from=-1, to=1)
f2 = function(x){(1-x^2)^2}
curve(f2, from=-1, to=1)

# do the loess without sorting data
plot(ethanol$E, ethanol$NOx, main="Local Average with Different Spans", 
     xlab="Equivalence ratio", ylab = "nitrogen oxides")
out1 = loess(ethanol$NOx~ethanol$E, span=0.2)
lines(ethanol$E, predict(out1, ethanol$E))

# do loess with sorted data 
library(fANCOVA)
plot(ethanol$E, ethanol$NOx, main="Local Regression with Different Spans", 
     xlab="Equivalence ratio", ylab = "nitrogen oxides")
a = ethanol[order(ethanol$E),]
out2 = loess(a$NOx~a$E,span=0.75)
out3 = loess(a$NOx~a$E,span=0.1)
out4 = loess.as(x=a$E, y=a$NOx, criterion = "gcv")
ss = seq(min(ethanol$E), max(ethanol$E), 0.01)
# lines(sort(a$E), predict(out2, sort(a$E)), col="black", lty=1, lwd=2)
lines(sort(a$E), predict(out3, sort(a$E)), col="red", lty=2, lwd=2)
lines(sort(a$E), predict(out4, sort(a$E)), col="blue", lty=3, lwd=2)
lines(ss, predict(out2, ss), col="black", lty=1, lwd=2)
legend("topright", legend = c("span=0.75", "span=0.1","span=gcv"), 
       col = c("black","red","blue"), lwd=2, lty=c(1,2,3))

# Kernel Smoothing
library(np)
plot(ethanol$E, ethanol$NOx, main="Local Regression with Different Spans", 
     xlab="Equivalence ratio", ylab = "nitrogen oxides")
a = ethanol[order(ethanol$E),]
out5 = npreg(bws=0.01, ckertype="gaussian", txdat=a$E, tydat=a$NOx)
out6 = npreg(bws=0.03, ckertype="gaussian", txdat=a$E, tydat=a$NOx)
out7 = npreg(bws=0.05, ckertype="gaussian", txdat=a$E, tydat=a$NOx)
lines(a$E, fitted(out5, a$E), col="black", lty=1, lwd=2)
lines(a$E, fitted(out6, a$E), col="red", lty=2, lwd=2)
lines(sort(a$E), fitted(out7, sort(a$E)), col="blue", lty=3, lwd=2)
legend("topright", legend = c("span=0.01", "span=0.03","span=0.05"), 
       col = c("black","red","blue"), lwd=2, lty=c(1,2,3))

# Data Driven bw 
library(np)
plot(ethanol$E, ethanol$NOx, main="Local Regression with Different Spans", 
     xlab="Equivalence ratio", ylab = "nitrogen oxides")
a = ethanol[order(ethanol$E),]
out5 = npreg(ckertype="gaussian", txdat=a$E, tydat=a$NOx, bwmethod="cv.aic")
out6 = npreg(ckertype="gaussian", txdat=a$E, tydat=a$NOx, bwmethod="cv.ls")
out7 = npreg(bws=0.05, ckertype="gaussian", txdat=a$E, tydat=a$NOx)
lines(a$E, fitted(out5, a$E), col="black", lty=1, lwd=2)
lines(a$E, fitted(out6, a$E), col="red", lty=2, lwd=2)
lines(sort(a$E), fitted(out7, sort(a$E)), col="blue", lty=3, lwd=2)
legend("topright", legend = c("span=aic", "span=ls","span=0.05"), 
       col = c("black","red","blue"), lwd=2, lty=c(1,2,3))


