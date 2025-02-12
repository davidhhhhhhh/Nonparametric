# 1/23/25
a = read.csv("Moon.csv")
dim(a)
names(a)
diff = a$Moon - a$Other
# q1a
t.test(diff, mu=0, alternative = "greater")

# check normality assumption 
shapiro.test(diff)
qqnorm(diff)
qqline(diff)

# mu = 0 for signed test
x = sum(diff > 0)
# pbinom(15,15,0.5) means probability of success smaller or equal to 15
p = 1- pbinom(13, 15, 0.5)

# use BSDA package
library(BSDA)
SIGN.test(diff, alternative = "greater")

# sort diff
diff_sort = sort(diff)

# to get confidence interval do two.sided 
SIGN.test(diff)

# simulation, N is number of repeats
TsignSize = function (n, N, dist){
  sizeT = 0; sizeSign = 0
  library(BSDA); mExp = qexp(0.5, rate=1)
  for (i in 1:N){
    if (dist == "normal"){
      y = rnorm(n)
      pt = t.test(y, alternative="less")$p.value
      psign = SIGN.test(y, alternative = "less")$p.value
    }
    else if (dist == "t"){
      y = rt(n, df = 3)
      pt = t.test(y, alternative="less")$p.value
      psign = SIGN.test(y, alternative = "less")$p.value
    }
    else {
      y = rexp(n, rate = 1)
      pt = t.test(y, mu = 1, alternative="less")$p.value
      psign = SIGN.test(y, md = mExp, alternative = "less")$p.value
    }
    if (pt <= 0.05){
      sizeT = sizeT + 1
    }
    if (psign <= 0.05){
      sizeSign = sizeSign + 1
    }
  }
  sizeT = sizeT / N; sizeSign = sizeSign / N
  result = list(Ttest = sizeT, SignTest = sizeSign)
  return(result)
}

# sample size 20
set.seed(1)
TsignSize(20, 10000, "normal")
set.seed(2)
TsignSize(20, 10000, "t")
set.seed(3)
TsignSize(20, 10000, "exp")

# sample size 40
set.seed(4)
TsignSize(40, 10000, "normal")
set.seed(5)
TsignSize(40, 10000, "t")
set.seed(6)
TsignSize(40, 10000, "exp")

# power plot for normal distribution 
TSignPowerNorm = function(n, N, alt){
  m = length(alt)
  powerT = rep(0, m); powerSign = rep(0,m)
  library(BSDA)
  for (j in 1:m){
    for (i in 1:N){
      y = rnorm(n, mean = alt[j])
      pt = t.test(y, alternative="less")$p.value
      psign = SIGN.test(y, alternative = "less")$p.value
      if (pt <= 0.05){
        powerT[j] = powerT[j] + 1
      }
      if (psign <= 0.05){
        powerSign[j] = powerSign[j] + 1
      }
    }
  }
  powerT = powerT / N; powerSign = powerSign / N
  result = list(Ttest = powerT, SignTest = powerSign)
  plot(alt, powerT, type="l", xlab="Alternative", ylab="power", main="alternative v.s. power")
  lines(alt, powerSign, col="red", lty=2)
  legend("topright", legend=c("t test", "sign test"), lty=c(1,2), col=c("black", "red"), lwd=c(2,2))
}

set.seed(1)
TSignPowerNorm(40, 10000, seq(-1.2,0,0.02))

# power plot for t distribution with df3
TSignPowerT = function(n, N, alt){
  m = length(alt)
  powerT = rep(0, m); powerSign = rep(0,m)
  library(BSDA)
  for (j in 1:m){
    for (i in 1:N){
      y = rt(n,3) + alt[j]
      pt = t.test(y, alternative="less")$p.value
      psign = SIGN.test(y, alternative = "less")$p.value
      if (pt <= 0.05){
        powerT[j] = powerT[j] + 1
      }
      if (psign <= 0.05){
        powerSign[j] = powerSign[j] + 1
      }
    }
  }
  powerT = powerT / N; powerSign = powerSign / N
  result = list(Ttest = powerT, SignTest = powerSign)
  plot(alt, powerT, type="l", xlab="Alternative", ylab="power", main="Alternative v.s. Power for t-3 distribution")
  lines(alt, powerSign, col="red", lty=2)
  legend("topright", legend=c("t test", "sign test"), lty=c(1,2), col=c("black", "red"), lwd=c(2,2))
}

set.seed(1)
TSignPowerT(20, 10000, seq(-1.2,0,0.02))

# wilcox 
dsignrank(0:6,n=3)

# poiner estimate
wilcox.test(diff, alternative = "greater", conf.int = TRUE)

# find walsh average
library(Rfit)
walsh(diff)
sort(walsh(diff))[26]
sort(walsh(diff))[95]

