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

# wilcoxon test example
experiment = c(15, 18, 8, 15, 17, 16, 13)
control = c(10, 5, 4, 9, 12, 6, 7)
t.test(experiment, control, var.equal = TRUE, alternative = "greater")
wilcox.test(experiment, control, alternative = "greater")

# conf int
t.test(experiment, control, var.equal = TRUE, conf.level = 0.95)
wilcox.test(experiment, control, conf.int = TRUE, conf.level = 0.95)

# Compare pooled T test and Wilcoxon test size and power -----
# simulation, N is number of repeats
pooledTWilcoxonSize = function (n, m, N, dist){
  sizeT = 0; sizeWilcoxon = 0
  library(BSDA); mExp = qexp(0.5, rate=1)
  for (i in 1:N){
    if (dist == "normal"){
      y = rnorm(n)
      x = rnorm(m)
      pt = t.test(x, y, var.equal = TRUE)$p.value
      pWil = wilcox.test(x, y)$p.value
    }
    else if (dist == "t"){
      y = rt(n, df = 3)
      x = rt(m, df = 3)
      pt = t.test(x, y, var.equal = TRUE)$p.value
      pWil = wilcox.test(x, y)$p.value
    }
    else {
      y = rexp(n, rate = 1)
      x = rexp(m, rate = 1)
      pt = t.test(x, y, var.equal = TRUE)$p.value
      pWil = wilcox.test(x, y)$p.value
    }
    if (pt <= 0.05){
      sizeT = sizeT + 1
    }
    if (pWil <= 0.05){
      sizeWilcoxon = sizeWilcoxon + 1
    }
  }
  sizeT = sizeT / N; sizeWilcoxon = sizeWilcoxon / N
  result = list(Ttest = sizeT, WilcoxTest = sizeWilcoxon)
  return(result)
}

# equal sample sizes 
set.seed(1);
pooledTWilcoxonSize(20, 20, 10000, "normal");
set.seed(1);
pooledTWilcoxonSize(20, 20, 10000, "t");
set.seed(1);
pooledTWilcoxonSize(20, 20, 10000, "exp");

# equal sample sizes 
set.seed(2);
pooledTWilcoxonSize(20, 30, 10000, "normal");
set.seed(2);
pooledTWilcoxonSize(20, 30, 10000, "t");
set.seed(2);
pooledTWilcoxonSize(20, 30, 10000, "exp");

# conclusion: two samples both wilcox and t are robust 
# and have good size

# power 
# power plot for normal distribution 
TSWilcoxPowerNorm = function(num_x, num_y, N, alt, distr){
  m = length(alt)
  powerT = rep(0, m); powerWilcox = rep(0,m)
  x = rep(0, num_x); y = rep(0, num_y)
  library(BSDA)
  for (j in 1:m){
    for (i in 1:N){
      if (distr == "Normal"){
        x = rnorm(num_x)
        y = rnorm(num_y, mean = alt[j])
      }
      else if (distr == "t3"){
        x = rt(num_x, df=3)
        y = rt(num_y, df=3) + alt[j]
      }
      else {
        x = rexp(num_x, 1)
        y = rexp(num_y, 1) + alt[j]
      }
      pt = t.test(x, y, var.equal = TRUE)$p.value
      pWil = wilcox.test(x, y)$p.value
      if (pt <= 0.05){
        powerT[j] = powerT[j] + 1
      }
      if (pWil <= 0.05){
        powerWilcox[j] = powerWilcox[j] + 1
      }
    }
  }
  powerT = powerT / N; powerWilcox = powerWilcox / N
  plot(alt, powerT, type="l", xlab="Alternative", ylab="power", ylim = c(0,1),main=distr)
  lines(alt, powerWilcox, col="red", lty=2)
  legend("top", legend=c("t test", "wilcox test"), lty=c(1,2), col=c("black", "red"), lwd=c(2,2))
}

alt = seq(-1.2, 1.2, 0.1)
TSWilcoxPowerNorm(20, 20, 10000, alt, "Normal")
TSWilcoxPowerNorm(20, 20, 10000, alt, "t3")
TSWilcoxPowerNorm(20, 20, 10000, alt, "exp")

# Conclusion if distribution is normal, t test has slightly more power than Wilcoxon
# if distribution is not normal, Wilcoxon test has higher power 
# for wilcoxon, higher variance of original distribution means less power 

# Fligner-Policello test
library(NSM3)
pFligPoli(x,y, method = "Asymptotic")
