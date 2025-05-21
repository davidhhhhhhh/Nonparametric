# HW3
#Q1
a = read.csv("Toad.csv")
wilcox.test(a$Length, mu = 0.12, alternative = "less")

# find size for wilcoxen testn-----
tSignWilcoxSize = function (n, N, dist){
  sizeT = 0; sizeSign = 0; sizeWil = 0
  library(BSDA)
  for (i in 1:N){
    if (dist == "normal"){
      y = rnorm(n)
      pt = t.test(y, alternative="less")$p.value
      psign = SIGN.test(y, alternative = "less")$p.value
      pwil = wilcox.test(y, alternative = "less")$p.value
    }
    else if (dist == "t"){
      y = rt(n, df = 3)
      pt = t.test(y, alternative="less")$p.value
      psign = SIGN.test(y, alternative = "less")$p.value
      pwil = wilcox.test(y, alternative = "less")$p.value
    }
    else {
      y = runif(n, -1, 1)
      pt = t.test(y, mu = 0, alternative="less")$p.value
      psign = SIGN.test(y, md = 0, alternative = "less")$p.value
      pwil = wilcox.test(y, mu = 0, alternative = "less")$p.value
    }
    if (pt <= 0.05){
      sizeT = sizeT + 1
    }
    if (psign <= 0.05){
      sizeSign = sizeSign + 1
    }
    if (pwil <= 0.05){
      sizeWil = sizeWil + 1
    }
  }
  sizeT = sizeT / N; sizeSign = sizeSign / N; sizeWil = sizeWil / N
  result = list(Ttest = sizeT, SignTest = sizeSign, WilcoxTest = sizeWil)
  return(result)
}

# sample size 20
set.seed(1)
tSignWilcoxSize(20, 10000, "normal")
tSignWilcoxSize(20, 10000, "t")
tSignWilcoxSize(20, 10000, "uniform")

# sample size 40
set.seed(2)
tSignWilcoxSize(40, 10000, "normal")
tSignWilcoxSize(40, 10000, "t")
tSignWilcoxSize(40, 10000, "uniform")

# power curve for normal distribution ----
TSignWilcoxPowerNorm = function(n, N, alt){
  m = length(alt)
  powerT = rep(0, m); powerSign = rep(0,m); powerWil = rep(0,m)
  library(BSDA)
  for (j in 1:m){
    for (i in 1:N){
      y = rnorm(n, mean = alt[j])
      pt = t.test(y, alternative="less")$p.value
      psign = SIGN.test(y, alternative = "less")$p.value
      pwil = wilcox.test(y, alternative = "less")$p.value
      if (pt <= 0.05){
        powerT[j] = powerT[j] + 1
      }
      if (psign <= 0.05){
        powerSign[j] = powerSign[j] + 1
      }
      if (pwil <=0.05){
        powerWil[j] = powerWil[j] + 1
      }
    }
  }
  powerT = powerT / N; powerSign = powerSign / N; powerWil = powerWil / N
  plot(alt, powerT, type="l", xlab="Alternative", ylab="power", main="alternative v.s. power for normal distribution")
  lines(alt, powerSign, col="red", lty=2)
  lines(alt, powerWil, col="green", lty=3)
  legend("topright", legend=c("t test", "sign test", "wilcoxon test"), lty=c(1,2,3), col=c("black", "red","green"), lwd=c(2,2,2))
}

set.seed(1)
TSignWilcoxPowerNorm(20, 10000, seq(-1.5,0,0.05))

# power curve for t distribution------ 
TSignWilcoxPowerT3 = function(n, N, alt){
  m = length(alt)
  powerT = rep(0, m); powerSign = rep(0,m); powerWil = rep(0,m)
  library(BSDA)
  for (j in 1:m){
    for (i in 1:N){
      y = rt(n,3) + alt[j]
      pt = t.test(y, alternative="less")$p.value
      psign = SIGN.test(y, alternative = "less")$p.value
      pwil = wilcox.test(y, alternative = "less")$p.value
      if (pt <= 0.05){
        powerT[j] = powerT[j] + 1
      }
      if (psign <= 0.05){
        powerSign[j] = powerSign[j] + 1
      }
      if (pwil <=0.05){
        powerWil[j] = powerWil[j] + 1
      }
    }
  }
  powerT = powerT / N; powerSign = powerSign / N; powerWil = powerWil / N
  plot(alt, powerT, type="l", xlab="Alternative", ylab="power", main="alternative v.s. power for t3 distribution")
  lines(alt, powerSign, col="red", lty=2)
  lines(alt, powerWil, col="green", lty=3)
  legend("topright", legend=c("t test", "sign test", "wilcoxon test"), lty=c(1,2,3), col=c("black", "red","green"), lwd=c(2,2,2))
}

set.seed(1)
TSignWilcoxPowerT3(20, 10000, seq(-1.5,0,0.05))

# power curve for uniform distribution------ 
TSignWilcoxPowerUnif = function(n, N, alt){
  m = length(alt)
  powerT = rep(0, m); powerSign = rep(0,m); powerWil = rep(0,m)
  library(BSDA)
  for (j in 1:m){
    for (i in 1:N){
      y = runif(n, min=alt[j], max=1)
      pt = t.test(y, alternative="less")$p.value
      psign = SIGN.test(y, alternative = "less")$p.value
      pwil = wilcox.test(y, alternative = "less")$p.value
      if (pt <= 0.05){
        powerT[j] = powerT[j] + 1
      }
      if (psign <= 0.05){
        powerSign[j] = powerSign[j] + 1
      }
      if (pwil <=0.05){
        powerWil[j] = powerWil[j] + 1
      }
    }
  }
  powerT = powerT / N; powerSign = powerSign / N; powerWil = powerWil / N
  plot(alt, powerT, type="l", xlab="Alternative", ylab="power", main="alternative v.s. power for uniform distribution")
  lines(alt, powerSign, col="red", lty=2)
  lines(alt, powerWil, col="green", lty=3)
  legend("topright", legend=c("t test", "sign test", "wilcoxon test"), lty=c(1,2,3), col=c("black", "red","green"), lwd=c(2,2,2))
}

set.seed(1)
TSignWilcoxPowerUnif(20, 10000, seq(-0.9,0,0.05))