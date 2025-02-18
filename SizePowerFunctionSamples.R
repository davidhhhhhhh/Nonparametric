# One Sample Size Function ----
# n is number Sample, N repeats, dist for distrbution in String 
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

# One Sample Power Function-----
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
# Two Sample Size Function-----
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

# Two Sample Power Function -----
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