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
Size = function (n, m, N, dist){
  size1 = 0; size2 = 0
  library(BSDA); mExp = qexp(0.5, rate=1)
  for (i in 1:N){
    if (dist == "normal"){
      y = rnorm(n)
      x = rnorm(m)
    }
    else if (dist == "t"){
      y = rt(n, df = 3)
      x = rt(m, df = 3)
    }
    else {
      y = rexp(n, rate = 1)
      x = rexp(m, rate = 1)
    }
    p1 = t.test(x, y, var.equal = TRUE)$p.value
    p2 = wilcox.test(x, y)$p.value
    if (p1 <= 0.05){
      size1 = size1 + 1
    }
    if (p2 <= 0.05){
      size2 = size2 + 1
    }
  }
  size1 = size1 / N; size2 = size2 / N
  result = list(Ttest = size1, WilcoxTest = size2)
  return(result)
}

# Two Sample Power Function -----
Power = function(num_x, num_y, N, alt, qsetup){
  m = length(alt)
  powerTWelch = rep(0, m); powerFP = rep(0,m)
  x = rep(0, num_x); y = rep(0, num_y)
  library(NSM3)
  for (j in 1:m){
    for (i in 1:N){
      if (qsetup == 1){
        x = rnorm(num_x, mean=5)
        y = rnorm(num_y, mean=alt[j] + 5, sd=3)
      }
      else {
        x = rnorm(num_x, mean=5)
        y = rt(num_y, df=3) + 5 + alt[j]
      }
      pt = t.test(x, y)$p.value
      pFP = pFligPoli(x,y, method = "Asymptotic")$two.sided
      if (pt <= 0.05){
        powerTWelch[j] = powerTWelch[j] + 1
      }
      if (pFP <= 0.05){
        powerFP[j] = powerFP[j] + 1
      }
    }
  }
  if (qsetup == 1){
    title = "Plot when Y is Normal"
  }
  else {
    title = "Plot when Y is t3"
  }
  powerTWelch = powerTWelch / N; powerFP = powerFP / N
  plot(alt, powerTWelch, type="l", xlab="Alternative", ylab="power", ylim = c(0,1),main=title)
  lines(alt, powerFP, col="red", lty=2)
  legend("top", legend=c("Welch t test", "Fligner-Policello test"), lty=c(1,2), col=c("black", "red"), lwd=c(2,2))
}