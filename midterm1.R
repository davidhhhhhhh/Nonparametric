# Midterm 1
# Q1
a = read.csv( "Anorexia.csv")
diff_cb = a$after - a$before
# 1a
t.test(diff_cb, alternative = "greater")
# 1b
library(BSDA)
SIGN.test(diff_cb, alternative = "greater")
# 1c
wilcox.test(diff_cb, alternative = "greater")
# 1e
t.test(diff_cb,conf.level = 0.9)
# 1f
SIGN.test(diff_cb, conf.level = 0.9)
# 1g
wilcox.test(diff_cb, conf.int = TRUE, conf.level = 0.9)

# Q2
b = read.csv("Anorexia2.csv")
df_cb = b[b$therapy=="cb",]
df_c = b[b$therapy=="c",]
# a
cb_gain = df_cb$after - df_cb$before
c_gain = df_c$after - df_c$before
mean(cb_gain)
sqrt(var(cb_gain))
mean(c_gain)
sqrt(var(c_gain))
# b
t.test(cb_gain, c_gain, var.equal = TRUE, alternative = "greater")
# c
wilcox.test(cb_gain, c_gain, alternative = "greater")
# e
t.test(c_gain, cb_gain, var.equal = TRUE, conf.level = 0.9)
# f
wilcox.test(c_gain, cb_gain, conf.int = TRUE, conf.level = 0.9)

# Q4d
pnorm(9.05, mean=9,sd=sqrt(9/30))

# Q5
Size = function (num_x, num_y, N, qsetup){
  sizePoolT = 0; sizeWelT = 0; sizeWilcox = 0; sizeFP = 0
  library(NSM3)
  for (i in 1:N){
    if (qsetup == 1){
      x=rnorm(num_x, mean=5)
      y=rnorm(num_y, mean=5, sd=3)
      ptPool = t.test(x,y,var.equal = TRUE)$p.value
      ptWel = t.test(x,y)$p.value
      pWil = wilcox.test(x,y)$p.value
      pFP = pFligPoli(x,y, method = "Asymptotic")$two.sided
    }
    else if (qsetup == 2){
      x=rnorm(num_x, mean=5)
      y=rnorm(num_y, mean=5, sd=0.33)
      ptPool = t.test(x,y,var.equal = TRUE)$p.value
      ptWel = t.test(x,y)$p.value
      pWil = wilcox.test(x,y)$p.value
      pFP = pFligPoli(x,y, method = "Asymptotic")$two.sided
    }
    else if (qsetup == 3){
      x=rnorm(num_x, mean=5)
      y=rt(num_y, df=3) + 5
      ptPool = t.test(x,y,var.equal = TRUE)$p.value
      ptWel = t.test(x,y)$p.value
      pWil = wilcox.test(x,y)$p.value
      pFP = pFligPoli(x,y, method = "Asymptotic")$two.sided
    }
    else {
      x=rnorm(num_x, mean=5)
      y=rchisq(num_y,df=5)
      ptPool = t.test(x,y,var.equal = TRUE)$p.value
      ptWel = t.test(x,y)$p.value
      pWil = wilcox.test(x,y)$p.value
      pFP = pFligPoli(x,y, method = "Asymptotic")$two.sided
    }
    if (ptPool <= 0.05){
      sizePoolT = sizePoolT + 1
    }
    if (ptWel <= 0.05){
      sizeWelT = sizeWelT + 1
    }
    if (pWil <= 0.05){
      sizeWilcox = sizeWilcox + 1
    }
    if (pFP <= 0.05){
      sizeFP = sizeFP + 1
    }
  }
  sizePoolT = sizePoolT / N; sizeWelT = sizeWelT / N
  sizeWilcox = sizeWilcox / N; sizeFP = sizeFP / N
  result = list(PooledTtest = sizePoolT, WelchTTest = sizeWelT,
                WilcoxonTest = sizeWilcox, FPTest = sizeFP)
  return(result)
}
# a
set.seed(100)
Size(40,45,10000,1)
# b
set.seed(200)
Size(40,45,10000,2)
# c
set.seed(300)
Size(40,45,10000,3)
# d
set.seed(400)
Size(40,45,10000,4)

# Q6
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
# a
delta = seq(-1,1,0.05)
set.seed(500)
Power(40,45,10000,delta,1)

# b
delta = seq(-1,1,0.05)
set.seed(600)
Power(40,45,10000,delta,2)