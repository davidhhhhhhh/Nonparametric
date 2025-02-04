# HW2
#Q5
# power plot for sign test for uniform with var from 10, 24
SignPowerUniform = function(n, N, alt){
  m = length(alt)
  powerSign = rep(0,m)
  library(BSDA)
  for (j in 1:m){
    for (i in 1:N){
      y = runif(n, min=0, max=alt[j])
      num_great_6 = sum(y>6)
      psign = pbinom(num_great_6, n, 3/4)
      if (psign <= 0.1){
        powerSign[j] = powerSign[j] + 1
      }
    }
  }
  powerSign = powerSign / N
  plot(alt, powerSign, type="l", xlab="Alternative", ylab="power", main="Alternative v.s. Power for Uniform Distribution")
}

set.seed(1)
SignPowerUniform(36, 10000, seq(10,24,0.5))
