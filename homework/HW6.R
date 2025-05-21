# HW 6
# Q1
Power = function (n, m, N, qsetup){
  powerAB = 0; powerF = 0
  library(BSDA)
  for (i in 1:N){
    if (qsetup==1){
      x=rnorm(n)
      y=rnorm(m, sd = 1/sqrt(2))
      pf = var.test(x,y)$p.value
      pAb = ansari.test(x,y)$p.value
    }
    else if (qsetup==2){
      x=rnorm(n)
      y=rnorm(m, sd=1/sqrt(3))
      pf = var.test(x,y)$p.value
      pAb = ansari.test(x,y)$p.value
    }
    else if (qsetup==3){
      x=rnorm(n)
      y=rnorm(m, sd=1/sqrt(4))
      pf = var.test(x,y)$p.value
      pAb = ansari.test(x,y)$p.value
    }
    else if (qsetup==4){
      x=rnorm(n)
      y=rnorm(m, sd=1/sqrt(5))
      pf = var.test(x,y)$p.value
      pAb = ansari.test(x,y)$p.value
    }
    else {
      x=rnorm(n)
      y=rnorm(m, mean=2, sd=1/sqrt(2))
      pf = var.test(x,y)$p.value
      pAb = ansari.test(x,y)$p.value
    }
    if (pf <= 0.05){
      powerF = powerF + 1
    }
    if (pAb <= 0.05){
      powerAB = powerAB + 1
    }
  }
  powerF = powerF / N; powerAB = powerAB / N
  result = list(Ftest = powerF, ABtest = powerAB)
  return(result)
}

# a
set.seed(2)
Power(20,20,10000,1)
set.seed(2)
Power(20,20,10000,2)
set.seed(2)
Power(20,20,10000,3)
set.seed(2)
Power(20,20,10000,4)
set.seed(1)
Power(20,20,10000,5)

# Q2
Power = function (n, m, N, qsetup){
  powerAB = 0; powerF = 0; powerJF = 0
  library(BSDA);library(NSM3)
  for (i in 1:N){
    if (qsetup==1){
      x=rnorm(n)
      y=rnorm(m, sd = 1/sqrt(2))
      pf = var.test(x,y)$p.value
      pAb = ansari.test(x,y)$p.value
      Q = MillerJack(x,y)
      pJF = 2 * (1 - pnorm(abs(Q)))
    }
    else {
      x=rnorm(n)
      y=rnorm(m, mean=2, sd=1/sqrt(2))
      pf = var.test(x,y)$p.value
      pAb = ansari.test(x,y)$p.value
      Q = MillerJack(x,y)
      pJF = 2 * (1 - pnorm(abs(Q)))
    }
    if (pf <= 0.05){
      powerF = powerF + 1
    }
    if (pAb <= 0.05){
      powerAB = powerAB + 1
    }
    if (pJF <= 0.05){
      powerJF = powerJF + 1
    }
  }
  powerF = powerF / N; powerAB = powerAB / N; powerJF = powerJF / N
  result = list(Ftest = powerF, ABtest = powerAB, MJtest = powerJF)
  return(result)
}

# a
set.seed(5)
Power(20,20,10000,1)
set.seed(5)
Power(20,20,10000,2)