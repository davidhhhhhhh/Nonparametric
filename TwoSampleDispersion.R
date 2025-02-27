# Ansari-Bradley
Ramsay <- c(111, 107, 100, 99, 102, 106, 109, 108, 
            104, 99, 101, 96, 97, 102, 107, 113, 
            116, 113, 110, 98)

jung <- c(107, 108, 106, 98, 105, 103, 110, 105, 
          104, 100, 96, 108, 103, 104, 114, 114, 
          113, 108, 106, 99)
# method 1
library(NSM3)
pAnsBrad(jung, Ramsay)

# method 2, preferred
ansari.test(jung,Ramsay, alternative="greater")

# size and power simulation
Size = function (n, m, N, dist){
  sizeAB = 0; sizeF = 0
  library(BSDA); mExp = qexp(0.5, rate=1)
  for (i in 1:N){
    if (dist == "normal"){
      x=rnorm(n)
      y=rnorm(m)
      pf = var.test(x,y)$p.value
      pAb = ansari.test(x,y)$p.value
    }
    else if (dist == "t"){
      x = rt(n, df = 3)
      y = rt(m, df = 3)
      pf = var.test(x,y)$p.value
      pAb = ansari.test(x,y)$p.value
    }
    else {
      x = rexp(n, rate = 1)
      y = rexp(m, rate = 1)
      pf = var.test(x,y)$p.value
      pAb = ansari.test(x,y)$p.value
    }
    if (pf <= 0.05){
      sizeF = sizeF + 1
    }
    if (pAb <= 0.05){
      sizeAB = sizeAB + 1
    }
  }
  sizeF = sizeF / N; sizeAB = sizeAB / N
  result = list(Ftest = sizeF, ABtest = sizeAB)
  return(result)
}
set.seed(1)
Size(20,20,10000,"normal")
set.seed(2)
Size(20,20,10000,"t")
set.seed(3)
Size(20,20,10000,"exp")

# conclusion f test is sensitive for departure from normality 
# power for Anaasri test is low when two medians are not equal

# Miller Jackknife -----
library(NSM3)
MillerJack(Ramsay,jung) # return q satistics
pnorm(0.8263065) # since ha is ramsey < jung 
# size and power simulation
Size = function (n, m, N, setting){
  sizeMJN = 0; sizeMJt = 0
  library(NSM3); mExp = qexp(0.5, rate=1)
  for (i in 1:N){
    if (setting == 1){
      x=rnorm(n)
      y=rnorm(m)
      Q = MillerJack(x,y)
      pMJN = 2 * (1 - pnorm(abs(Q)))
      pMJT = 2 * (1-pt(abs(Q), df=n+m-2)) # notice pvalue be absolute for Q 
    }
    else if (setting == 2){
      x=rnorm(n)
      y=rnorm(m, mean=2, sd=1)
      Q = MillerJack(x,y)
      pMJN = 2 * (1 - pnorm(abs(Q)))
      pMJT = 2 * (1-pt(abs(Q), df=n+m-2))
    }
    else if (setting == 3){
      x=rt(n, df=5)
      y=rt(m, df=5)
      Q = MillerJack(x,y)
      pMJN = 2 * (1 - pnorm(abs(Q)))
      pMJT = 2 * (1-pt(abs(Q), df=n+m-2))
    }
    else {
      x=rt(n, df=5)
      y=rt(m, df=5) + 1
      Q = MillerJack(x,y)
      pMJN = 2 * (1 - pnorm(abs(Q)))
      pMJT = 2 * (1-pt(abs(Q), df=n+m-2))
    }
    if (pMJN <= 0.05){
      sizeMJN = sizeMJN + 1
    }
    if (pMJT <= 0.05){
      sizeMJt = sizeMJt + 1
    }
  }
  sizeMJN = sizeMJN / N; sizeMJt = sizeMJt / N
  result = list(MJN = sizeMJN, MJT = sizeMJt)
  return(result)
}
set.seed(1)
Size(20,20,10000,1)
set.seed(1)
Size(20,20,10000,2)
set.seed(3)
Size(20,20,10000,3)
set.seed(3)
Size(20,20,10000,4)
# conclusion: for jackknife, location not matter 
