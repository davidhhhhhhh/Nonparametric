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
# power for Anaasri test is low when two medians are not eqaul 