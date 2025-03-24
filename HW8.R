# HW 8
# 1
a = read.csv("MeanWeight.csv")
cor.test(a$Cysticerci,a$WormsRecovered,alternative = "greater", method="pearson")
cor.test(a$Cysticerci,a$WormsRecovered,alternative = "greater", method="kendall")
cor.test(a$Cysticerci,a$WormsRecovered,alternative = "greater", method="spearman")

# 2
independenceSize = function (N){
  sizePearson = 0; sizeKendall = 0; sizeSpearman = 0
  library(MASS)
  for (i in 1:N){
      mu = c(0, 0)
      sigma = matrix(c(1, 0, 0, 1), nrow = 2)
      samples = mvrnorm(n = 30, mu = mu, Sigma = sigma)
      y = samples[, 2]
      x = samples[, 1]
      pPearson = cor.test(x,y,method = "pearson")$p.value
      pKendall = cor.test(x,y,method = "kendall")$p.value
      pSpearman = cor.test(x,y,method = "spearman")$p.value
      if (pPearson <= 0.05){
        sizePearson = sizePearson + 1
      }
      if (pKendall <= 0.05){
        sizeKendall = sizeKendall + 1
      }
      if (pSpearman <= 0.05){
        sizeSpearman = sizeSpearman + 1
      }
  }
  sizePearson = sizePearson/N; sizeKendall = sizeKendall/N 
  sizeSpearman = sizeSpearman/N
  result = list(Pearson = sizePearson, Kendall = sizeKendall, Spearman = sizeSpearman)
  return(result)
}
set.seed(7)
independenceSize(10000)

# 3
independencePower = function (N, rho){
  powerPearson = 0; powerKendall = 0; powerSpearman = 0
  library(MASS)
  for (i in 1:N){
    mu = c(0, 0)
    sigma = matrix(c(1, rho, rho, 1), nrow = 2)
    samples = mvrnorm(n = 30, mu = mu, Sigma = sigma)
    y = samples[, 2]
    x = samples[, 1]
    pPearson = cor.test(x,y,method = "pearson")$p.value
    pKendall = cor.test(x,y,method = "kendall")$p.value
    pSpearman = cor.test(x,y,method = "spearman")$p.value
    if (pPearson <= 0.05){
      powerPearson = powerPearson + 1
    }
    if (pKendall <= 0.05){
      powerKendall = powerKendall + 1
    }
    if (pSpearman <= 0.05){
      powerSpearman = powerSpearman + 1
    }
  }
  powerPearson = powerPearson/N; powerKendall = powerKendall/N 
  powerSpearman = powerSpearman/N
  result = list(Pearson = powerPearson, Kendall = powerKendall, Spearman = powerSpearman)
  return(result)
}
set.seed(7)
independencePower(10000,0.3)
set.seed(7)
independencePower(10000,0.6)