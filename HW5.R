# HW 5
# Q4
pooledTWilcoxonSize = function (num_x, num_y, N, alt_dist){
  sizeT = 0; sizeWilcoxon = 0
  library(BSDA)
  for (i in 1:N){
    if (alt_dist == "normal"){
      x = rnorm(num_x)
      y = rnorm(num_y, sd=3)
      pt = t.test(x, y, var.equal = TRUE, alternative = "greater")$p.value
      pWil = wilcox.test(x, y, alternative = "greater")$p.value
    }
    else if (alt_dist == "t"){
      x = rnorm(num_x)
      y = rt(num_y, df=3)
      pt = t.test(x, y, var.equal = TRUE, alternative = "greater")$p.value
      pWil = wilcox.test(x, y, alternative = "greater")$p.value
    }
    else {
      x = rnorm(num_x)
      y = runif(num_y, min=-1, max=1)
      pt = t.test(x, y, var.equal = TRUE, alternative = "greater")$p.value
      pWil = wilcox.test(x, y, alternative = "greater")$p.value
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

num_x = 30
num_y = 25
set.seed(1)
pooledTWilcoxonSize(num_x, num_y, 10000, alt_dist = "normal")
set.seed(1)
pooledTWilcoxonSize(num_x, num_y, 10000, alt_dist = "t")
set.seed(1)
pooledTWilcoxonSize(num_x, num_y, 10000, alt_dist = "uniform")

# Q5
alpha = read.csv("Alpha.csv")
dim(alpha)
names(alpha)

# pooled t test
t.test(alpha$Solitary,alpha$Nonconfined, var.equal = TRUE, alternative = "less")

# welch t test
t.test(alpha$Solitary,alpha$Nonconfined, alternative = "less")

# wilcoxon test
wilcox.test(alpha$Solitary, alpha$Nonconfined, alternative = "less")

# Fligner-Policello test
library(NSM3)
pFligPoli(alpha$Solitary, alpha$Nonconfined)
