# Midterm 2
# 1
men = c(58,76,82,74,79,65,74,86)
women = c(66,74,69,76,72,73,75,67,68)
# 1a
var_men = var(men)
var_women = var(women)

# 1b
var.test(men, women)

# 1c
ansari.test(men, women)

# 1d
ansari.test(men-median(men), women-median(women))

# 1e
library(NSM3)
MillerJack(men,women) 
2 * (1-pnorm(2.774))

# 1f
set.seed(12)
pLepage(men,women, method="Monte Carlo")

# 1g
library(NSM3)
pKolSmirn(men,women)

# 3
a_3 = read.csv("InsectSprays.csv")

# 3a
spray_name = unique(a_3$spray)
for (i in 1 : length(spray_name)) {
  temp = a_3[a_3$spray == spray_name[i],]
  print(mean(temp$count))
  print(var(temp$count))
}

# 3b
boxplot(count~spray, data = a_3, main="Boxplot for Different Sprays")

# 3c
out_3 = aov(count~spray, data = a_3)
anova(out_3)

# 3d
qqnorm(residuals(out_3))
qqline(residuals(out_3))

# 3e
kruskal.test(a_3$count,a_3$spray)

# 3f
a_3$sqrt_count = sqrt(a_3$count)
out_3f = aov(sqrt_count~spray, data = a_3)
anova(out_3f)
qqnorm(residuals(out_3f))
qqline(residuals(out_3f))

# 3g
kruskal.test(a_3$sqrt_count,a_3$spray)

# 4
gmat = c(710,610,640,580,545,560,610,530,560,540,570,560)
gpa = c(4,4,3.9,3.8,3.7,3.6,3.5,3.5,3.5,3.3,3.2,3.3)

# 4a
cor.test(gmat,gpa, method="pearson")
cor.test(gmat,gpa, method="kendall")
cor.test(gmat,gpa, method="spearman")

# 4b
cor.test(gmat,gpa, method="pearson", alternative = "greater")

# 4c
cor.test(gmat,gpa, method="kendall", alternative = "greater")

# 4d
cor.test(gmat,gpa, method="spearman", alternative = "greater")

# 4e
qqnorm(gmat)
qqline(gmat)
qqnorm(gpa)
qqline(gpa)
boxplot(gmat,gpa)

# 5a
Size = function (n, m, N, setting){
  sizeLP = 0; sizeKS = 0
  library(NSM3); mExp = qexp(0.5, rate=1)
  for (i in 1:N){
    if (setting == 1){
      x=rnorm(n)
      y=rnorm(m)
    }
    else if (setting == 2){
      x=rt(n, df = 3)
      y=rt(m, df = 3)
    }
    else if (setting == 3){
      x=rexp(n)
      y=rexp(m)
    }
    else {
      x=rchisq(n, df = 5)
      y=rchisq(m, df = 5)
    }
    pLP = pLepage(x,y,method = "Asymptotic")$p.val
    pKS = pKolSmirn(x,y,method = "Asymptotic")$p.val
    if (pLP <= 0.05){
      sizeLP = sizeLP + 1
    }
    if (pKS <= 0.05){
      sizeKS = sizeKS + 1
    }
  }
  sizeLP = sizeLP / N; sizeKS = sizeKS / N
  result = list(LP = sizeLP, KS = sizeKS)
  return(result)
}
set.seed(2282025)
Size(50,50,10000,1)
set.seed(2282025)
Size(50,50,10000,2)
set.seed(2282025)
Size(50,50,10000,3)
set.seed(2282025)
Size(50,50,10000,4)

# 5b
Power = function (n, m, N, setting){
  powerLP = 0; powerKS = 0
  library(NSM3)
  for (i in 1:N){
    if (setting == 1){
      x=rnorm(n)
      y=rnorm(m, mean = 0.5, sd = 1)
    }
    else if (setting == 2){
      x=rnorm(n)
      y=rnorm(m, mean = 0, sd = 2)
    }
    else {
      x=rnorm(n)
      y=rnorm(m, mean = 0.5, sd = 2)
    }
    pLP = pLepage(x,y,method = "Asymptotic")$p.val
    pKS = pKolSmirn(x,y,method = "Asymptotic")$p.val
    if (pLP <= 0.05){
      powerLP = powerLP + 1
    }
    if (pKS <= 0.05){
      powerKS = powerKS + 1
    }
  }
  powerLP = powerLP / N; powerKS = powerKS / N
  result = list(LP = powerLP, KS = powerKS)
  return(result)
}
set.seed(1125)
Power(50,50,10000,1)
set.seed(1125)
Power(50,50,10000,2)
set.seed(1125)
Power(50,50,10000,3)