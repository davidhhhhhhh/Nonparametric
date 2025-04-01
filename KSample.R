# K sample 
# Detergent Data-------
A <- c(77, 81, 71, 76, 80)
B <- c(72, 58, 74, 66, 70)
C <- c(76, 85, 82, 80, 77)

# Combine into a data frame
detergent_data <- data.frame(
  Detergent = rep(c("A", "B", "C"), each = 5),
  Value = c(A, B, C)
)

# ANOVA for K Sample
out = aov(Value~Detergent, data = detergent_data)
anova(out)

# no constant variance assumption between different groups
oneway.test(Value~Detergent, data = detergent_data)

# Kruskal test 
kruskal.test(Value~Detergent, data = detergent_data)

# Data Ethnicity------
df <- data.frame(
  Black = c(1, 3, 6, 7, 9, 10, 11, 16,20,21),
  Whites = c(4,  5,12,13,15, 17,22,27,29,30),
  Hispanic1 = c(2, 3, 5, 8, 10, 12,14,18,23,25)
)
library(tidyr)
df_long = pivot_longer(df, 
                       cols = everything(),
                       names_to = "Ethnic",
                       values_to = "IQ"
                       )
out2 = aov(IQ~Ethnic, data = df_long)
summary(out2)

# Size Simulation----------
Size = function (n1, n2, n3, N, dist){
  sizeANOVA = 0; sizeKS = 0
  for (i in 1:N){
    if (dist == "normal"){
      x = rnorm(n1)
      x_lab = rep("x", n1)
      y = rnorm(n2)
      y_lab = rep("y", n2)
      z = rnorm(n3)
      z_lab = rep("z", n3)
      value = c(x,y,z)
      label = c(x_lab,y_lab,z_lab)
      df = data.frame(Value = value, 
                      Label = label)
      pAnova = oneway.test(Value~Label, var.equal = TRUE,data = df)$p.value
      pKS = kruskal.test(Value~Label, data = df)$p.value
    }
    else if (dist == "t"){
      x = rt(n1,df=3)
      x_lab = rep("x", n1)
      y = rt(n2,df=3)
      y_lab = rep("y", n2)
      z = rt(n3,df=3)
      z_lab = rep("z", n3)
      value = c(x,y,z)
      label = c(x_lab,y_lab,z_lab)
      df = data.frame(Value = value, 
                      Label = label)
      pAnova = oneway.test(Value~Label, var.equal = TRUE,data = df)$p.value
      pKS = kruskal.test(Value~Label, data = df)$p.value
    }
    else {
      x = rexp(n1)
      x_lab = rep("x", n1)
      y = rexp(n2)
      y_lab = rep("y", n2)
      z = rexp(n3)
      z_lab = rep("z", n3)
      value = c(x,y,z)
      label = c(x_lab,y_lab,z_lab)
      df = data.frame(Value = value, 
                      Label = label)
      pAnova = oneway.test(Value~Label, var.equal = TRUE,data = df)$p.value
      pKS = kruskal.test(Value~Label, data = df)$p.value
    }
    if (pAnova <= 0.05){
      sizeANOVA = sizeANOVA + 1
    }
    if (pKS <= 0.05){
      sizeKS = sizeKS + 1
    }
  }
  sizeANOVA = sizeANOVA / N; sizeKS = sizeKS / N
  result = list(sizeANOVA = sizeANOVA, sizeKS = sizeKS)
  return(result)
}
set.seed(1)
Size(20,20,20,10000,"normal")
set.seed(1)
Size(20,20,20,10000,"t")
set.seed(1)
Size(20,20,20,10000,"exp")
set.seed(2)
Size(10,20,30,10000,"normal")
set.seed(2)
Size(10,20,30,10000,"t")
set.seed(2)
Size(10,20,30,10000,"exp")

# Power Simulation--------
Power = function(num_x1, num_x2, num_x3, N, alt, qsetup){
  m = length(alt)
  powerAov = rep(0, m); powerKS = rep(0,m)
  x1 = rep(0, num_x1); x2 = rep(0, num_x2); x3 = rep(0, num_x3)
  for (j in 1:m){
    for (i in 1:N){
      if (qsetup == 1){
        x = rnorm(num_x1)
        y = rnorm(num_x2)
        z = rnorm(num_x3)+ alt[j]
      }
      else if (qsetup == 2){
        x = rt(num_x1,df=3)
        y = rt(num_x2,df=3)
        z = rt(num_x3,df=3)+ alt[j]
      }
      else {
        x = rexp(num_x1)
        y = rexp(num_x2)
        z = rexp(num_x3)+ alt[j]
      }
      x_lab = rep("x", num_x1)
      y_lab = rep("y", num_x2)
      z_lab = rep("z", num_x3)
      value = c(x,y,z)
      label = c(x_lab,y_lab,z_lab)
      pAnova = oneway.test(Value~Label, var.equal = TRUE,data = df)$p.value
      pKS = kruskal.test(Value~Label, data = df)$p.value
      if (pAnova <= 0.05){
        powerAov[j] = powerAov[j] + 1
      }
      if (pKS <= 0.05){
        powerKS[j] = powerKS[j] + 1
      }
    }
  }
  if (qsetup == 1){
    title = "Plot when Y is Normal"
  }
  else if (qsetup == 2){
    title = "Plot when Y is T3"
  }
  else {
    title = "Plot when Y is exp"
  }
  powerAov = powerAov / N; powerKS = powerKS / N
  plot(alt, powerAov, type="l", xlab="Alternative", ylab="power", ylim = c(0,1),main=title)
  lines(alt, powerKS, col="red", lty=2)
  legend("top", legend=c("AOV", "KS"), lty=c(1,2), col=c("black", "red"), lwd=c(2,2))
}
delta = seq(-1,1,0.1)
set.seed(3)
Power(20,20,20,100,delta,1)