# Bootstrap
# Example 11 ----------
tele_df = read.csv("repairtimes.csv")
ilec_df = tele_df[tele_df$Group=="ILEC",]
bootstrap_one_sample = function (x1, N){
  result = rep(0,N); n = length(x1)
  for (i in 1:N){
    result[i] = mean(sample(x1, n, replace = TRUE))
  }
  m = mean(result)
  s = sd(result)
  hist(result, main = "Histogram for bootstap distribution")
  return(list(mean = m, sd = s))
}
set.seed(1)
bootstrap_one_sample(ilec_df$Time, 1000)

# R code----------------
library(bootstrap)
theta = function (x) {mean(x)}
set.seed(17)
r = boott(ilec_df$Time, theta, nboott = 1000)
print(r$confpoints, digits = 4)
print(r, digits = 4)

# alternative code
library(boot)
theta = function(x,a){mean(x[a])}
set.seed(17)
boot_r = boot(ilec_df$Time, theta, R=1000)
boot.ci(boot_r, type = "perc", conf = 0.9)
