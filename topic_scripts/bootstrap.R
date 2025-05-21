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

# R code for one sample ----------------
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

# Two sample (independence)----------------
bootstrap_two_sample = function (x1, x2, N, conf){
  result = rep(0,N); n = length(x1); m = length(x2)
  for (i in 1:N){
    result[i] = mean(sample(x1, n, replace = TRUE)) - mean(sample(x2, m, replace = TRUE))
  }
  lower_ci = quantile(result, (1-conf) / 2)
  upper_ci = quantile(result, 1- (1-conf) / 2)
  hist(result, main = "Histogram for two sample bootstap distribution")
  return(list(mean = mean(result), lower_ci = lower_ci, upper_ci = upper_ci))
}
set.seed(9)
bootstrap_two_sample(ilec_df$Time, tele_df$Time[tele_df$GroupNum==2],10000, 0.95)

# Example 14
estate_df = read.csv("realestate.csv")
theta = function(x){mean(x, trim = 0.25)}
set.seed(8)
result = boott(estate_df$Price, theta, nboott = 1000)
print(result, digits = 4)

bootstrap_one_sample = function (x1, N){
  result = rep(0,N); n = length(x1)
  for (i in 1:N){
    result[i] = mean(sample(x1, n, replace = TRUE), trim = 0.25)
  }
  m = mean(result)
  s = sd(result)
  hist(result, main = "Histogram for bootstap distribution")
  return(list(mean = m, sd = s))
}
set.seed(37)
bootstrap_one_sample(estate_df$Price, 1000)

