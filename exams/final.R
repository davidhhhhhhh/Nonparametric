# Final Exam 
# q1
a = read.csv("Talk.csv")
male = a[a$Male==1,]
female = a[a$Male!=1,]

# 1a 
summary(male$WordsPerDay)
summary(female$WordsPerDay)
boxplot(male$WordsPerDay, female$WordsPerDay)
sqrt(var(female$WordsPerDay))
sqrt(var(male$WordsPerDay))

# 1b
t.test(male$WordsPerDay, female$WordsPerDay)

# 1c
wilcox.test(male$WordsPerDay, female$WordsPerDay)

# 1d
qqnorm(male$WordsPerDay)
qqline(male$WordsPerDay)
qqnorm(female$WordsPerDay)
qqline(female$WordsPerDay)

# 1e
permutation = function (x1,x2, N){
  current_diff = mean(x1) - mean(x2)
  all = c(x1,x2)
  mean_diff = rep(0,N)
  for (i in 1:N){
    sample_indices = sample(seq_along(all), size = length(x1))
    sample_value = all[sample_indices]
    rest_value = all[-sample_indices]
    diff = mean(sample_value) - mean(rest_value)
    mean_diff[i] = diff
  }
  hist(mean_diff, main = "distribution of sample mean difference")
  abline(v = current_diff, col = "blue")
  pvalue = sum(mean_diff >= current_diff) / N
  return(pvalue)
}
set.seed(47155)
permutation(female$WordsPerDay,male$WordsPerDay,10000)

# 1f
var.test(female$WordsPerDay,male$WordsPerDay)

# 1g
ansari.test(female$WordsPerDay - median(female$WordsPerDay), 
            male$WordsPerDay - median(male$WordsPerDay))

# 1h
library(NSM3)
set.seed(1142)
pLepage(female$WordsPerDay,male$WordsPerDay, method="Monte Carlo")

# 1i
ks.test(female$WordsPerDay,male$WordsPerDay)

# 2 
# 2a 
t.test(female$WordsPerDay,male$WordsPerDay, conf.level = 0.9)

# 2b
wilcox.test(female$WordsPerDay,male$WordsPerDay, conf.int = TRUE, conf.level = 0.9)

# 2c
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
set.seed(212)
bootstrap_two_sample(female$WordsPerDay,male$WordsPerDay,10000, 0.90)

# 5
x = c(68,72,79,69,84,80,78,64,60,68,73,72,70)

# 5a 
sort(x)
length(x)
rank = c (3.5,7.5,11,5,13,12,10,2,1,3.5,9,7.5,6)
qnorm(rank/14)

# 5b 
S_norm = -0.67448975 + 0.08964235 +0.79163861 -0.36610636 +1.46523379 +
  1.06757052 +0.56594882

# 5c
median_score = c(0,1,1,0,1,1,1,0,0,0,1,1,0)
a_bar_median = sum(median_score) / 13
s_median_var = sum((median_score-mean(median_score))^2) * 6 * 7 / (13 * 12)

norm_score = qnorm(rank/14)
a_bar_norm = mean(norm_score)
s_norm_mean = a_bar_norm * 7
s_norm_var = sum((norm_score-mean(norm_score))^2) * 6 * 7 / (13 * 12)

# 5d
p_median = (1- pnorm(5, mean=3.7692, sd=sqrt(0.8698))) * 2
p_norm = (1- pnorm(2.9394, mean=0.0042, sd=sqrt(2.3549))) * 2

# 6a
p_mood_median = function (x,y){
  n = length(x); m = length(y)
  combined_rank = rank(c(x,y)) # get combined rank 
  cut_off = (m + n + 1) / 2 # calculate cut off 
  m_score = as.numeric(combined_rank > cut_off) # get median score 
  ts = sum(m_score[1:n]) # calculate test stat from score in x sample
  E0 = n * mean(m_score)
  Var0 = m * n/((m+n) * (m+n-1)) * sum((m_score-mean(m_score))^2)
  p_value = 0
  # decide p value based on location of t.s. with respect to the mean 
  if (ts < E0) {
    p_value = 2 * pnorm(ts, mean = E0, sd = sqrt(Var0))
  }
  else {
    p_value = 2 * (1 - pnorm(ts, mean = E0, sd = sqrt(Var0)))
  }
  return(p_value)
}

p_norm_score = function (x,y){
  n = length(x); m = length(y)
  combined_rank = rank(c(x,y)) # get combined rank 
  n_score = qnorm(combined_rank/(m+n+1)) # get norm score
  ts = sum(n_score[1:n]) # calculate test stat from score in x sample
  E0 = n * mean(n_score)
  Var0 = m * n/((m+n) * (m+n-1)) * sum((n_score-mean(n_score))^2)
  if (ts < E0) {
    p_value = 2 * pnorm(ts, mean = E0, sd = sqrt(Var0))
  }
  else {
    p_value = 2 * (1 - pnorm(ts, mean = E0, sd = sqrt(Var0)))
  }
  return(p_value)
}

Size = function (n, m, N, dist){
  size1 = 0; size2 = 0; size3 = 0
  library(BSDA); mExp = qexp(0.5, rate=1)
  for (i in 1:N){
    if (dist == "normal"){
      y = rnorm(m)
      x = rnorm(n)
    }
    else if (dist == "t"){
      y = rt(m, df = 3)
      x = rt(n, df = 3)
    }
    else if (dist == "exp"){
      y = rexp(m, rate = 1)
      x = rexp(n, rate = 1)
    }
    else {
      y = rlogis(m, location = 0, scale = 1)
      x = rlogis(n, location = 0, scale = 1)
    }
    p1 = wilcox.test(x, y)$p.value
    p2 = p_mood_median(x,y)
    p3 = p_norm_score(x,y)
    if (p1 <= 0.05){
      size1 = size1 + 1
    }
    if (p2 <= 0.05){
      size2 = size2 + 1
    }
    if (p3 <= 0.05){
      size3 = size3 + 1
    }
  }
  size1 = size1 / N; size2 = size2 / N; size3 = size3 / N
  result = list(Wilcoxon = size1, Mood_Median = size2, Norm_Score = size3)
  return(result)
}

# define parameters 
set.seed(1156)
Size(50, 45, 10^4, "normal")
set.seed(1156)
Size(50, 45, 10^4, "t")
set.seed(1156)
Size(50, 45, 10^4, "exp")
set.seed(1156)
Size(50, 45, 10^4, "logistci")

# 6b
Power = function(num_x, num_y, N, alt, qsetup){
  m = length(alt)
  powerWil = rep(0, m); powerMedian = rep(0,m); powerNorm = rep(0,m)
  x = rep(0, num_x); y = rep(0, num_y)
  library(NSM3)
  for (j in 1:m){
    for (i in 1:N){
      if (qsetup == 1){
        x = rnorm(num_x)
        y = rnorm(num_y, mean=alt[j])
      }
      else if (qsetup == 2) {
        x = rt(num_x, df=3)
        y = rt(num_y, df=3) + alt[j]
      }
      else if (qsetup == 3) {
        x = rexp(num_x, rate =1)
        y = rexp(num_y, rate = 1) + alt[j]
      }
      else {
        x = rlogis(num_x, location = 0, scale = 1)
        y = rlogis(num_y, location = alt[j], scale = 1)
      }
      p1 = wilcox.test(x, y)$p.value
      p2 = p_mood_median(x,y)
      p3 = p_norm_score(x,y)
      if (p1 <= 0.05){
        powerWil[j] = powerWil[j] + 1
      }
      if (p2 <= 0.05){
        powerMedian[j] = powerMedian[j] + 1
      }
      if (p3 <= 0.05){
        powerNorm[j] = powerNorm[j] + 1
      }
    }
  }
  if (qsetup == 1){
    title = "Plot when Y is Normal"
  }
  else if (qsetup==2) {
    title = "Plot when Y is t3"
  }
  else if (qsetup==3) {
    title = "Plot when Y is exp"
  }
  else {
    title = "Plot when Y is logistic"
  }
  powerWil = powerWil / N; powerMedian = powerMedian / N; powerNorm = powerNorm / N
  plot(alt, powerWil, type="l", xlab="Alternative", ylab="power", ylim = c(0,1),main=title)
  lines(alt, powerMedian, col="red", lty=2)
  lines(alt, powerNorm, col="green", lty=3)
  legend("top", legend=c("Wilcoxon test", "Mood's Median", "Normal Score"), lty=c(1,2,3), col=c("black", "red","green"), lwd=c(2,2,2))
}
delta = seq(-1,1,0.1)
set.seed(1094)
Power(50, 45, 10^4, delta, 1)
set.seed(1094)
Power(50, 45, 10^4, delta, 2)
set.seed(1094)
Power(50, 45, 10^4, delta, 3)
set.seed(1094)
Power(50, 45, 10^4, delta, 4)