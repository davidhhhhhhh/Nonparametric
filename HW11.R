# HW 11
# 1
df_1 = read.csv("realestateHW.csv")

# 1a
t.test(df_1$Year2001, df_1$Year2002)

# 1b
permutation = function (x1,x2, N){
  current_diff = abs(mean(x1) - mean(x2))
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
  abline(v = -current_diff, col="blue")
  pvalue = sum(abs(mean_diff) >= current_diff) / N # alt for not equal be abs 
  return(pvalue)
}
set.seed(10)
permutation(df_1$Year2001, df_1$Year2002,10000)

# 1c
permutation_median = function (x1,x2, N){
  current_diff = abs(median(x1) - median(x2))
  all = c(x1,x2)
  diff_vec = rep(0,N)
  for (i in 1:N){
    sample_indices = sample(seq_along(all), size = length(x1))
    sample_value = all[sample_indices]
    rest_value = all[-sample_indices]
    diff = median(sample_value) - median(rest_value)
    diff_vec[i] = diff
  }
  hist(diff_vec, main = "distribution of sample mean difference")
  abline(v = current_diff, col = "blue")
  pvalue = sum(abs(diff_vec) >= current_diff) / N
  return(pvalue)
}
set.seed(10)
permutation_median(df_1$Year2001, df_1$Year2002,10000)

# 2
df_2 = read.csv("callcenter80.csv")

# 2a
hist(df_2$length, main = "Call Length Histogram", xlab = "call length")

# 2b
bootstrap_one_sample = function (x1, N){
  result = rep(0,N); n = length(x1)
  for (i in 1:N){
    result[i] = mean(sample(x1, n, replace = TRUE))
  }
  m = mean(result)
  s = sd(result)
  hist(result, main = "Histogram for bootstap distribution (n=20)")
  return(list(mean = m, sd = s))
}
set.seed(1)
bootstrap_one_sample(df_2$length, 1000)

# 3
df_3 = read.csv("callcenter20.csv")

# 3a
set.seed(1)
bootstrap_one_sample(df_3$CallLength, 1000)

# 3b