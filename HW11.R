# HW 11
# 1
df_1 = read.csv("realestateHW.csv")

# 1a
t.test(df_1$Year2001, df_1$Year2002)

# 1b
permutation = function (x1,x2, N){
  current_diff = mean(x1) - mean(x2)
  all = c(x,x2)
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
  pvalue = sum(mean_diff <= current_diff) / N
  return(pvalue)
}
set.seed(10)
permutation(df_1$Year2001, df_1$Year2002,10000)

