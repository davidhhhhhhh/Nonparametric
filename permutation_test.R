# Permutation Test
# Example 10
all_round_2 = c(25, 33, 35, 38, 48, 55, 56)
laid_off = c(55, 55, 64)
t.test(all_round_2, laid_off, alternative = "less")

permutation = function (x1,x2, N){
  current_diff = median(x1) - median(x2)
  all = c(x,x2)
  median_diff = rep(0,N)
  for (i in 1:N){
    sample_indices = sample(seq_along(all), size = 3)
    sample_value = all[sample_indices]
    rest_value = all[-sample_indices]
    diff = median(sample_value) - median(rest_value)
    median_diff[i] = diff
  }
  hist(median_diff, main = "distribution of sample median difference")
  abline(v = current_diff, col = "blue")
  pvalue = sum(median_diff >= current_diff) / N
  return(pvalue)
}
set.seed(10)
permutation(laid_off,all_round_2,100000)
