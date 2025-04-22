# Permutation Test
# Example 10---------
all_round_2 = c(25, 33, 35, 38, 48, 55, 56)
laid_off = c(55, 55, 64)
t.test(all_round_2, laid_off, alternative = "less")

permutation = function (x1,x2, N){
  current_diff = median(x1) - median(x2)
  all = c(x1,x2)
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

# Example 11-------
tele_df = read.csv("repairtimes.csv")
boxplot(Time~Group, data = tele_df)
ilec_df = tele_df[tele_df$Group=="ILEC",]
CLEC_df = tele_df[tele_df$Group=="CLEC",]
summary(ilec_df$Time)
summary(CLEC_df$Time)
# some ordinary tests 
t.test(ilec_df$Time, CLEC_df$Time, alternative = "less") # t normality
wilcox.test(ilec_df$Time, CLEC_df$Time, alternative = "less") # wilcox, same shape

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
  pvalue = sum(mean_diff <= current_diff) / N
  return(pvalue)
}
set.seed(5)
permutation(ilec_df$Time,CLEC_df$Time,10000)

# Example 12----------
moon = read.csv("Moon.csv")
permutation_paired <- function(x1, x2, N) {
  # Ensure that the two samples are paired
  if (length(x1) != length(x2)) {
    stop("x1 and x2 must be of the same length (paired samples required).")
  }
  
  # Compute the differences for each pair
  d <- x1 - x2
  # Observed statistic: the mean of the paired differences
  current_diff <- mean(d)
  
  # Initialize vector to hold permutation results
  permuted_diffs <- numeric(N)
  
  # Permutation: randomly flip the sign of each paired difference
  for (i in 1:N) {
    # For each pair, flip the sign with probability 0.5
    signs <- sample(c(-1,1), size = length(d), replace = TRUE)
    permuted_d <- d * signs
    permuted_diffs[i] <- mean(permuted_d)
  }
  
  # Plot the permutation distribution
  hist(permuted_diffs, main = "Distribution of Permuted Mean Differences",
       xlab = "Mean Difference", col = "lightgray", border = "white")
  abline(v = current_diff, col = "blue", lwd = 2)
  
  # Compute two-tailed p-value:
  # p-value is the proportion of permuted mean differences at least as extreme as the observed one
  pvalue <- mean(abs(permuted_diffs) >= abs(current_diff))
  
  return(pvalue)
}
set.seed(5)
permutation_paired(moon$Moon, moon$Other, 10000) # use permutation under correct setting in H0
