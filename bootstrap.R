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
