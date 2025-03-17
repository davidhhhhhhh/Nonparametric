# Function to simulate one instance of KS statistic for n=1 and m=3
simulate_KS_statistic <- function(n = 1, m = 3) {
  # Generate samples from Uniform(0,1)
  sample1 <- runif(n)
  sample2 <- runif(m)
  
  # Combine and sort the observations
  combined <- sort(c(sample1, sample2))
  
  # Compute the empirical CDF for each sample at the points in the combined sample
  ecdf1 <- sapply(combined, function(x) sum(sample1 <= x) / n)
  ecdf2 <- sapply(combined, function(x) sum(sample2 <= x) / m)
  
  # KS statistic is the maximum absolute difference between the two ECDFs
  max_diff <- max(abs(ecdf1 - ecdf2))
  return(max_diff)
}

# Set parameters for simulation
set.seed(123)  # for reproducibility
replications <- 10000
ks_stats <- numeric(replications)

# Run simulation
for (i in 1:replications) {
  ks_stats[i] <- simulate_KS_statistic(n = 1, m = 3)
}

# Show summary statistics of the simulated KS statistics
summary(ks_stats)

# Plot histogram of the simulated KS statistics
hist(ks_stats, breaks = 20, main = "Simulated Distribution of KS Statistic (n=1, m=3)",
     xlab = "KS Statistic", col = "lightblue", border = "gray")

# Optional: Print a table of frequencies for the observed KS statistic values
ks_table <- table(round(ks_stats, digits = 2))
print(ks_table)