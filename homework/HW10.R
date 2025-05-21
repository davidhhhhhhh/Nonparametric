# HW 10
library(dplyr)
library(tidyr)
# Q5
df_5 = read.csv("Smoke.csv")
df_long = df_5 %>% pivot_longer(cols = everything(),
                                names_to = "Intensity",
                                values_to = "Value")
kruskal.test(Value~Intensity, data = df_long)

# Q6
df_6 = read.csv("LightBulb.csv")

df_long_6 = df_6 %>% pivot_longer(cols = everything(),
                                  names_to = "PlantType",
                                  values_to = "Value")
kruskal.test(Value~PlantType, data = df_long_6)
out_6 = aov(Value~PlantType, data = df_long_6)
anova(out_6)

df_long_6$Value[df_long_6$PlantType == "Plant3" & df_long_6$Value == 2246] = 1500
kruskal.test(Value~PlantType, data = df_long_6)
out_6 = aov(Value~PlantType, data = df_long_6)
anova(out_6)

#------------------------------
# Size Simulation Function
#------------------------------
Size = function(n1, n2, N, scenario) {
  size_ttest = 0
  size_aov = 0
  
  for (i in 1:N) {
    if (scenario == "a") {
      # Scenario (a): equal variances
      x = rnorm(n1, mean = 0, sd = 1)
      y = rnorm(n2, mean = 0, sd = 1)
    } else if (scenario == "b") {
      # Scenario (b): unequal variances, note: sd = sqrt(32)
      x = rnorm(n1, mean = 0, sd = 1)
      y = rnorm(n2, mean = 0, sd = sqrt(32))
    }
    
    # Pooled t-test assuming equal variances
    ttest_res <- t.test(x, y, var.equal = TRUE)
    p_t <- ttest_res$p.value
    
    # One-way ANOVA using two groups (equivalent to the t-test for k = 2)
    group <- factor(c(rep("X", n1), rep("Y", n2)))
    value <- c(x, y)
    df <- data.frame(Value = value, Group = group)
    p_aov <- oneway.test(Value~Group, var.equal = TRUE,data = df)$p.value
    
    if (p_t <= 0.05) { size_ttest = size_ttest + 1 }
    if (p_aov <= 0.05) { size_aov = size_aov + 1 }
  }
  
  # Compute the proportion of rejections (size)
  size_ttest <- size_ttest / N
  size_aov <- size_aov / N
  
  result <- list(size_ttest = size_ttest, size_aov = size_aov)
  return(result)
}

#------------------------------
# Power Simulation Function
#------------------------------
Power = function(n1, n2, N, alt, scenario) {
  m <- length(alt)
  power_ttest <- rep(0, m)
  power_aov   <- rep(0, m)
  
  for (j in 1:m) {
    for (i in 1:N) {
      if (scenario == "a") {
        # Scenario (a): equal variances; Y ~ N(mu,1)
        x = rnorm(n1, mean = 0, sd = 1)
        y = rnorm(n2, mean = alt[j], sd = 1)
      } else if (scenario == "b") {
        # Scenario (b): unequal variances; Y ~ N(mu, sqrt(32))
        x = rnorm(n1, mean = 0, sd = 1)
        y = rnorm(n2, mean = alt[j], sd = sqrt(32))
      }
      
      # Pooled t-test (equal variance assumed)
      ttest_res <- t.test(x, y, var.equal = TRUE)
      p_t <- ttest_res$p.value
      
      # One-way ANOVA for two groups
      group <- factor(c(rep("X", n1), rep("Y", n2)))
      value <- c(x, y)
      df <- data.frame(Value = value, Group = group)
      p_aov <- oneway.test(Value~Group, var.equal = TRUE,data = df)$p.value
      
      if (p_t <= 0.05) { power_ttest[j] <- power_ttest[j] + 1 }
      if (p_aov <= 0.05) { power_aov[j] <- power_aov[j] + 1 }
    }
  }
  
  # Divide by number of simulations to get power estimates
  power_ttest <- power_ttest / N
  power_aov <- power_aov / N
  
  # Plot the power curves
  if (scenario == "a") {
    title_text <- "Power Simulation (Scenario a: Y ~ N(mu,1))"
  } else if (scenario == "b") {
    title_text <- "Power Simulation (Scenario b: Y ~ N(mu, sqrt(32)))"
  }
  plot(alt, power_ttest, type = "l", xlab = "Shift (mu)", ylab = "Power",
       ylim = c(0, 1), main = title_text)
  lines(alt, power_aov, col= "red", lty = 2)
  legend("top", legend = c("Pooled t-test", "ANOVA"),
         lty = c(1, 2), col=c("black", "red"), lwd = c(2, 2))
  
  result <- list(power_ttest = power_ttest, power_aov = power_aov)
  return(result)
}

#------------------------------
# Execute Simulations
#------------------------------

# Define simulation parameters
n1 <- 10      # Sample size for group X
n2 <- 10      # Sample size for group Y
N <- 10000    # Number of simulation iterations
alt_values <- seq(0, 0.5, by = 0.01)  # Alternative shift values

# -- Scenario (a): X ~ N(0,1), Y ~ N(mu,1) --
set.seed(1)
cat("Scenario (a) - Size Simulation (mu = 0):\n")
print(Size(n1, n2, N, scenario = "a"))

set.seed(1)
cat("Scenario (a) - Power Simulation:\n")
power_a <- Power(n1, n2, N, alt = alt_values, scenario = "a")
print(power_a)

# -- Scenario (b): X ~ N(0,1), Y ~ N(mu,sqrt(32)) --
set.seed(2)
cat("Scenario (b) - Size Simulation (mu = 0):\n")
print(Size(n1, n2, N, scenario = "b"))

set.seed(2)
cat("Scenario (b) - Power Simulation:\n")
power_b <- Power(n1, n2, N, alt = alt_values, scenario = "b")
print(power_b)