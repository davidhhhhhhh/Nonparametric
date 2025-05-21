# HW 9
# Q1
df <- data.frame(
  A = c(22, 26, NA),
  B = c(28, 24, 29),
  C = c(29, 32, 28),
  D = c(23, 24, NA)
)

# Load tidyr library for reshaping
library(tidyr)

# Serialize the dataframe into two columns
df_long <- pivot_longer(df, 
                        cols = everything(),
                        names_to = "Variable",
                        values_to = "Value",
                        values_drop_na = TRUE)

out = aov(Value~Variable, data = df_long)
summary(out)

# Q2
q2 = read.csv("PriceEarning.csv")
q2_long = pivot_longer(q2,
                       cols = everything(),
                       names_to = "Types",
                       values_to = "Ratio")
out2 = aov(Ratio~Types, data = q2_long)
summary(out2)

# Q4
# Create the data frame
df <- data.frame(
  A = c(16, 17, 16, 17),
  B = c(4, 12, 2, 26),
  C = c(26, 22, 23, 24),
  D = c(8, 9, 11, 8)
)

# Generate a boxplot for each column
boxplot(df, 
        main = "Boxplot of Variables A, B, C, and D", 
        xlab = "Variables", 
        ylab = "Values", 
        col = "lightblue")