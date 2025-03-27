# K sample 
# Create vectors for each detergent
A <- c(77, 81, 71, 76, 80)
B <- c(72, 58, 74, 66, 70)
C <- c(76, 85, 82, 80, 77)

# Combine into a data frame
detergent_data <- data.frame(
  Detergent = rep(c("A", "B", "C"), each = 5),
  Value = c(A, B, C)
)

# ANOVA for K Sample
out = aov(Value~Detergent, data = detergent_data)
anova(out)

# no constant variance assumption between different groups
oneway.test(Value~Detergent, data = detergent_data)


