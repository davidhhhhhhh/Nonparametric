# Two sample independence problem
# raw data
hunter_L_value = c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
panel_score_Y = c(2.6, 3.1, 2.5, 5.0, 3.6, 4.0, 5.2, 2.8, 3.8)

# Kendall's tau
cor.test(hunter_L_value, panel_score_Y, method = "kendall", alternative = "greater")

# Pearson's rho (remember check normality)
cor.test(hunter_L_value, panel_score_Y, method = "pearson", alternative = "greater")

