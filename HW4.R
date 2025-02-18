# HW 4
#Q1
a = read.csv("FEV1.csv")
names(a)
t.test(a$Ratio, mu = 0.8, alternative="less")
wilcox.test(a$Ratio, mu = 0.8, alternative = "less")
library(BSDA)
SIGN.test(a$Ratio, md = 0.8, alternative = "less")

# Q3 
temperature = read.csv("temperature.csv")
wilcox.test(temperature$Moderate, temperature$High)
