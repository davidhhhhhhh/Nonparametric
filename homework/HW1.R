# HW 1 on Jan 24
# Q1
a = read.csv("Gorillas.csv")
dim(a)
groupSize = a$NoGorillas
library(BSDA)
SIGN.test(groupSize, md = 9)

# Q2
b = read.csv("Toad.csv")
dim(b)
SIGN.test(b$Length, md = 0.12, alternative = "less")
# check num of chirps greater than 0.12
x = sum(b$Length > 0.12)
# standardize x
x_s = (x - 0.5* 15)/ sqrt(0.25 * 15)
# probability of x_s in standardized normal
p_large_sample = pnorm(x_s)
# prob x in small sample
p_small_sample = pbinom(2, 15, 0.5)

# Q3
c = read.csv("exponential.csv")
names(c)
SIGN.test(c$Sample, md = log(2))
