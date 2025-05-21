# HW 12
chisq.test(c(455,343,318,152,130,129), p=c(0.3,0.2,0.2,0.1,0.1,0.1))

# 5
chisq.test(c(9,11,8,22), p=c(0.125,0.25,0.3125,0.3125))

# 6
preg = read.csv("Pregnancy.csv")
chisq.test(c(18,44,8), p=c(pnorm(250,266,16),pnorm(280,266,16)-pnorm(250,266,16),1-pnorm(280,266,16)))
