# HW 13
# q1
chisq.test(c(30,56,73,41), p=c(0.0961,0.3411,0.4036,0.1592))
1-pchisq(11.609, df=2)

# q2
chisq.test(c(59,27,9,1,0), p=c(0.6065,0.3033,0.0758,0.0126,0.0018))
1-pchisq(0.78142, df=3)

# q3
a = read.table("geometric")
table(a)
chisq.test(c(15,8,9,4,2,4,1,1,2,1,1,1,1), p=c(dgeom(0,1/3.86),
                                              dgeom(1,1/3.86),
                                              dgeom(2,1/3.86),
                                              dgeom(3,1/3.86),
                                              dgeom(4,1/3.86),
                                              dgeom(5,1/3.86),
                                              dgeom(6,1/3.86),
                                              dgeom(7,1/3.86),
                                              dgeom(8,1/3.86),
                                              dgeom(9,1/3.86),
                                              dgeom(10,1/3.86),
                                              dgeom(11,1/3.86),
                                              1-pgeom(11,1/3.86)))
1-pchisq(5.033,df=11)

# q4
b = matrix(c(628,146,172,54),nrow=2, byrow = TRUE)
chisq.test(b, correct = FALSE)

# q5
c = matrix(c(27,47,140,93),nrow=2, byrow = TRUE)
chisq.test(c, correct = FALSE)

# q6
d = matrix(c(11,10,55,24),nrow=2, byrow = TRUE)
chisq.test(d, correct = FALSE)
