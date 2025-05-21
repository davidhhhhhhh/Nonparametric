# Ex. 3
my_vector <- c(
  0.72253333, 0.56142011, 0.76740719, 0.07510663, 0.33490185, 0.58515244,
  0.47098080, 0.05419383, 0.33661432, 0.13366613, 0.75226711, 0.89413058,
  0.35632000, 0.64904489, 0.71786053, 0.54468155, 0.62595019, 0.52480576,
  0.19889240, 0.65813650, 0.73231647, 0.08531394, 0.40557222, 0.04893669,
  0.72839276, 0.71895743, 0.84746688, 0.53319205, 0.31743430, 0.69311536
)

chisq.test(c(7,13,10), p=c(1/3,1/3,1/3))
chisq.test(c(6,6,14,4), p=c(1/4,1/4,1/4,1/4))

# Ex 4
theta1 = min(my_vector)
theta2 = max(my_vector)
range = theta2-theta1

# notice the degree of freedom for estimation are different: 4-2-1
chisq.test(c(6,6,14,4), p=c((0.25-theta1)/range,0.25/range,0.25/range,(theta2-0.75)/range))
1-pchisq(4.3165, df=1)
sum(my_vector < 0.25)
sum(my_vector < 0.5)
sum(my_vector < 0.75)
length(my_vector)

# Ex 5
x = matrix(c(25,18,3,12,6,13), nrow = 3,
           byrow = TRUE)
chisq.test(x, correct = FALSE) # no continuity correction 
chisq.test(t(x), correct = FALSE) # transpose does not change result 

# Ex 6
y = matrix(c(20,62,18,28,56,16), nrow = 2, byrow = TRUE)
y = 5 * y
chisq.test(y, correct = FALSE)
