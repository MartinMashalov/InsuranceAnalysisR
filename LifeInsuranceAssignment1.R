
# Problem 2 
rm(list=ls())

l <- c(10000, 20000, 30000, rep(0, 20))
p <- c(0, 0, 0, 0.95^(0:19))
print(p)
i <- c(rep(0.06, 5), rep(0.07, 5), rep(0.08, 13))

yd <- 1 / (1 + i)
d_fac <- c(1, cumprod(yd))
pv_r <- l * d_fac[1:23]
pv_p <- p * d_fac[1:23]

m <- sum(pv_r) / sum(pv_p)
print(m * p)

#3.1
n_payments = 180
i = 0.055

interest_m = (1 + i) ^ (1/12) - 1
discount_factors = (1 + interest_m) ^ - (1:n_payments)

payment = rep(1, n_payments)
monthly_payment = 150000 / sum(payment * discount_factors)
monthly_payment

#3.2
months_2 = 12
monthly_i = interest_m
B_2 = n_payments*(1+monthly_i)^months_2-(m/monthly_i)*(((1+monthly_i)^months_2)-1)
print(B_2)


