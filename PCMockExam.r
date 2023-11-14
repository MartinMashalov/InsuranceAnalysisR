# question 1 
f <- function(x) {
  return(0.5*exp(-x/2))
}

NumInt <- function(f,a,b,n) {
  # set the previous r value
  R = list(list((b-a)*(f(a)+f(b))*0.5))

  # loop through each element in n 
  for (j in 2:n) {
    # set h value 
    h <- (b-a)*2^(1-j)
    
    # get the S value 
    S <- 0 
    for (i in 1:(2^(j-2))) {
      S = S + f(a + (2*i-1)*h)
    }

    # set the previous r value 
    sub_list <- list()
    sub_list = append(sub_list, 0.5*R[[j-1]][[1]] + h*S)
    for (k in 2:j) {
      val <- (4^(k-1)*sub_list[[k-1]] - R[[j-1]][[k-1]])/ (4^(k-1)-1)
      sub_list = append(sub_list, val)
    }
    R = append(R, list(sub_list))
  }
  return(R[[n]][[n]])
}

# get actual value of integral 
actual <- integral(f, 0, 4*log(2)+2*log(5))
NumInt(f, 0, 4*log(2)+2*log(5), 5)

# generate the plot 
errors = c()
max_n <- 6
for (n in 2:max_n) {
  approx_val <- NumInt(f, 0, 4*log(2)+2*log(5), n)
  errors = c(errors, abs(approx_val-actual))
}
plot(2:max_n, log2(errors), type='p', lty='solid', col='red', xlab='n', ylab='log2error')

# part 5 - use the bisection method to 
optim_f <- function(b) {
  return(NumInt(f, 0, b, 6) - 0.99)
}
a = 8 
b = 10
m = NULL
# code the bisection method 
while (abs(b-a) > 1E-4) {
  if (optim_f(b)*optim_f(a) < 0) {
    m = 0.5*(a+b)
    if (optim_f(m)*optim_f(b) < 0) {
      a = m
    }
    else {
      b = m
    }
  }
}
result = mean(c(a, b))
cat(sprintf("The result for b is %0.5f", result))










