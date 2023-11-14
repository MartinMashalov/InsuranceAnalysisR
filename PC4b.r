# question 1
#install.packages('spuRs')
library(pracma)
library(spuRs)
x1 <- seq(from=-2*pi, to=2*pi, l=1000)
x2 <- seq(from=-2*pi, to=2*pi, l=1000)
fx = 2*cos(x)-sin(x)
gx1 = acos(0.5*sin(x))
gx2 = asin(2*cos(x))
#plot(x, gx1, xlim=c(-2*pi, 2*pi), ylim=c(-2.5, 2.5), type='l')
#lines(x, gx2, xlim=c(-2*pi, 2*pi), ylim=c(-2.5, 2.5), type='l')

# 
dg1x <- function(x) {
  return(-(1/2 * cos(x)/sqrt(1-(1/2 * sin(x))^2)))
}

dg2x <- function(x) {
  return((2*sin(x)*cot(2*cos(x)))/(sin(2*cos(x))))
}

plot(x1, dg1x(x1), type='l', ylim=c(-10, 10), xlim=c(-2*pi, 2*pi))
lines(x2, dg2x(x2), type='l', ylim=c(-10, 10), xlim=c(-2*pi, 2*pi))
lines(x2, rep(1, 1000))
lines(x2, rep(-1, 1000))

# question 1b
x <- seq(from=-6, to=6, l=1000)

g1 <- function(x) {
  return(acos(0.5*sin(x)))
}

fx <- function(x) {
  return(x)
}

plot(x, g1(x), type='l', xlim=c(1, 1.2), ylim=c(1, 1.2))
lines(x, fx(x), type='l', xlim=c(1, 1.2), ylim=c(1, 1.2))

# define the f function 
fx <- function(x) {
  return(2*cos(x)-sin(x))
}

fixedpoint1 <- function(ftn, x0, tol = 1e-9, max.iter = 100) {
  # applies the fixed point algorithm to find x such that ftn(x) == x
  
  # do first iteration
  xold <- x0
  xnew <- ftn(xold)
  iter <- 1
  
  # continue iterating until stopping conditions are met
  while ((abs(xnew-xold) > tol) && (iter < max.iter)) {
    xold <- xnew;
    xnew <- ftn(xold);
    iter <- iter + 1
  }
  
  # output depends on success of algorithm
  if (abs(xnew-xold) > tol) {
    return(c(NULL, NULL))
  } else {
    return(c(xnew, iter))
  }
}

output <- ""
data <- c(NULL, NULL)
for (i in -5:5) {
  data = fixedpoint1(g1, i)
  print(data[1])
  if (is.null(data[1])) {
    next
  }
  else {
    if (i >= 0) {
      output = paste(output, 'x0= ', i, ': Root=', round(data[1], 5), ' found in ', data[2], ' iterations', '\n', sep='')
    }
    else {
      output = paste(output, 'x0=', i, ': Root=', round(data[1], 5), ' found in ', data[2], ' iterations', '\n', sep='')
    }
  }
} 
cat(output)


# question 3
fx <- function(x) {
  return(2*cos(x) - sin(x))
}
bisection(fx, 1, 2)

# question 4
secant <- function(ftn, x0, x1, tol = 1e-9, max.iter = 100) {
  # Newton_Raphson algorithm for solving ftn(x)[1] == 0
  # we assume that ftn is a function of a single variable that returns
  # the function value and the first derivative as a vector of length 2
  #
  # x0 is the initial guess at the root
  # the algorithm terminates when the function value is within distance
  # tol of 0, or the number of iterations exceeds max.iter
  
  # initialise
  x <- x1
  prevx <- x0
  fx <- ftn(x)
  iter <-  0
  
  # continue iterating until stopping conditions are met
  while ((abs(fx[1]) > tol) && (iter < max.iter)) {
    x <- x - (ftn(x)*(x-prevx))/(ftn(x) - ftn(prevx))
    fx <- ftn(x)
    iter <-  iter + 1
    cat("At iteration", iter, "value of x is:", x, "\n")
  }
  
  # output depends on success of algorithm
  if (abs(fx[1]) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  } else {
    cat("Algorithm converged\n")
    return(x)
  }
}

fx_test <- function(x) {
  return(log(x)-exp(-x))
}
secant(fx_test, 1, 2)

# question 5
polyroot(c(1, 1, 1, 1))
polyroot(c(-4, -2, 1, 0, 0, 1))
# polyfit - lowest exponent coefficients to highest - also remember to substract any constants from the left hand side 


  
