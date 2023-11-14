#install.packages("ggplot2")
library(ggplot2)

# exercise 1 
factorial <- function(n) {
  counter = 1
  for (i in 1:n) {
    counter = counter * i
  }
  return(counter)
}

stirling <- function(n) {
  return(sqrt(2*pi*n)*((n/exp(1))^n))
}

# get the first 100 values
for (i in 1:10) {
  print(i)
  print(factorial(i)/stirling(i))
} 

# exercise 3
times2 <- function(x) {
  k <- length(x)
  x2 <- rep(0, k)
  carry <- 0
  i <- k
  for (n in 1:999) {
    if (i >= 1) {
      temp <- 2*x[i]+carry
      carry <- 0
      if (temp > 9) {
        carry <- 1 
        x2[i] <- temp - 10
      }
      else {
        x2[i] <- temp
      }
      i <- i - 1
    }
    else {
      if (carry > 0) {
        x2 <- c(1, x2)
        return (x2)
      }
      else {
        return(x2)
      }
    }
  }
} 

# find the sum of the times2 function and sum of digits produced
arr = c(1)
for (n in 1:999) {
  arr = times2(arr)
}
counter <- 0
for (i in arr) {
  counter = i + counter
}

# define b 
#b <- readline(prompt = "Enter any number : ")
b <- 3

# create the approximation function 
approx2 <- function(x, b) {
  fx <- exp(2/i)
  fx_2 <- exp(2/b) - 2*((x-b)/(b^2))*exp(2/b) + 2*(1+b)*(((x-b)^2)/(b^4))*exp(2/b)
  return(c(fx, fx_2))
}

fx_storage = c()
second_fx_storage = c()
arr <- seq(2, 4, 0.1)
for (x in arr) {
  results <- approx2(x, b) 
  fx_storage = append(fx_storage, results[1]) # in R, the index starts from 1!!!! 
  second_fx_storage = append(second_fx_storage, results[2])
}

# plotting the result
plot(arr, fx_storage, type='l', col='red')
lines(arr, second_fx_storage, col='green')

