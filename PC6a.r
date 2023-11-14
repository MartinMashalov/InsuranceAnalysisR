# question 1
r = 1:4
r = rbind(r, c(0, 0, 0, 3))
r = rbind(r, c(0, 0, 0, 2))
r = rbind(r, c(0, 0, 0, 1))
matrix(r, nrow=4, ncol=4)

# question 2 
A <- matrix(1, 3, 3)
B <- 2*matrix(1, 3, 2)
C <- 3*matrix(1, 2, 3)

# question 3
A = matrix(c(1, 2, 3, 4), nrow=2, ncol=2, byrow=TRUE)
B = matrix(c(3, 4, -1, 2), nrow=2, ncol=2, byrow=TRUE)
A %*% B

# question 4
A <- matrix(c(1, 1, 2, 1, -1, -3, -2, -5, 1), nrow=3, ncol=3, byrow=TRUE)
b <- matrix(c(1, 0, 4), nrow=3, ncol=1, byrow=TRUE)
solve(A) %*% b

# question 5 
A <- matrix(c(3, 2, 3, -2), nrow=2, ncol=2, byrow=TRUE)
b <- matrix(c(7, 7), nrow=2, ncol=1, byrow=TRUE)
solve(A) %*% b

A <- matrix(c(1, 1, 1, 1, 1, -1, 1, 1, 0), nrow=3, ncol=3, byrow=TRUE)
b <- matrix(c(1, 0, 0), nrow=3, ncol=1, byrow=TRUE)
solve(A) %*% b

A <- matrix(c(1, 1, 1, 1, 1, 1, 
              1, -1, 1, 1, 1, 1, 
              1, 1, -1, 1, 1, 1, 
              1, 1, 1, -1, 1, 1, 
              1, 1, 1, 1, -1, 1, 
              1, 1, 1, 1, 1, -1), nrow=6, ncol=6, byrow=TRUE)
b <- matrix(c(1, 1, 1, 1, 1, 1), nrow=6, ncol=1, byrow=TRUE)
solve(A) %*% b

# question 6 
# part a 
library(Ryacas)
library(pracma)
library(spuRs)
yac("A:={{0, 1, s}, {s, 0, 1}, {1, s, 0}}")
yac("Determinant(A)")

# part b 
ys = c()
ss = c()
for (i in 1:4) {
  s <- (i-1)*(pi/4)
  A <- matrix(c(0, 1, s, s, 0, 1, 1, s, 0), nrow=3, ncol=3, byrow=TRUE)
  ys = c(ys, det(A))
  ss = c(ss, s)
}
polyfit(ss, ys, n=3)

def_func <- function(s) {1+s^3}
bisection(def_func, -2, 2, tol = 1e-09)

# question 7 
# part a
A <- matrix(c(1, 0, 0, -1, 
              0, 1, 0, 0, 
              0, 0, 1, 0, 
              -1, 0, 0, 1), nrow=4, ncol=4, byrow=TRUE)
eigendata <- eigen(A)
eigendata$values

# part b 
for (i in 1:4) {
  print(paste('A*v_', i, '  ', 'lambda_1*v_1: '))
  v1 = A %*% eigendata$vectors[, i]
  v2 = eigendata$values[[i]]*eigendata$vectors[, i]
  
  #print(paste(' ', value_a, ' ', value_b))
  cat(sprintf("A*v_%d   lambda*v_%d: \n", i, i))
  cat(sprintf('%10f  %10f \n', v1, v2))
}

# question 8 
# part a
P <- matrix(c(0.9, 0.5, 0.1, 0.5), nrow=2, ncol=2, byrow=TRUE)
eigendata <- eigen(P)
V <- eigendata$vectors
D <- matrix(c(eigendata$values[1], 0, 0, eigendata$values[2]), 2, 2, byrow=TRUE)
V %*% D %*% solve(V)

# part b 
V %*% D^10 %*% solve(V)

# part c





