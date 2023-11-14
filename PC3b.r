# exercise 1 
geometric <- function(r, n) {
  arr <- rep(r, n+1)
  exp_arr = seq(from=0, to=n)
  print(sum(arr^exp_arr))
}
geometric(1.03, 10)


# exercise 2 
euler <- function(n) {
  arr <- seq(1:n)
  sum_arr = sum((1/(arr^2)))
  print(paste('pi^2/6 is ', 1.6449))
  cat(sprintf('The sum of the first 50 terms is %.4f', sum_arr))
}
euler(50)

# exercise 3
set.seed(321)
x <- matrix(sample(1:30, 3*5, replace=T), nrow=3, ncol=5)
maxmat <- function(mat) {
  # get the maximum of each row
  row_counter <- list()
  for (i in 1:nrow(mat)) {
    row_counter = append(row_counter, max(mat[i, ]))
  }
  
  # get the maximum of each column
  col_counter <- list()
  for (i in 1:ncol(mat)) {
    col_counter = append(col_counter, max(mat[, i]))
  }
  
  # maximum for entire matrix
  mat_counter <- -100000
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      if (mat[i, j] > mat_counter) {
        mat_counter = mat[i, j]
      }
    }
  }
  results = list(maxcol=col_counter, maxrow=row_counter, maxtot=mat_counter) ###make list into dictionary with list(name=name, othername=othername...)
  return(results)
}
results <- maxmat(x)
print(results$maxcol)
print(results$maxrow)
print(results$maxtot)

# exercise 4 
areatri <- function(x1, x2, x3, y1, y2, y3) {
  # function for distance
  dist <- function(x1, y1, x2, y2) {
    return(sqrt((x1-x2)^2 + (y1-y2)^2))
  }
  
  # sides of the triangle
  a = dist(x1, y1, x2, y2)
  b = dist(x1, y1, x3, y3)
  c = dist(x2, y2, x3, y3)
  
  # compute s 
  s <- (a+b+c)/2
  
  return(sqrt(s*(s-a)*(s-b)*(s-c)))
}
areatri(0, 4, 4, 0, 0, 3)

# exercise 5
#remove.packages("ggplot2") # Unisntall ggplot
#install.packages("ggplot2") # Install it again
library(ggplot2)
approx <- function(x, M) {
  collection <- c()
  for (j in x) {
    sum <- 0
    for (i in 1:M) {
      sum = sum + ((j^(2*i-1))/(2*i-1))
    }
    collection = c(collection, 2*sum)
  }
  return(collection)
}

actual_func <- function(x1, x2, by) {
  arr <- seq(x1, x2, by=by)
  func_container = c()
  for (i in arr) {
    func_container = c(func_container, log((i+1)/(1-i)))
  }
  return(func_container )
}

# part b 
func_container <- actual_func(-0.99, 0.99, 0.03)
plot(approx(seq(-0.99, 0.99, by=0.03), 5), col='blue')
lines(func_container, type='l', col='red')

# part c 
func_container <- actual_func(-0.99, -0.87, 0.03)
taylor = approx(seq(-0.99, -0.87, 0.03), 5)
x_coords <- seq(-0.99, -0.87, 0.03) 
df <- data.frame(x=x_coords, "log"=func_container,  Taylor=taylor) 
df

# part d
start_str <- 'log((x+1)/(1-x)) ~  2('
M = 4
for (i in seq(from=0, to=M, by=1)) {
  added_str <- paste('x^', 2*i+1, '/', 2*i+1, sep='')
  start_str = paste(start_str, added_str, sep='')
  if (i != M) {
    start_str = paste(start_str, '+', sep='')
  }
  else {
    start_str = paste(start_str, ')', sep='')
  }
}
start_str


