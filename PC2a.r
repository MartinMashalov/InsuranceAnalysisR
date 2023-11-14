# exercise 1
create_iter_num <- function(idx)  {
  num_str <- ""
  if (idx == 1) {
    return(idx)
  }
  else {
    for (i in 1:idx) {
      num_str = paste(num_str, i, sep='')
    }
    return(as.numeric(num_str))
  }
}

for (x in 1:9) {
  left_num = create_iter_num(x)
  right_num = left_num * 8 + x
  cat(sprintf('%s x 8 + %d = %s\n', left_num, x, right_num))
}

# exercise 2  
n <- 0 
result <- 1000
while (abs(exp(-1) - result) > 0.0001) {
  n = n + 1
  result = (1-(1/n))^n
}
print(paste("Answer is: ", n))

# exercise 3
k <- 1000
found <- FALSE
a <- 1
maxa <- k/3

while (a < maxa && found == FALSE) {
  a <- a + 1
  b <- a 
  maxb <- k
  while (b < maxb && found== FALSE) {
    b <- b + 1
    c <- 1000-b-a
    if((a+b+c==k) && (a^2 + b^2 == c^2)) {
        found <- TRUE
        }
    }
}
print(a*b*c)

# exercise 4
for (m in 501:1000) {
  if (500000 %% m == 0) {
    a <- 1000 - (500000 / m)
    b <- 1000 - m 
    c <- 1000 - a - b
    break
  }
}
print(a*b*c)













