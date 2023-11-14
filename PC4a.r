# question 1
x = seq(from=-pi/2, to=pi/2, length=100)

f <- function(x) {
  return(x/(1+x^2))
}

g <- function(x) {
  return(sin(x)/cos(x))
}


fg <- f(g(x))
gf <- g(f(x))

plot(x, fg, col='blue', lty='dashed', type='l')
lines(x, gf, col='red', lty='dashed', type='l')
# add a legend to the chart
legend('bottomright', title='Line Types', legend=c('f(g(x))', 'g(f(x))'))

# question 2
x <- seq(from=0, to=3, by=1)
N <- seq(from=1, to=3, by=1)

container <- c()

for (n in N) {
  for (X in x) {
    # internal sum for a specific x and a specific N value 
    sum_result <- 0
    for (i in 0:n) {
      sum_result = sum_result + ((-1)^i) * (X^(2*i))/(fact(2*i))
    }
    container = c(container, sum_result)
  }
}
z <-1
# loop through and return results
cat(sprintf('%f %f %f %f \n', cos(x)[z],cos(x)[z+1], cos(x)[z+2], cos(x)[z+3]))
for (i in 1:3) {
  cat(sprintf('%f %f %f %f \n', container[z],container[z+1], container[z+2], container[z+3]))
  z<-z+4
  #cat(sprintf('%f %f %f %f \n', container[4*i+1], container[4*i+2], container, container[4*i+3]))
}


