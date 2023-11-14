library(spuRs)

p = c(0.5, 0.95, 0.975, 0.99)

phi_pdf <- function(x) {
  (1/(2*pi))*exp(-0.5*x^2)
}

simpson_n <- function(ftn, a, b, n = 100) {
  n <- max(c(2*(n %/% 2), 4))
  h <- (b-a)/n
  x.vec1 <- seq(a+h, b-h, by = 2*h)
  x.vec2 <- seq(a+2*h, b-2*h, by = 2*h)
  f.vec1 <- sapply(x.vec1, ftn)
  f.vec2 <- sapply(x.vec2, ftn)
  S <- h/3*(ftn(a) + ftn(b) + 4*sum(f.vec1) + 2*sum(f.vec2))
  return(S)
}

newtonraphson <- function(ftn, x0, tol = 1e-9, max.iter = 100) {
  x <- x0
  fx <- ftn(x)
  print(fx)
  iter <-  0
  
  # continue iterating until stopping conditions are met
  while ((abs(fx[1]) > tol) && (iter < max.iter)) {
    x <- x - fx[1]/fx[2]
    print(x)
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

bisection <- function(ftn, x.l, x.r, tol = 1e-9) {
  # applies the bisection algorithm to find x such that ftn(x) == x
  # we assume that ftn is a function of a single variable
  #
  # x.l and x.r must bracket the fixed point, that is
  # x.l < x.r and ftn(x.l) * ftn(x.r) < 0
  #
  # the algorithm iteratively refines x.l and x.r and terminates when
  # x.r - x.l <= tol
  
  # check inputs
  if (x.l >= x.r) {
    #cat("error: x.l >= x.r \n")
    return(NULL)
  }
  f.l <- ftn(x.l)
  f.r <- ftn(x.r)
  if (f.l == 0) {
    return(x.l)
  } else if (f.r == 0) {
    return(x.r)
  } else if (f.l * f.r > 0) {
    #cat("error: ftn(x.l) * ftn(x.r) > 0 \n")
    return(NULL)
  }
  
  # sucessively refine x.l and x.r
  n <- 0
  while ((x.r - x.l) > tol) {
    x.m <- (x.l + x.r)/2
    f.m <- ftn(x.m)
    if (f.m == 0) {
      return(x.m)
    } else if (f.l * f.m < 0) {
      x.r <- x.m
      f.r <- f.m
    } else {
      x.l <- x.m
      f.l <- f.m
    }
    n <- n + 1
    #cat("at iteration", n, "the root lies between", x.l, "and", x.r, "\n")
  }
  
  # return (approximate) root
  return((x.l + x.r)/2)
}

Phi <- function(z) {
  f <- function(x){1/sqrt(2*pi)*exp(-x^2/2)}
  return(c(simpson_n(f, -10000, z), phi_pdf(z)))
}

# newton-raphson for root finding 
for (i in p) {
  nf <- function(z){Phi(z)} - i
  print(paste('P value: ', i))
  print(paste('Zp value: ', bisection(nf, 0, 4, tol = 1e-10)))
  writeLines('\n')
}


# question 4
trapezoid <- function(f, a, b) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  
  h <- b - a
  
  fxdx <- (h / 2) * (f(a) + f(b))
  
  return(fxdx)
  
} # simpson function is up above 

nt_e <- function() {
  
}






