# exercise 1a
exercise_one_a <- function() {
  num_terms <- 10#readline(prompt="Number of terms: ")
  collection <- c()
  odd_sum <- 0
  n <- 1
  while (length(collection) < num_terms) {
    # add to the collection 
    if (n %% 2 == 1) {
      collection = c(collection, n)
      odd_sum = odd_sum + n
    }
    # keep track of sum 
    n = n+1
  }
  return(c(collection, odd_sum))
}

# exercise 1b 
exercise_one_b <- function() {
  num_terms <- 5#readline(prompt="Number of terms: ")
  collection <- c()
  num_sum <- 0 
  n <- ''
  while (length(collection) < num_terms) {
    # create number
    next_num <- as.numeric(paste(n, 9, sep=''))
    # update n to keep track of next number 
    n = next_num
    collection = c(collection, next_num)
  }
  return(sum(collection))
}

exercise_three <- function() {
  # first pyramid 
  pyramid_base <- 50
  hashes <- NULL
  spaces <- NULL
  for (i in 1:pyramid_base) {
    hashes = strrep('#', i)
    spaces = strrep(' ', pyramid_base-i)
    print(paste(spaces, hashes))
  }
  
  # second pyramid 
  pyramid_base <- 50
  hashes <- NULL
  spaces <- NULL
  for (i in 1:pyramid_base) {
    hashes = strrep('#', i)
    spaces = strrep(' ', pyramid_base-i)
    print(paste(spaces, hashes, hashes, spaces))
  }
}

# exercise 4 

# get the largest product and location of such sequence
compute_m <- function(m4, seq_len) {
  # get product of each row
  row_prod <- 0
  row <- NULL
  for (i in 1:seq_len) {
    counter <- 1
    for (j in m4[i, ]) {
      counter = counter * j
    }
    if (counter > row_prod) {
      row_prod = counter
      row = i
    }
  }
  
  # get product for each column
  col_prod <- 0
  col <- NULL
  for (i in 1:seq_len) {
    counter <- 1
    for (j in m4[, i]) {
      counter = counter * j
    }
    if (counter > row_prod) {
      col_prod = counter
      col = i
    }
  }
  
  # get product of the diagonals
  diag_a = m4[1, 1] * m4[2, 2] * m4[3, 3] * m4[4, 4]
  diag_b = m4[1, 4] * m4[2, 3] * m4[3, 2] * m4[4, 1]
  
  # sum collection 
  sum_collection = c(row_prod, col_prod, diag_a, diag_b)
  
  # find maximum from the 4 functionality
  if (max(sum_collection) == row_prod) {
    return(c(row_prod, m4[row,]))
  }
  else if (max(sum_collection) == col_prod) {
    return(c(col_prod, m4[,col]))
  }
  else if (max(sum_collection) == diag_a) {
    return(c(diag_a, c(m4[1, 1],m4[2, 2],m4[3, 3],m4[4, 4])))
  }
  else if (max(sum_collection) == diag_a) {
    return(c(diag_b, c(m4[1, 4],m4[2, 3],m4[3, 2],m4[4, 1])))
  }
}

# create starter matrix with parameters
dims <- 20
seq_len <- 4
m <- matrix(rnorm(dims^2), nrow=dims)

# get all possible n by n matrices if n < dims
m_collection <- list()
current_max <- -100000000
longest_seq <- NULL
for (i in 1:(dims-seq_len)) {
  for (j in 1:(dims-seq_len)) {
    sub_m <- m[i:(i+3), j:(j+3)]
    result = compute_m(sub_m, seq_len)
    if (result[1] > current_max) {
      current_max = result[1]
      longest_seq = result[2:5]
    }
  }
}
current_max
longest_seq

