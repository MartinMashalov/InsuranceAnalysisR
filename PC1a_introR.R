m <- 1000000
hash_map = rep(0, m)

sequence_func <- function (n) {
  total <- 0
  while (n != 1) {
    if (hash_map[n] != 0 & n != 1) {
      return(hash_map[n]+total)
    }
    if (n %% 2 == 0) {
      n = n/2
    }
    else if (n %% 2 != 0) {
      n = 3*n + 1
    } 
    total = total + 1
    hash_map[n] = total 
  }
  return(total)
}

maximum <- 0
idx <- 0
for (x in 1:m) {
  seq_len <- sequence_func(x) 
  if (seq_len > maximum) {
    maximum = seq_len 
    idx = x
  }
}
print(idx)
print(maximum)






