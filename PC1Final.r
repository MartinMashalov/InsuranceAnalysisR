# final exam for programming in r 2022 

# exercise 1 
circle <- function(r) {
  # get the maximum range of iteration 
  top_range <- 2*r
  # counters for keeping track of layer of circle
  layer <- 1
  current_space <- r 
  sidespace_counter <- r-1
  
  # create the top part of the circle
  print(paste(strrep(' ', sidespace_counter), strrep('*', r), strrep(' ', sidespace_counter)))
  
  for (i in 1:top_range) {
    for (j in 1:top_range) {
      if (abs((sqrt((i-r)^2+(j-r)^2))-r) < 0.5) {
        # print the top of the circle
        if (layer < r) {
          sidespace_counter = sidespace_counter - 1
          print(paste(strrep(' ', sidespace_counter), '*', strrep(' ', current_space-2), '*', strrep(' ', sidespace_counter)))
          current_space = current_space + 2
        }
        
        # print middle of the circle
        else if (layer ==r) {
          for (i in 1:r) {
            print(paste('*', strrep(' ', current_space-2), '*'))
          }
        }
        
        else {
          current_space = current_space - 2
          if (current_space - r < 0) {
            break
          }
          print(paste(strrep(' ', sidespace_counter), '*', strrep(' ', current_space-2), '*', strrep(' ', sidespace_counter)))
          sidespace_counter = sidespace_counter + 1
        }
        
        # increment the space counter between each side of the circle and the layer 
        layer = layer + 1
      }
    }
  }
  
  # create the bottom part of the circle 
  print(paste(strrep(' ', r-1), strrep('*', r), strrep(' ', r-1)))
}
circle(5)

# exercise 2 
t <- seq(0, 2*pi, ((2*pi)/10000))
x <- cos(t) - cos(80*t)*sin(t)
y <- 2*sin(t) - sin(80*t)
plot(x, y, xlim=c(-3, 3), ylim=c(-3, 3), main='Nice Curve', type='l')

# exercise 3
reverse_num <- function(n) {
  reverse_num_str <- ''
  n = as.character(n)
  for (i in strsplit(n, '')[[1]]) {
    reverse_num_str = paste(i, reverse_num_str, sep='')
  }
  return(as.numeric(reverse_num_str))
}

result <- NULL
total <- 0
for (i in 1:10000) {
  result = i + reverse_num(i)
  
  # check if the number follows the condition for reversible
  odd_counter <- TRUE
  if (result %% 2 == 1) {
    # iterate through each digit
    for (digit in strsplit(as.character(result), '')[[1]]) {
      # check if the digit is even and set the counter to false if found 
      if (as.numeric(digit) %% 2 == 0) {
        odd_counter = FALSE
      }
    }
  }
  else {
    odd_counter = FALSE
  }
  
  # check the counter and increase total if stays true
  if (odd_counter == TRUE) {
    total = total + 1
  }
}

print(paste("There are", total, "reversible numbers below 10,000"))











