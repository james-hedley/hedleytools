# Function for summing across rows instead of down columns
psum <- function(...) {
  Reduce(`+`, list(...))
}

