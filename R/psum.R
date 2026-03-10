# Function for summing across rows instead of down columns
#' @export
psum <- function(...) {
  Reduce(`+`, list(...))
}

