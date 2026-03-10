# Function: Round a number to the nearest 'nice' number on a log base 10 scale
# Created by: James Hedley
# Date created: 18th August 2025

# Example 'nice' numbers: 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, etc. 
log_round <- function(x, direction = c("nearest", "up", "down")) {
  direction <- match.arg(direction)
  
  if (any(x <= 0)) stop("All x must be positive")
  
  sapply(x, function(xi) {
    exp <- 10^floor(log10(xi))
    
    # Candidates: previous, current, next order of magnitude
    candidates <- c(1, 2, 5) * exp / 10       # previous order
    candidates <- c(candidates, c(1, 2, 5) * exp)      # current order
    candidates <- c(candidates, c(1, 2, 5) * exp * 10) # next order
    
    # Compute log10 distance
    log_dist <- log10(candidates) - log10(xi)
    
    chosen <- switch(direction,
                     nearest = candidates[which.min(abs(log_dist))],
                     up      = min(candidates[log_dist >= 0], na.rm = TRUE),
                     down    = max(candidates[log_dist <= 0], na.rm = TRUE))
    
    return(chosen)
  })
}
