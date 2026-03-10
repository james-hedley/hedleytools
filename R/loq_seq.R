# Function: Create a sequence of 'nice' numbers on a log 10 scale
# Created by: James Hedley
# Date created: 18th August 2025

# Example 'nice' numbers: 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, etc. 
# Given a minimum and a maximum, create a sequence of all 'nice' numbers in between
# Useful for creating breaks on a plot on a log scale

# Option 'threshold' is how far beyond the last break point xmin or xmax must be 
# before the next 'nice' number is included in the sequence
# e.g. if xmin is 0.7, on a log scale this is 48.5% of the way between 0.5 and 1,
# i.e. it is 51.5% to the left of 1. If the threshold is 50%, then this means the
# sequence should extend to 0.5. If the threshold was greater, say 60%, then we would 
# end the sequence at 1, and on a plot the point at 0.7 would be just a bit past the last break point.  
log_seq <- function(xmin, xmax, include_1 = TRUE, threshold = 0.5) {
  if (xmin <= 0 || xmax <= 0) stop("xmin and xmax must be positive")
  
  # Handle situation when xmin > xmax
  reverse <- FALSE
  if (xmin > xmax) {
    tmp <- xmin
    xmin <- xmax
    xmax <- tmp
    reverse <- TRUE
  }
  
  xmin_lower <- log_round(xmin, direction = "down")
  xmin_upper <- log_round(xmin, direction = "up")
  xmin_dist <- if_else(xmin_lower == xmin_upper, 0, 1 - (log10(xmin) - log10(xmin_lower)) / (log10(xmin_upper) - log10(xmin_lower)))
  start <- min(ifelse(xmin_dist > threshold, xmin_lower, xmin_upper),
               ifelse(include_1, 1, NA), 
               na.rm = TRUE)
          
  xmax_lower <- log_round(xmax, direction = "down")
  xmax_upper <- log_round(xmax, direction = "up")
  xmax_dist <- if_else(xmax_lower == xmax_upper, 0, (log10(xmax) - log10(xmax_lower)) / (log10(xmax_upper) - log10(xmax_lower)))
  end <- max(ifelse(xmax_dist > threshold, xmax_upper, xmax_lower),
             ifelse(include_1, 1, NA), 
             na.rm = TRUE)
          
  # Compute orders of magnitude
  min_exp <- floor(log10(start))
  max_exp <- ceiling(log10(end))
  
  # Generate all 1-2-5 multiples across the range
  exponents <- min_exp:max_exp
  candidates <- sort(as.numeric(sapply(exponents, function(e) c(1,2,5) * 10^e)))
  
  # Keep only candidates within the range
  candidates <- candidates[candidates >= start & candidates <= end]
  
  # Reverse order if xmin > xmax
  if(reverse) {candidates <- rev(candidates)}
  
  return(candidates)
}
