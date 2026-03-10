# Function to format numbers
n_fmt <- function(x, dp = 0, sep = ",", round_direction = c("nearest", "up", "down")) {
  # Ensure numeric
  if (!is.numeric(x)) stop("x must be numeric")
  
  # Determine round direction
  if(all(round_direction == c("nearest", "up", "down"))) {
    rnd <- "nearest"
  } else {
    rnd <- round_direction
  }
  
  # Thresholds for truncating values
  lower_threshold <- 10^-dp

  # Truncate very small or large values
  n <- case_when(
    x == 0 ~ formatC(0, format = "f", digits = dp, big.mark = sep),
    x == 100 ~ formatC(100, format = "f", digits = dp, big.mark = sep),
    x < lower_threshold ~ paste0("<", formatC(lower_threshold, format = "f", digits = dp, big.mark = sep)),
    rnd == "nearest" ~ as.character(formatC(round_nearest(x, multiple = 10^-dp), format = "f", digits = dp, big.mark = sep)),
    rnd == "up" ~ as.character(formatC(round_up(x, multiple = 10^-dp), format = "f", digits = dp, big.mark = sep)),
    rnd == "down" ~ as.character(formatC(round_down(x, multiple = 10^-dp), format = "f", digits = dp, big.mark = sep)))
  
  # Return result
  return(n)
}
