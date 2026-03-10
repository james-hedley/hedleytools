# Function to format regression results
## For linear-scale results (mean difference, % change etc.), report to 1dp, or 1 significant figure if <0.05
## For log-scale results (OR, HR, IRR etc.), report to 2dp (or 1 significant figure if <0.005)

coef_fmt <- function(x, scale = "linear", truncate_small_values = FALSE, dp = 1, exp = FALSE) {
  #---------------------------------------------------------------------
  # Options
  #---------------------------------------------------------------------
  # scale can be "linear" or "log"
  # truncate_small_values means e.g. if odds ratio = 0.002, show as <0.01 
  # dp is the number of decimal places to use (for linear scale only, log scale is always dp = 2) 
  # exp means values should first be exponentiated
  #---------------------------------------------------------------------
  
  # Ensure numeric
  if (!is.numeric(x)) stop("x must be numeric")
  
  # Exponentiate results if required
  if (exp) x <- exp(x)
  
  # Initialise output (keep NA as NA_character_)
  formatted <- rep(NA_character_, length(x))
  
  # Work only on non-missing values
  not_na <- !is.na(x)
  x_non_na <- x[not_na]
  
  if (length(x_non_na) == 0) return(formatted)
  
  # ---------------------------
  # Linear scale
  # ---------------------------
  if (scale == "linear") {
    abs_x <- abs(x_non_na)
    
    # ≥ 0.05 - round to dp decimal places
    idx1 <- abs_x >= (10^-dp)/2
    formatted[not_na][idx1] <- format(round(x_non_na[idx1], dp), scientific = FALSE, nsmall = dp, trim = TRUE)
    
    # < 0.05 - 1 significant figure
    idx2 <- abs_x < (10^-dp)/2
    formatted[not_na][idx2] <- format(signif(x_non_na[idx2], 1), scientific = FALSE, trim = TRUE)
    
    # Optionally truncate very small values
    if (truncate_small_values) {
      tiny <- abs_x < 10^(-dp)/2
      formatted[not_na][tiny] <- paste0("<", format(10^(-dp)/2, scientific = FALSE, nsmall = 2))
    }
  }
  
  # ---------------------------
  # Log scale (OR, HR, IRR etc.)
  # ---------------------------
  if (scale == "log") {
    log_dp <- 2
    abs_x <- abs(x_non_na)
    
    # ≥ 0.005 - round to 2 decimal places
    idx1 <- abs_x >= (10^-log_dp)/2
    formatted[not_na][idx1] <- format(round(x_non_na[idx1], log_dp), scientific = FALSE, nsmall = 2, trim = TRUE)
    
    # < 0.005 - 1 significant figure
    idx2 <- abs_x < (10^-log_dp)/2
    formatted[not_na][idx2] <- format(signif(x_non_na[idx2], 1), scientific = FALSE, trim = TRUE)
    
    # Optionally truncate very small values
    if (truncate_small_values) {
      tiny <- abs_x < 10^(-log_dp)/2
      formatted[not_na][tiny] <- paste0("<", format(10^(-log_dp)/2, scientific = FALSE, nsmall = 2))
    }
  }
  
  return(formatted)
}

# Wrapper where scale is always log
rr_fmt <- function(x, truncate_small_values = FALSE, exp = FALSE) {
  coef_fmt(x, scale = "log", truncate_small_values = truncate_small_values, exp = exp)
}


