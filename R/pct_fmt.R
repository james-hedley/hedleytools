# Function to format percentages (but optionally add required decimal places if they are really small)
#' @export
pct_fmt <- function(x, decimal = TRUE, dp = 0, symbol = "", brackets = FALSE) {
  # Ensure numeric
  if (!is.numeric(x)) stop("x must be numeric")
  
  # If x is a decimal, then multiply by 100
  if (decimal) x <- x * 100
  
  # Thresholds for truncating values
  lower_threshold <- 10^-dp
  upper_threshold <- 100 - 10^-dp

  # Truncate very small or large values
  pct <- case_when(
    is.na(x) ~ NA,
    x == 0 ~ formatC(0, format = "f", digits = dp),
    x == 100 ~ formatC(100, format = "f", digits = dp),
    x < lower_threshold ~ paste0("<", formatC(lower_threshold, format = "f", digits = dp)),
    x > upper_threshold ~ paste0(">", formatC(upper_threshold, format = "f", digits = dp)),
    TRUE ~ as.character(formatC(janitor::round_half_up(x, digits = dp), format = "f", digits = dp)))
  
  # Add symbol
  pct <- paste0(pct, symbol) 
  
  # Add brackets
  if (brackets) {pct <- paste0("(", pct, ")")}
  
  # Return result
  return(pct)
}
