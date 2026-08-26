extract_ci <- function(x) {
  # Iterate over each string element in input vector 'x' and bind row results into a matrix
  res <- do.call(rbind, lapply(x, function(val) {
    
    # 1. Fallback for NA values, NULLs, or whitespace-only strings
    if (is.na(val) || !nzchar(trimws(val))) {
      return(c(lower = NA_real_, upper = NA_real_))
    }
    
    # 2. Extract text inside () or [] to prioritize CI bounds over leading point estimates
    inside <- regmatches(val, regexec("[(\\[](.*?)[)\\]]", val))[[1]]
    target <- if (length(inside) >= 2) inside[2] else val
    
    # 3. Match numbers with optional minus signs, thousands-commas, and decimal points
    num_pattern <- "(?:(?<=^|\\s|\\(|\\[|,)-)?\\d+(?:,\\d+)*(?:\\.\\d+)?"
    matches <- regmatches(target, gregexpr(num_pattern, target, perl = TRUE))[[1]]
    
    # 4. Remove thousands separators (commas) and convert matched strings to numeric
    nums <- as.numeric(gsub(",", "", matches))
    
    # 5. Require at least two extracted numbers (lower and upper bound)
    if (length(nums) < 2) {
      return(c(lower = NA_real_, upper = NA_real_))
    }
    
    # 6. Return first two numbers named 'lower' and 'upper'
    stats::setNames(nums[1:2], c("lower", "upper"))
  }))
  
  # Convert matrix result into a 2-column data frame (lower, upper)
  as.data.frame(res)
}