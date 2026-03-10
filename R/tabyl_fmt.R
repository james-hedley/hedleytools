# Function to create a neatly formatted 2-way table
tabyl_fmt <- function(data, ..., denominator = "row", dp = 0, droplevels = TRUE) {
  
  # Check if one-way or two-way table
  n_vars <- length(rlang::ensyms(...))
  if(!(n_vars %in% c(1, 2))) stop("Only one-way or two-way tables are supported")
  
  # Drop unused levels
  if(droplevels == TRUE) {
    data <- data %>% droplevels()
  }
  
  # Create one-way table
  if(n_vars == 1) {
    tbl <- tabyl(data, ...) %>%
      adorn_totals(where = c("row"))
    
    tbl$percent <- sapply(tbl$percent, function(x) {
      pct_fmt(x, dp = dp, brackets = TRUE)
    })
  }
  
  # Create two-way table
  if(n_vars == 2) {
    tbl <- tabyl(data, ...) %>%
      adorn_totals(where = c("row", "col")) %>%
      adorn_percentages(denominator = denominator)
    
    pct_cols <- sapply(tbl, is.numeric)
    tbl[pct_cols] <- lapply(tbl[pct_cols], function(x) {
      pct_fmt(x, dp = dp)
    })

    tbl <- adorn_ns(tbl, position = "front")
    
    tbl <- adorn_title(tbl)
    
  }
  
  # Return table
  return(tbl)
}





