# Function to create a blank row in a table
header_row <- function(data, n_name = "col1", pct_name = "col2", overall = FALSE) {

  group_vars <- group_vars(data)

  if(length(group_vars) == 0) {
    tbl <- tibble(var = "", !!n_name := "", !!pct_name := "")
  } else if(length(group_vars) == 1) {
    # Get observed group levels (preserve ordering if factor)
    grp <- group_vars[1]
    grp_vals <- data[[grp]]
    grp_levels <- if (is.factor(grp_vals)) levels(grp_vals) else sort(unique(as.character(grp_vals)))
    grp_levels <- str_to_lower(grp_levels)
    
    # Build column names like n_group1, pct_group1, ...
    n_cols   <- paste0(n_name, "_", grp_levels)
    pct_cols <- paste0(pct_name, "_", grp_levels)
    
    tbl <- tibble::tibble(
      var = ""
    )
    
    tbl[n_cols]   <- ""
    tbl[pct_cols] <- ""
  }
  
  return(tbl)
}
