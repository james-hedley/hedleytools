# Function to summarise count and percentage for a given binary variable
binary_n_pct <- function(data, var, dp = 0, sep = ",", n_name = "col1", pct_name = "col2", overall = FALSE) {
  
  group_vars <- group_vars(data)
  
  var_quosure <- enquo(var)
  var_label   <- as_label(var_quosure)  # get variable name as string
  
  # Run n_pct logic first
  tbl <- n_pct(data, !!var_quosure, dp = dp, sep = sep, n_name = n_name, pct_name = pct_name, overall = overall)
  
  # Copy p-value up to the first row
  if ("p_value" %in% names(tbl)) {
    tbl <- tbl %>% mutate(p_value = first(p_value))
  }
  
  # Only keep the last row
  final_tbl <- tbl %>%
    mutate(var = var_label) %>%        
    slice(n()) 
  
  return(final_tbl)
}
