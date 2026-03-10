# Function to create a new variable with the original numeric Stata coding (after converting labelled variables to factors)
#' @export
lbl_values <- function(data, ..., suffix = "_val", after_var = TRUE) {
  # must have dictionary
  if (is.null(attr(data, "value_labels"))) {
    stop("No data dictionary found in attr(data, \"value_labels\")")
  }
  
  dict <- attr(data, "value_labels")
  
  vars <- ensyms(...)  # capture variable names as symbols
  
  for (var in vars) {
    var_name <- as_string(var)
    
    # Validate
    if (!var_name %in% names(data)) {
      warning("Variable ", var_name, " not found in data — skipped.")
      next
    }
    if (!is.factor(data[[var_name]])) {
      warning("Variable ", var_name, " must be a factor — skipped.")
      next
    }
    
    var_str <- as.character(data[[var_name]])
    
    dict_var <- subset(dict, variable == var_name)
    if (nrow(dict_var) == 0) {
      warning("No value labels found for variable ", var_name, " — inserting NA.")
      data[[paste0(var_name, suffix)]] <- NA_real_
      next
    }
    
    lookup <- setNames(dict_var$value, dict_var$label)
    new_col <- paste0(var_name, suffix)
    data[[new_col]] <- lookup[var_str]
    
    if (after_var) {
      data <- data %>% relocate(all_of(new_col), .after = all_of(var_name))
    }
  }
  
  data
}
