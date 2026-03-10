# Function to create a table 1 of baseline characteristics
create_table1 <- function(data, ..., overall = FALSE) {
  args <- list(...)
  tbl_list <- list()
  
  for (i in seq_along(args)) {
    mapping <- args[[i]]
    
    # Skip non-formula entries
    if (!inherits(mapping, "formula")) next
    
    # Detect one-sided vs two-sided formula
    if (length(mapping) == 2) {
      # One-sided: RHS only (e.g., ~ total_rowpct)
      var_sym <- NULL
      fun_expr <- mapping[[2]]
    } else if (length(mapping) == 3) {
      # Two-sided: var ~ fun
      var_expr <- mapping[[2]]
      var_sym <- rlang::ensym(var_expr)
      fun_expr <- mapping[[3]]
    } else {
      stop("Formula must be one-sided (~ fun) or two-sided (var ~ fun)")
    }
    
    # If bare function, make it a call with overall
    if (rlang::is_symbol(fun_expr)) {
      fun_expr <- rlang::call2(rlang::as_string(fun_expr), overall = overall)
    }
    
    # If it's already a call, append overall = overall
    if (rlang::is_call(fun_expr)) {
      fun_expr <- rlang::call_modify(fun_expr, overall = overall)
    }
    
    # Build arguments
    fun_args <- rlang::call_args(fun_expr)
    if (!is.null(var_sym)) {
      fun_args <- c(list(data = data, var = var_sym), fun_args)
    } else {
      fun_args <- c(list(data = data), fun_args)
    }
    
    # Evaluate
    tbl <- rlang::eval_tidy(
      rlang::call2(rlang::call_name(fun_expr), !!!fun_args)
    )
    
    # Ensure 'var' exists if applicable
    if (!"var" %in% names(tbl) && !is.null(var_sym)) {
      tbl <- dplyr::mutate(tbl, var = as_label(var_sym))
    }
    
    tbl_list[[length(tbl_list) + 1]] <- tbl
  }
  
  final_tbl <- dplyr::bind_rows(tbl_list)
  
  # Fill remaining NAs
  final_tbl <- final_tbl %>% 
    dplyr::mutate(dplyr::across(where(is.character), ~tidyr::replace_na(.x, "")))
  
  return(final_tbl)
}



