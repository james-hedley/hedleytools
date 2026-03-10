# Function to summarise total and row percentage for grouped data
#' @export
total_rowpct <- function(data, 
                         dp = 0,
                         sep = ",",
                         n_name = "col1", 
                         pct_name = "col2",
                         overall = FALSE) {
  
  # Check that data is grouped
  group_vars <- dplyr::group_vars(data)
  if(length(group_vars) == 0) stop("total_rowpct() requires data to be grouped.")
  
  # Counts per group
  tbl <- data %>%
    summarise(n = dplyr::n(), .groups = "drop") %>%
    mutate(var_value = "Total")
  
  # Add overall row if requested
  if(overall) {
    overall_data <- data %>%
      ungroup() %>%
      summarise(n = n()) %>%
      mutate(var_value = "Total",
             !!group_vars[1] := "Overall")
    
    tbl <- bind_rows(tbl, overall_data)
    
    # Convert group variable to factor with levels including "Overall"
    tbl[[group_vars[1]]] <- factor(tbl[[group_vars[1]]],
                                   levels = c(levels(data[[group_vars[1]]]), "Overall"))
  }
  
  # Calculate row percentages
  total_n <- sum(tbl$n[tbl[[group_vars[1]]] != "Overall"])
  
  tbl <- tbl %>%
    mutate(
      pct = if_else(.data[[group_vars[1]]] == "Overall", 100, 100 * n / total_n)
    ) %>%
    mutate(
      !!n_name := formatC(n, format = "d", big.mark = sep, digits = 0),
      !!pct_name := paste0("(", pct_fmt(pct, dp = dp, decimal = FALSE, brackets = FALSE), ")")
    )
  
  # Pivot wider so counts and percentages appear in one row
  tbl <- tbl %>%
    select(all_of(group_vars), !!n_name, !!pct_name) %>%
    pivot_longer(cols = c(!!n_name, !!pct_name),
                 names_to = "stat", values_to = "value") %>%
    unite("stat_group", stat, all_of(group_vars), sep = "_") %>%
    pivot_wider(names_from = stat_group, values_from = value) %>%
    janitor::clean_names()
  
  # Add column 'var' at the start
  tbl <- tbl %>% mutate(var = "Total") %>% relocate(var)
  
  return(tbl)
}
