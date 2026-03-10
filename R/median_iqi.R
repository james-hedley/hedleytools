# Function to summarise median and IQI for a given variable
median_iqi <- function(data, var, 
                       dp = 0,
                       median_dp = NULL, 
                       iqi_dp = NULL, 
                       sep = ",",
                       median_name = "col1", 
                       iqi_name = "col2",
                       overall = FALSE) {
  
  var <- enquo(var)
  
  median_dp <- ifelse(is.null(median_dp), dp, median_dp)
  iqi_dp    <- ifelse(is.null(iqi_dp), dp, iqi_dp)
  
  group_vars <- dplyr::group_vars(data)
  
  # Duplicate data for overall if requested
  if(overall && length(group_vars) > 0) {
    overall_data <- data %>%
      dplyr::ungroup() %>%
      mutate(across(all_of(group_vars), ~ "Overall"))
    data <- bind_rows(data, overall_data)
    data <- data %>% group_by(across(all_of(group_vars)))
  }
  
  # Summarise median and IQR
  tbl <- data %>% 
    summarise(
      median_val = quantile(!!var, 0.5, na.rm = TRUE),
      lower      = quantile(!!var, 0.25, na.rm = TRUE),
      upper      = quantile(!!var, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      median_val = formatC(median_val, format = "f", digits = median_dp, big.mark = sep),
      lower      = formatC(lower, format = "f", digits = iqi_dp, big.mark = sep),
      upper      = formatC(upper, format = "f", digits = iqi_dp, big.mark = sep),
      !!median_name := median_val,
      !!iqi_name    := paste0("(", lower, "-", upper, ")"),
      var           = as_label(var)
    ) %>%
    select(var, !!median_name, !!iqi_name, all_of(group_vars))
  
  # Only reshape if there is at least one grouping variable
  if(length(group_vars) > 0) {
    # Remember original column order
    orig_group_vars <- group_vars
    
    tbl <- tbl %>%
      pivot_longer(
        cols = c(!!median_name, !!iqi_name),
        names_to = "stat",
        values_to = "value"
      ) %>%
      unite("stat_group", stat, all_of(group_vars), sep = "_") %>%
      pivot_wider(
        id_cols = var,
        names_from = stat_group,
        values_from = value
      )
    
    # Keep original group column order, overall columns at end
    all_cols <- colnames(tbl)
    non_overall_cols <- all_cols[!grepl("_Overall$", all_cols)]
    overall_cols <- all_cols[grepl("_Overall$", all_cols)]
    tbl <- tbl[, c(non_overall_cols, overall_cols)]
    
    # Clean names but do not reorder
    tbl <- janitor::clean_names(tbl)
    
    
    # Calculate p-value if exactly one original grouping variable (exclude Overall)
    if(length(group_vars) == 1) {
      grp <- group_vars[1]
      levels <- unique(dplyr::pull(data %>% filter(!!sym(grp) != "Overall"), !!sym(grp)))
      x <- data %>% filter(!!sym(grp) != "Overall") %>% select(!!var, !!sym(grp))
      
      if(length(levels) == 2) {
        grp1 <- x %>% filter(!!sym(grp) == levels[1]) %>% pull(!!var) %>% as.numeric()
        grp2 <- x %>% filter(!!sym(grp) == levels[2]) %>% pull(!!var) %>% as.numeric()
        test <- wilcox.test(grp1, grp2)
        tbl <- tbl %>% mutate(p_value = p_fmt(test$p.value))
      } else if(length(levels) > 2) {
        test <- kruskal.test(as.formula(paste0(as_label(var), " ~ ", grp)), data = x)
        tbl <- tbl %>% mutate(p_value = p_fmt(test$p.value))
      } else {
        tbl <- tbl %>% mutate(p_value = "")
      }
    } else {
      tbl <- tbl %>% mutate(p_value = "")
    }
    
    # Ensure p_value is final column
    tbl <- tbl %>% relocate(p_value, .after = last_col())
  }
  
  return(tbl)
}
