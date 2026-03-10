# Function to summarise count and percentage for a given categorical variable
#' @export
n_pct <- function(data, var, 
                  dp = 0,
                  sep = ",",
                  n_name = "col1", 
                  pct_name = "col2",
                  header = TRUE,
                  overall = FALSE) {
  
  var <- enquo(var)
  group_vars <- dplyr::group_vars(data)
  var_label <- as_label(var)
  
  if (length(group_vars) > 1) stop("n_pct() only supports at most 1 grouping variable")
  
  # Duplicate data for overall if requested
  if (overall && length(group_vars) == 1) {
    grp <- group_vars[1]
    levels_grp <- if (is.factor(data[[grp]])) levels(data[[grp]]) else sort(unique(data[[grp]]))
    overall_data <- data %>%
      dplyr::ungroup() %>%
      mutate(!!grp := "Overall")
    data <- bind_rows(data, overall_data)
    data <- data %>% mutate(!!grp := factor(.data[[grp]], levels = c(levels_grp, "Overall"))) %>%
      group_by(across(all_of(group_vars)))
  }
  
  # Capture observed levels so we can preserve ordering later
  observed <- dplyr::pull(data, !!var)
  observed_levels <- if (is.factor(observed)) levels(observed) else sort(unique(as.character(observed)))
  
  # Count including NAs, but use complete() so missing combinations get n = 0
  tbl <- data %>%
    group_by(!!!syms(group_vars), !!var) %>%
    summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::complete(!!!syms(group_vars), !!var, fill = list(n = 0)) %>%
    mutate(var_value = ifelse(is.na(as.character(!!var)), "Not reported", as.character(!!var)))
  
  # Calculate denominators per group
  tbl <- tbl %>%
    group_by(!!!syms(group_vars)) %>%
    mutate(
      denom_all   = sum(n),
      denom_nonNA = sum(n[var_value != "Not reported"])
    ) %>%
    ungroup()
  
  # Calculate percentages (avoid divide by zero)
  tbl <- tbl %>%
    mutate(
      pct = dplyr::if_else(
        denom_all == 0 | denom_nonNA == 0, 
        0,
        dplyr::if_else(
          var_value == "Not reported",
          100 * n / denom_all,
          100 * n / denom_nonNA
        )
      ),
      pct = pct_fmt(pct, dp = dp, decimal = FALSE),
      !!n_name   := formatC(n, format = "d", big.mark = sep, digits = 0),
      !!pct_name := paste0("(", pct, ")")
    ) %>%
    mutate(var = var_label, level = var_value)
  
  # Drop "Not reported" if no missing values
  if (!any(is.na(dplyr::pull(data, !!var)))) {
    tbl <- tbl %>% filter(level != "Not reported")
  }
  
  # Reshape and header (handle duplicates safely)
  if (length(group_vars) > 0) {
    tbl <- tbl %>%
      pivot_longer(cols = c(!!n_name, !!pct_name),
                   names_to = "stat", values_to = "value") %>%
      unite("stat_group", stat, all_of(group_vars), sep = "_") %>%
      pivot_wider(
        id_cols = c(var, level), 
        names_from = stat_group, 
        values_from = value,
        values_fn = function(x) paste(x, collapse = ";")
      ) %>%
      janitor::clean_names()
  }
  
  # Add header row if requested
  if (header) tbl <- tbl %>% add_rows(1)
  
  # Convert level to character, sort rows
  tbl <- tbl %>%
    mutate(level = as.character(level)) %>%
    mutate(level = dplyr::coalesce(level, var_label)) %>%
    mutate(sort_var = factor(level, levels = c(var_label, observed_levels, "Not reported"))) %>%
    arrange(sort_var) %>%
    select(-c(sort_var, var)) %>%
    rename(var = level) %>%
    mutate(var = if_else(var == var_label, var, paste0(" ", var)))
  
  # Fill remaining NAs with ""
  tbl <- tbl %>% mutate(across(where(is.character), ~replace_na(.x, "")))
  
  # P-value if exactly one group
  if (header && length(group_vars) == 1) {
    grp <- group_vars[1]
    x <- data %>% select(all_of(c(var_label, grp)))
    tab <- table(x, useNA = "ifany")
    if (all(dim(tab) > 1)) {
      test <- suppressWarnings(stats::chisq.test(tab))
      pval <- test$p.value
      tbl <- tbl %>% mutate(p_value = if_else(var == var_label, p_fmt(pval), ""))
    } else {
      tbl <- tbl %>% mutate(p_value = "")
    }
  }
  
  # Ensure p_value is last column
  if (length(group_vars) == 1) {
    if (!"p_value" %in% colnames(tbl)) tbl <- tbl %>% mutate(p_value = "")
    tbl <- tbl %>% relocate(p_value, .after = last_col())
  }
  
  # Order all columns
  if (length(group_vars) == 0) {
    # Single grouping var: just keep the main n/pct cols (user-provided names)
    keep_cols <- c("var", n_name, pct_name)
    keep_cols <- keep_cols[keep_cols %in% names(tbl)]
    tbl <- tbl %>% select(all_of(keep_cols))
    
  } else if (length(group_vars) == 1) {
    # Multiple grouping vars: collect dynamic cols
    dynamic_cols <- c(
      grep(paste0("^", n_name, "_"), names(tbl), value = TRUE),
      grep(paste0("^", pct_name, "_"), names(tbl), value = TRUE)
    )
    
    # Capture all "overall_" columns + optional p_value
    overall_cols <- grep("^overall_", names(tbl), value = TRUE)
    optional_cols <- c(overall_cols, intersect("p_value", names(tbl)))
    
    tbl <- tbl %>% select(var, all_of(dynamic_cols), all_of(optional_cols))
  }
  
  return(tbl)
}

