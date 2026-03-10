# Function to summarise mean and SD for a given variable
mean_sd <- function(data, var, 
                    dp = 0,
                    mean_dp = NULL, 
                    sd_dp = NULL, 
                    sep = ",",
                    mean_name = "col1", 
                    sd_name = "col2",
                    overall = FALSE) {
  var <- enquo(var)
  
  mean_dp <- ifelse(is.null(mean_dp), dp, mean_dp)
  sd_dp   <- ifelse(is.null(sd_dp), dp, sd_dp)
  
  group_vars <- dplyr::group_vars(data)
  
  # Duplicate data for overall if requested
  if(overall && length(group_vars) > 0) {
    overall_data <- data %>%
      dplyr::ungroup() %>%
      mutate(across(all_of(group_vars), ~ "Overall"))
    data <- bind_rows(data, overall_data)
    data <- data %>% group_by(across(all_of(group_vars)))
  }
  
  # Summarise mean and SD
  tbl <- data %>% 
    summarise(
      mean_val = mean(!!var, na.rm = TRUE),
      sd_val   = sd(!!var, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      mean_val = formatC(mean_val, format = "f", digits = mean_dp, big.mark = sep),
      sd_val   = formatC(sd_val,   format = "f", digits = sd_dp),
      !!mean_name := mean_val,
      !!sd_name   := paste0("(", sd_val, ")"),
      var         = as_label(var)
    ) %>%
    select(var, !!mean_name, !!sd_name, all_of(group_vars))
  
  if(length(group_vars) > 0) {
    orig_group_vars <- group_vars
    
    tbl <- tbl %>%
      pivot_longer(
        cols = c(!!mean_name, !!sd_name),
        names_to = "stat",
        values_to = "value"
      ) %>%
      unite("stat_group", stat, all_of(group_vars), sep = "_") %>%
      pivot_wider(
        id_cols = var,
        names_from = stat_group,
        values_from = value
      )
    
    # Preserve column order: original groups first, overall at the end
    all_cols <- colnames(tbl)
    non_overall_cols <- all_cols[!grepl("_Overall$", all_cols)]
    overall_cols <- all_cols[grepl("_Overall$", all_cols)]
    tbl <- tbl[, c(non_overall_cols, overall_cols)]
    
    tbl <- janitor::clean_names(tbl)
    
    # Compute p-value only on original groups (exclude overall)
    if(length(group_vars) == 1) {
      grp <- group_vars[1]
      levels <- unique(dplyr::pull(data %>% filter(!!sym(grp) != "Overall"), !!sym(grp)))
      x <- data %>% filter(!!sym(grp) != "Overall") %>% select(!!var, !!sym(grp))
      
      if(length(levels) == 2) {
        grp1 <- x %>% filter(!!sym(grp) == levels[1]) %>% pull(!!var) %>% as.numeric()
        grp2 <- x %>% filter(!!sym(grp) == levels[2]) %>% pull(!!var) %>% as.numeric()
        test <- t.test(grp1, grp2)
        tbl <- tbl %>% mutate(p_value = p_fmt(test$p.value))
      } else if(length(levels) > 2) {
        test <- aov(as.formula(paste0(as_label(var), " ~ ", grp)), data = x)
        pval <- summary(test)[[1]][["Pr(>F)"]][1]
        tbl <- tbl %>% mutate(p_value = p_fmt(pval))
      } else {
        tbl <- tbl %>% mutate(p_value = "")
      }
    } else {
      tbl <- tbl %>% mutate(p_value = "")
    }
    
    tbl <- tbl %>% relocate(p_value, .after = last_col())
  }
  
  return(tbl)
}

