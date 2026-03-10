# Function to add blank rows at the top of a dataset (tibble). If done after group_by(), adds extra rows for each group
#' @export
add_rows <- function(df, rows = 1, position = "top", newvar = NULL) {
    group_vars <- df %>% group_vars()
      
    group_order <- df %>% 
      select(all_of(group_vars(.))) %>%
      distinct() %>%
      ungroup() %>% 
      mutate(order_ = row_number()) %>%
      arrange(across(-order_)) %>%
      mutate(actual_order_ = row_number()) %>%
      arrange(order_) %>%
      pull(actual_order_)
    
    newdf <- df %>%
      group_split(.keep = TRUE) %>%
      map(~{
        blank <- slice(.x[1, 0], rep(1, rows))
        if(!is.null(newvar)) blank[[newvar]] <- 1
        if(!is.null(newvar)) .x[[newvar]] <- 0
        
        if(position == "top") {
          out <- bind_rows(blank, .x) %>% fill(all_of(group_vars), .direction = "up")
        } else {
          out <- bind_rows(.x, blank) %>% fill(all_of(group_vars), .direction = "down")
        }
        out
      }) %>%
      `[`(group_order) %>%
      bind_rows() %>%
      group_by(across(all_of(group_vars))) %>%
      relocate(all_of(newvar), .after = last_col())

  return(newdf)
}

