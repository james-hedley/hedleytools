# Function: Generate a data dictionary as a tibble, just for labelled variables
#' @export
# Created by: James Hedley
# Date created: 18th August 2025

lbl_dict <- function(data) {
  
  # Only labelled variables
  labelled_vars <- select(data, where(is.labelled))
  
  # Map each variable to a tibble of value/label
  dict_list <- map2(labelled_vars, names(labelled_vars), ~{
    lbls <- attr(.x, "labels")
    tibble(
      variable = .y,
      value = as.vector(lbls),
      label = names(lbls)
    )
  })
  
  # Combine into one tibble
  dict_tbl <- bind_rows(dict_list)
  
  return(dict_tbl)
}

