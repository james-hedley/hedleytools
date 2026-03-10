# Function to replace all NA values in character variables with a specified string
#' @export
replace_all_na <- function(df, replace = NULL, replace_num = 0, replace_chr = "") {
  if(is.null(replace)) {
    df %>%
      mutate(across(where(is.numeric), ~ tidyr::replace_na(.x, replace_num))) %>%
      mutate(across(where(is.character), ~ tidyr::replace_na(.x, replace_chr)))
  } else {
    if(is.numeric(replace)) {
      df %>%
        mutate(across(where(is.numeric), ~ tidyr::replace_na(.x, replace))) 
    } else if (is.character(replace)) {
      df %>%
        mutate(across(where(is.character), ~ tidyr::replace_na(.x, replace))) 
    } else {
      stop("Replace is ", typeof(replace), ". Only numeric or character is supported")
    }
  }
}
