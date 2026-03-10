# Function to insert characters at a specified position within a string
#' @export
str_insert <- function(x, insert, after_position, skip_short = TRUE) {
  
  skip_short_missing <- missing(skip_short)  # TRUE if user didn't pass it
  
  stopifnot(is.character(x), is.character(insert), length(insert) == 1)
  
  too_short <- str_length(x) <= after_position
  
  if (any(too_short)) {
    if (skip_short) {
      x[too_short] <- x[too_short]  # no change
      
      if (skip_short_missing) {
        warning(sum(too_short), 
                " string(s) shorter than 'after_position' (", after_position, 
                "), nothing inserted")
      }
    } else {
      x[too_short] <- paste0(x[too_short], insert)
      
      if (skip_short_missing) {
        warning(sum(too_short), 
                " string(s) shorter than 'after_position' (", after_position, 
                "), inserted at end instead")
      }
    }
  }
  
  x[!too_short] <- paste0(
    str_sub(x[!too_short], 1, after_position),
    insert,
    str_sub(x[!too_short], after_position + 1)
  )
  
  x
}
