# Function to determine which rows of a dataset meet specified conditions
which_row <- function(data, ...) {
  # Capture the logical conditions
  conditions <- enquos(...)
  
  data %>%
    mutate(.rn = row_number()) %>%       # add row numbers
    filter(!!!conditions) %>%            # apply all conditions
    pull(.rn)                            # return row numbers
}
