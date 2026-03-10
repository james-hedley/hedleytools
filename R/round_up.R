# Function to round up to nearest specified multiple
round_up <- function(x, multiple = 1) {
  ceiling(x / multiple) * multiple
}