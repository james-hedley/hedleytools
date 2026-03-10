# Function to round to nearest specified multiple
#' @export
round_nearest <- function(x, multiple = 1) {
  round_half_up(x / multiple) * multiple
}
