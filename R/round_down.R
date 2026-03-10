# Function to round down to nearest specified multiple
#' @export
round_down <- function(x, multiple = 1) {
  floor(x / multiple) * multiple
}
