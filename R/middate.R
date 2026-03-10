# Function to calculate the midpoint between two given dates
#' @export
middate <- function(start, end) {
  as.Date(start) + ((as.Date(end) - as.Date(start)) / 2)
}
