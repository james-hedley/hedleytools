# Function to calculate the midpoint between two given dates
middate <- function(start, end) {
  as.Date(start) + ((as.Date(end) - as.Date(start)) / 2)
}