# Function to load an object from a .RData file, and assign it to a specified name
#' @export
load_data <- function(file) {
  tmp <- new.env()
  objnames <- load(file, envir = tmp)   # load into temp env
  
  if (length(objnames) != 1) {
    stop("File contains ", length(objnames), " objects - expected exactly 1.")
  }
  
  get(objnames, envir = tmp)
}
