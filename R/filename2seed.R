# Function to turn the full file path and file name of the current script into a random number seed
filename2seed <- function(set = FALSE) {
  filename <- rstudioapi::getActiveDocumentContext()$path
  string <- rlang::hash(filename)
  seed <- TeachingDemos::char2seed(string, set = set)
  
  if (set == TRUE) {
    set.seed(seed)
  } else {
    return(seed)
  }
}