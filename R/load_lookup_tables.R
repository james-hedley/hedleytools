# Function to load CODE lookup tables - SEIFA score by ABS area
load_lookup_tables <- function() {
  pkgloc <- file.path(rds, "PRJ-CODE/Shared code/Lookup tables")
  
  folders <- list.files(pkgloc)
  
  for(folder in folders){
    files <- list.files(path = file.path(pkgloc, folder), pattern = "\\.RData$", recursive = FALSE, full.names = TRUE)
    
    for(file in files) {
      load(file, envir = .GlobalEnv)
    }
  }
}