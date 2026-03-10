# Function to read in REDCap data that has been exported in R format
# There should be two files, a .csv file and a .r file
read_redcap <- function(file) {
  temp_env <- new.env()
  source(file, local = temp_env)
  data <- get("data", envir = temp_env) %>% as_tibble()
  rm(temp_env)
  return(data)
}


