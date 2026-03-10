# Function to run code_setup.R script
#' @export
code_setup <- function() {
  source(file.path(unname(ifelse(Sys.info()["sysname"]=="Windows",
                                 "//shared.sydney.edu.au/research-data",
                                 "/Volumes")), paste0("PRJ-CODE/Shared code/code_setup", ".R")), echo = TRUE)
}
