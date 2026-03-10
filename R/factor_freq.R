# Function to convert a variable to a factor with levels in order of frequency
#' @export
factor_freq <- function(var, order = "down", last = NA) {
  if(order == "down") {
    observed_levels <- tabyl(var) %>% as_tibble() %>% arrange(desc(n)) %>% `[`(,1) %>% pull() %>% as.character()
  } else {
    observed_levels <- tabyl(var) %>% as_tibble() %>% arrange(n) %>% `[`(,1) %>% pull() %>% as.character()
  }

  levels <- c(observed_levels[!(observed_levels %in% c(last, NA))], last, NA)
  
  fct <- factor(as.character(var), levels = levels) %>% droplevels()
  
  return(fct)
}
