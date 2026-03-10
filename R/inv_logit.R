# Function for inverse logit operation
inv_logit <- function(x) {1 / (1 + exp(-x))}