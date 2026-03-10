# Function for inverse logit operation
#' @export
inv_logit <- function(x) {1 / (1 + exp(-x))}
