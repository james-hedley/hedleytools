# Function for Z score based on a given alpha
z_alpha <- function(alpha = 0.05) {qnorm(1 - alpha/2)}
