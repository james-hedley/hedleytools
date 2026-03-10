# Calculate global p-value from coefficients and their corresponding variance/covariance matrix
#' @export
global_p_calc <- function(coef, V, varname = NULL, coef_index = NULL) {
  # Error messages
  if(is.null(varname) & is.null(coef_index)) stop("Either a factor variable name or coefficinet index(es) must be specified")
  if(!is.null(varname) & is.null(names(coef))) stop("coef must be a named vector of coefficients, no names were provided")
  
  # Determine coefficient indices
  if(!is.null(varname)) {
    i <- which(str_starts(names(coef), varname))
  } else {
    i <- coef_index
  } 
  
  # Calculate global p-value
  b_i <- as.numeric(coef[i])
  V_i <- V[i, i]
  chi2 <- as.numeric(t(b_i) %*% solve(V_i) %*% b_i)
  df <- length(b_i)
  pval <- 1 - pchisq(chi2, df)
  
  
  # Return formatted p-value
  return(p_fmt(pval))
}

