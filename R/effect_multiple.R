# Function to multiply effect sizes from a regression model, and adjust the estimate and CI appropriately
effect_multiple <- function(model, effect, multiple, exp = FALSE, alpha = 0.05, dp = 2) {
  est <- coef(model)[effect]
  se  <- sqrt(vcov(model)[effect, effect])
  
  new_est <- multiple * est
  new_se  <- multiple * se
  
  z <- qnorm(1 - (alpha / 2))
  
  estimate <- new_est
  lower <- new_est - (z*new_se)
  upper <- new_est + (z*new_se)
  
  if(exp) {
    estimate <- exp(estimate)
    lower <- exp(lower)
    upper <- exp(upper)
  }
  
  estimate <- estimate %>% round(dp) %>% format(nsmall = dp)
  lower <- lower %>% round(dp) %>% format(nsmall = dp)
  upper <- upper %>% round(dp) %>% format(nsmall = dp)
  ci <- paste0("(", lower, ", ", upper, ")")
  
  new_result <- c(estimate, ci)
  names(new_result) <- c("Estimate", paste0(round((1-alpha)*100, 1), "%CI"))
  
  return(new_result)
}