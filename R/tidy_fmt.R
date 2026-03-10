# Function to format regression model output correctly after broom::tidy()
tidy_fmt <- function(model_object, dp = 2, ...) {
  model_object %>%
    tidy(...) %>%
    mutate(p.value = p_fmt(p.value)) %>%
    mutate(across(where(is.numeric), ~ coef_fmt(.x, dp = dp, truncate_small_values = TRUE)))
}
