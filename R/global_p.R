# Function to extract global p-values from a model object
#' @export
global_p <- function(model) {
  car::Anova(model) %>% 
    broom::tidy() %>%
    mutate(pvalue = p_fmt(p.value)) %>%
    rename(variable = term) %>%
    select(variable, pvalue)
}
