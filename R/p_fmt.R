# Function: Format p-values to standard format (1 significant figure, up to 3 decimal places)
# Created by: James Hedley
# Date created: 18th August 2025

p_fmt <- function(p) {
  # If p is a character, try to convert to numeric
  p_num <- suppressWarnings(as.numeric(p))
  
  # Format numeric p to 1 significant figure, maximum 3 decimal places
  p_formatted <- case_when(
    p_num >= 0.9 ~ "0.9",
    p_num >= 0.095 ~ sprintf("%.1f", p_num),
    p_num >= 0.0095 ~ sprintf("%.2f", p_num),
    p_num >= 0.001 ~ sprintf("%.3f", p_num),
    p_num < 0.001 ~ "<0.001",
    TRUE ~ as.character(p))
  
  # Return formatted p
  return(p_formatted)
} 
