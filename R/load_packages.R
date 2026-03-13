# Load packages (and install them if not already installed) ----
#' @export
load_packages <- function() {
  
  ## pacman::p_load installs packages if not already installed, then loads them
  if (!requireNamespace("pacman", quietly = TRUE)) {install.packages("pacman")} # install pacman if not already installed
  suppressWarnings(library(pacman)) 
  
  ## Data manipulation
  p_load(tidyverse)
  p_load(janitor)
  p_load(skimr)
  p_load(labelled)
  p_load(lubridate)
  p_load(hms)
  p_load(stringr)
  p_load(stringi)
  p_load(dplyr)
  p_load(data.table)
  p_load(broom)
  p_load(broom.mixed)
  
  ## Importing/exporting in data
  p_load(haven)
  p_load(openxlsx)
  p_load(readxl)
  
  ## Plotting
  p_load(ggplot2)
  p_load(grDevices)
  p_load(ggplotify)
  p_load(ggh4x) 
  p_load(scales)
  
  ## Tables
  p_load(gtsummary)
  
  ## Image manipulation
  p_load(magick)
  
  
  ## Statistical analyses
  p_load(car)
  p_load(survival)
  p_load(emmeans)
  p_load(caret)
  if (!requireNamespace("units", quietly = TRUE)) {install.packages("units", type = "binary")} # Needed for epiR
  if (!requireNamespace("sf", quietly = TRUE)) {install.packages("sf", type = "binary")} # Needed for epiR
  p_load(epiR) # Agreement measures (sensitivity, specificity, etc.)
  p_load(cmprsk) # Competing risks regression
  p_load(lme4) # GLM with random effects
  p_load(MASS) # fitdistr() to get distribution parameters
  
  ## Mapping ICD codes
  p_load(touch)
  
  ## R development tools
  p_load(devtools)
  p_load(rlang)
  p_load(rstudioapi)
  
  ## Simulation tools
  p_load(TeachingDemos)
  p_load(truncnorm)
  
  ## Meta-analysis
  p_load(metafor)
  
  ## For ABS API
  p_load(readabs)
  
  ## For handling naming conflicts
  p_load(conflicted)
  
  
  # Manually specify which functions should 'win' in cases of conflict ----
  suppressMessages(conflict_prefer("filter", "dplyr"))
  suppressMessages(conflict_prefer("lag", "dplyr"))
  suppressMessages(conflict_prefer("select", "dplyr"))
  suppressMessages(conflict_prefer("rename", "dplyr"))
  suppressMessages(conflict_prefer("summarise", "dplyr"))
  suppressMessages(conflict_prefer("mutate", "dplyr"))
  suppressMessages(conflict_prefer("slice", "dplyr"))
  suppressMessages(conflict_prefer("count", "dplyr"))
  suppressMessages(conflict_prefer("between", "dplyr"))
  suppressMessages(conflict_prefer("year", "lubridate"))
  suppressMessages(conflict_prefer("recode", "dplyr"))
  suppressMessages(conflict_prefer("cluster", "survival"))
  suppressMessages(conflict_prefer("first", "dplyr"))
  suppressMessages(conflict_prefer("last", "dplyr"))
  suppressMessages(conflict_prefer("month", "lubridate"))
  suppressMessages(conflict_prefer("week", "lubridate"))
  suppressMessages(conflict_prefer("chisq.test", "stats"))

}
