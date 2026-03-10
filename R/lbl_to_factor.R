# Function: Convert all labelled variables in a dataset to factors
# Created by: James Hedley
# Date created: 18th August 2025

## After reading in a Stata dataset with read_dta, convert all labelled variables to factors
## so the labels display properly in tables etc. 
## Note this removes some information, e.g. first digit of ethnicity code is often 
## useful for broad groupings, this detail is lost when converted to a factor
## This function also saves the original labels in a separate data dictionary
## called "value_labels", which is attached to the dataset as an attribute
## You can view this with: data %>% lbl_to_factor() %>% attr("value_labels")
lbl_to_factor <- function(data) {
  library(dplyr)
  library(labelled)
  
  dict <- lbl_dict(data)
  newdata <- mutate(data, across(where(is.labelled), to_factor))
  attr(newdata, "value_labels") <- dict
  return(newdata)
}