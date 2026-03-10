# Function to switch order of rows in a dataset
#' @export
switch_rows <- function(df, row1, row2) {
  df_row1 <- df[row1,]
  df_row2 <- df[row2,]
  
  df[row1,] <- df_row2
  df[row2,] <- df_row1
  
  return(df)
}
