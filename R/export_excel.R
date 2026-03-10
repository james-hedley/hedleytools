# Function to export data to a specific worksheet within an excel workbook 
#' @export
export_excel <- function(x, file, sheet = "Sheet1") {
  # Check if workbook exists
  if (file.exists(file)) {
    wb <- loadWorkbook(file)
  } else {
    wb <- createWorkbook()
  }
  
  # Overwrite sheet if it already exists, otherwise create it
  if (sheet %in% names(wb)) {
    ws <- wb$worksheets[[which(names(wb) == sheet)]]
    
    if (ws$sheet_data$n_elements != 0) {
      rows <- ws$sheet_data$rows
      cols <- ws$sheet_data$cols
      deleteData(wb, sheet = sheet, rows = min(rows):max(rows), cols = min(cols):max(cols), gridExpand = TRUE)
    }
  } else {
    addWorksheet(wb, sheet)
  }
  
  # Save data to specified worksheet
  writeData(x = x, wb = wb, sheet = sheet, keepNA = FALSE, na.string = "")

  # Save workbook
  saveWorkbook(wb, file, overwrite = TRUE)
}
