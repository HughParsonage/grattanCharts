#' Bundle chart data
#' @param chart_data_table A data.table including a Figure C.M column (\code{fig_no}) and the file name for the 
#' figure image (\code{extract}).
#' @param chart_data.xlsx Excel file to write to.
#' @export

bundle_chart_data <- function(chart_data_table,
                              chart_data.xlsx = "atlas/bundled-chart-data.xlsx") {
  if (!requireNamespace("xlsx", quietly = TRUE)) {
    stop("`xlsx` is not a strict dependency of 'grattanCharts', but is required ",
         "for `bundle_chart_data()`.")
  }
  stopifnot("extract" %in% names(chart_data_table))
  
  wb <- xlsx::createWorkbook()
  headerStyle <- xlsx::CellStyle(wb) + xlsx::Font(wb, isBold=TRUE) + xlsx::Border()
  
  for (r in seq_len(nrow(chart_data_table))) {
    the_row <- chart_data_table[r]
    extract <- the_row[["extract"]]
    the_png <- normalizePath(sub("-1(-crop)?(\\.pdf)?$", "-1.png", extract))
    the_csv <- normalizePath(sub("-1(-crop)?(\\.pdf)?$", ".csv", extract))
    if (file.exists(the_csv)) {
      the_csv_data <- tryCatch(fread(the_csv), error = function(e) cat(the_csv, "\n"))
      if (is.null(the_csv_data)) {
        the_sheet <- xlsx::createSheet(wb, the_row[["fig_no"]])
        xlsx::addDataFrame(the_csv_data, sheet = the_sheet, row.names = TRUE, colnamesStyle = headerStyle)
        tryCatch(xlsx::addPicture(the_png, sheet = the_sheet, startRow = 2, startColumn = ncol(the_csv_data) + 2, scale = 0.33),
                 error = function(e) {
                   cat(the_png, "\n")
                   print(the_row)
                 })
      }
    }
  }
  xlsx::saveWorkbook(wb, file = normalizePath(chart_data.xlsx))
}