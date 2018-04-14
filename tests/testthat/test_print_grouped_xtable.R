context("Grouped Table")

test_that("Returns expected output STE__Age_Sex", {
  library(data.table)
  DT <- fread("print-grouped-xtable/STE__Age_Sex.tsv")
  print_grouped_xtable(DT, out.file = paste0("print-grouped-xtable/STE__Age_Sex-actual.tex"))
})
