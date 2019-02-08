context("Grouped Table")

test_that("Returns expected output STE__Age_Sex", {
  library(data.table)
  DT <- fread("print-grouped-xtable/STE__Age_Sex.tsv")
  expected <- readLines("print-grouped-xtable/STE__Age_Sex-expected.tex")
  print_grouped_xtable(DT,
                       group_by = c("STE_NAME16", "Age"),
                       out.file = paste0("print-grouped-xtable/STE__Age_Sex-actual.tex"))
  actual <- readLines("print-grouped-xtable/STE__Age_Sex-actual.tex")
  expect_equal(expected, actual)
})

