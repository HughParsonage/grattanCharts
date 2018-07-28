context("start rnw")

test_that("Error handling", {
  expect_error(start_rnw(path = 1:2), 
               "`path` had length 2, but must be length-one.", 
               fixed = TRUE)
  expect_error(start_rnw(path = 1), 
               "`path = 1` was type double, but must be a string.", 
               fixed = TRUE)
  
})


test_that("Rnw", {
  tempdA <- tempfile()
  hutils::provide.dir(tempdA)
  rnw_a <- tempfile(tmpdir = tempdA, fileext = ".Rnw")
  expect_equal(start_rnw(rnw_a, rstudio = FALSE), rnw_a)
  expect_true(file.exists(tempdA))
  
  tempdB <- tempfile()
  expect_equal(normalizePath(dirname(start_rnw(tempdB, rstudio = FALSE)), 
                             winslash = "/"),
               normalizePath(tempdB, winslash = "/"))
  expect_true(file.exists(file.path(tempdB, "Report.Rnw")))
})

