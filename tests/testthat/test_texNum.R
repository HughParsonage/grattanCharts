context("texNum")

test_that("Error handling", {
  expect_error(texNum(1:2), 
               regexp = "length")
  expect_error(texNum("a"), 
               regexp = "type character")
})

