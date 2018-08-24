context("grattan_percent")

test_that("fixed values", {
  expect_identical(grattan_percent(0.5), "50.0~per~cent")
  expect_identical(grattan_percent(0.5, 1), "50.0~per~cent")
  expect_identical(grattan_percent(0.49999, 2), "50.00~per~cent")
})
