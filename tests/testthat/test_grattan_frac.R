context("Grattan_frac")

test_that("Returns correct", {
  expect_equal(Grattan_frac(0.74, hedges = c("Almost", "Over")), "Almost three-quarters")
  expect_equal(Grattan_frac(0.74, hedges = c("Squiggle", "Pop")), "Squiggle three-quarters")
  expect_equal(Grattan_frac(0.76, hedges = c("Squiggle", "Pop")), "Pop three-quarters")
})