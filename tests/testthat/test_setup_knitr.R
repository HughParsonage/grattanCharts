context("Setup knitr")

test_that("Error messages for halt_if_xtable_unattached", {
  if (hutils::isAttached("knitr")) {
    detach("package:knitr")
  }
  expect_message(setup_knitr_for_grattex(), 
                 "xtable does not appear to be attached")
  expect_error(setup_knitr_for_grattex(halt_if_xtable_unattached = TRUE),
               "xtable does not appear to be attached")
})
