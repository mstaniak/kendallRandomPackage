context("Check function related to stable Kendall distribution")

pKend <- pkend(function(x) 1)
testthat::test_that("Check that support is on positive numbers", {
  testthat::expect_equal(pKend(c(-20, -10, 0), 0.5), rep(0, 3))

})
