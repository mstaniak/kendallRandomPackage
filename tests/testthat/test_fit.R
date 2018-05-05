context("Test fit-related functions")

set.seed(17)
rKend <- rkend(function(x) 1)
some_data <- rKend(100, 0.7)

fit_kend <- suppressWarnings(fit_kendall(some_data, function(x) 1, sort(runif(80)),
                        initial_point = c(5, 0, 1)))

testthat::test_that("Fit happened", {
  testthat::expect_is(fit_kend, "list")
  testthat::expect_is(fit_kend$estimated_alpha, "numeric")
  testthat::expect_is(fit_kend$estimated_location, "numeric")
  testthat::expect_is(fit_kend$estimated_scale, "numeric")
  testthat::expect_equal(is.finite(fit_kend$estimated_alpha), TRUE)
  testthat::expect_equal(is.finite(fit_kend$estimated_location), TRUE)
  testthat::expect_equal(is.finite(fit_kend$estimated_scale), TRUE)
})
