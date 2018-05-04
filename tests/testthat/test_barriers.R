context("Test functions related to barrier crossing")

set.seed(17)
kendall_rw <- simulate_kendall_rw(1000, 100, runif, 0.75)
symmetric_kendall_rw <- simulate_kendall_rw(1000, 100, rnorm, 0.25)

moment <- ladder_moment(kendall_rw, 100)
moment2 <- ladder_moment(kendall_rw, 200)
symmetric_moment <- ladder_moment(symmetric_kendall_rw, 1000)

height  <- ladder_height(kendall_rw, 100)
height2 <- ladder_height(kendall_rw, 200)
symmetric_height <- ladder_height(symmetric_kendall_rw, 1000)

testthat::test_that("Object have the right type", {
  testthat::expect_is(symmetric_moment, "tbl_df")
  testthat::expect_is(symmetric_moment, "kendall_barrier_crossing")

  testthat::expect_is(moment, "tbl_df")
  testthat::expect_is(moment, "kendall_barrier_crossing")

  testthat::expect_is(symmetric_height, "tbl_df")
  testthat::expect_is(symmetric_height, "kendall_barrier_crossing")

  testthat::expect_is(height, "tbl_df")
  testthat::expect_is(height, "kendall_barrier_crossing")
})

testthat::test_that("Object have the right size", {
  testthat::expect_equal(nrow(moment), 1000)
  testthat::expect_equal(nrow(symmetric_moment), 1000)
  testthat::expect_equal(nrow(height), 1000)
  testthat::expect_equal(nrow(symmetric_height), 1000)
})

testthat::test_that("Bigger level implies greater mean of the distribution", {
  testthat::expect_gte(mean(moment2$ladder_moment, na.rm = T),
                       mean(moment$ladder_moment, na.rm = T))
  testthat::expect_gte(mean(height2$ladder_moment, na.rm = T),
                       mean(height$ladder_moment, na.rm = T))

})

testthat::test_that("S3 methods are fine", {
  testthat::expect_silent(plot(moment))
  testthat::expect_output(print(height))
})

