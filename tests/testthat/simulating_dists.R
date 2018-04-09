# Check that support is on positive numbers

pKend <- pkend(function(x) 1)
testthat::expect_equal(pKend(c(-20, -10, 0), 0.5), rep(0, 3))
