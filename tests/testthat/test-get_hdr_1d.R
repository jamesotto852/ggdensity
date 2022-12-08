test_that("structure of get_hdr_1d() return value is as expected", {

  x <- 1:10

  res <- get_hdr_1d(x)

  # Checking the top level of res
  expect_type(res, "list")
  expect_equal(length(res), 3)
  expect_equal(names(res), c("df_est", "breaks", "data"))

  # Checking res$df_est:
  expect_type(res$df_est, "list")
  expect_equal(ncol(res$df_est), 4)
  expect_equal(colnames(res$df_est), c("x", "fhat", "fhat_discretized", "hdr"))

  # Checking res$data
  expect_type(res$data, "list")
  expect_equal(ncol(res$data), 2)
  expect_equal(nrow(res$data), 10)
  expect_equal(colnames(res$data), c("x", "hdr_membership"))

  # Checking res$breaks
  expect_type(res$breaks, "double")
  expect_equal(length(res$breaks), 5)
  expect_equal(names(res$breaks), c("99%", "95%", "80%", "50%", NA))

})

test_that("`method` can be provided as a character vector or function", {

  x <- 1:10

  expect_equal(get_hdr_1d(x, "kde"), get_hdr_1d(x, method_kde_1d()))
  expect_equal(get_hdr_1d(x, "norm"), get_hdr_1d(x, method_norm_1d()))
  expect_equal(get_hdr_1d(x, "freqpoly"), get_hdr_1d(x, method_freqpoly_1d()))
  expect_equal(get_hdr_1d(x, "histogram"), get_hdr_1d(x, method_histogram_1d()))

})

test_that("get_hdr() errors informatively if bad `method` argument", {

  x <- 1:10

  expect_error(get_hdr_1d(x, method = "not-a-method"), regexp = "Invalid method specified")
  expect_error(get_hdr_1d(x, method = method_kde_1d), regexp = "did you forget")
  expect_error(get_hdr_1d(x, method = method_kde()), regexp = "1d")

})

test_that("get_hdr_1d() fails if `method != 'fun' and `x` isn't provided", {

  expect_error(get_hdr_1d(method = method_kde_1d()), regexp = ".x. must be provided")

})

test_that("fun argument of get_hdr_1d() requires range", {

  expect_error(get_hdr_1d(method = "fun", fun = dexp), regexp = ".range. must be provided")

})


test_that("fun argument of get_hdr_1d() works", {

  res <- get_hdr_1d(method = "fun", fun = dexp, range = c(0, 10))

  # Structure of res is as expected
  expect_type(res, "list")
  expect_equal(length(res), 3)
  expect_equal(names(res), c("df_est", "breaks", "data"))

  expect_null(res$data)

  # fhat_discretized should be normalized to sum to 1
  expect_equal(sum(res$df_est$fhat_discretized), 1)

  expect_equal(range(res$df_est$x), c(0, 10))

  # default grid is 512:
  expect_equal(nrow(res$df_est), 512)

  # Checksums:
  expect_equal(round(sum(res$df_est$fhat)), 52)
  expect_equal(as.numeric(round(res$breaks, 4)), c(0.0101, 0.0501, 0.201, 0.5041, Inf))
  expect_equal(sort(unique(res$df_est$hdr)), c(.5, .8, .95, .99, 1))
  expect_equal(as.numeric(table(res$df_est$hdr)), c(36, 47, 71, 82, 276))
})

