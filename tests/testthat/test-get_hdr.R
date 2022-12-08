test_that("structure of get_hdr() return value is as expected", {

  data <- data.frame(
    x = 1:10,
    y = rep(1:5, each = 2)
  )

  res <- get_hdr(data)

  # Checking the top level of res
  expect_type(res, "list")
  expect_equal(length(res), 3)
  expect_equal(names(res), c("df_est", "breaks", "data"))

  # Checking res$df_est:
  expect_type(res$df_est, "list")
  expect_equal(ncol(res$df_est), 5)
  expect_equal(colnames(res$df_est), c("x", "y", "fhat", "fhat_discretized", "hdr"))

  # Checking res$data
  expect_type(res$data, "list")
  expect_equal(ncol(res$data), 3)
  expect_equal(nrow(res$data), 10)
  expect_equal(colnames(res$data), c("x", "y", "hdr_membership"))

  # Checking res$breaks
  expect_type(res$breaks, "double")
  expect_equal(length(res$breaks), 5)
  expect_equal(names(res$breaks), c("99%", "95%", "80%", "50%", NA))


  # Now with non-default args -----------------------------------------
  res <- get_hdr(data, probs = c(.989, .878, .67, .43, .21), hdr_membership = FALSE)

  # Checking res$data
  expect_equal(ncol(res$data), 2)

  # Checking res$breaks
  expect_type(res$breaks, "double")
  expect_equal(length(res$breaks), 6)
  expect_equal(names(res$breaks), c("99%", "88%", "67%", "43%", "21%", NA))

})

test_that("`method` can be provided as a character vector or function", {

  data <- data.frame(
    x = 1:10,
    y = rep(1:5, each = 2)
  )

  expect_equal(get_hdr(data, "kde"), get_hdr(data, method_kde()))
  expect_equal(get_hdr(data, "mvnorm"), get_hdr(data, method_mvnorm()))
  expect_equal(get_hdr(data, "freqpoly"), get_hdr(data, method_freqpoly()))
  expect_equal(get_hdr(data, "histogram"), get_hdr(data, method_histogram()))

})

test_that("get_hdr() errors informatively if bad `method` argument", {

  data <- data.frame(
    x = 1:10,
    y = rep(1:5, each = 2)
  )

  expect_error(get_hdr(data, method = "not-a-method"), regexp = "Invalid method specified")
  expect_error(get_hdr(data, method = method_kde), regexp = "did you forget")

})


# # The data used for tests:
#
# set.seed(1)
# df <- data.frame(
#   x = rnorm(5e3),
#   y = rnorm(5e3)
# )
#
# write_rds(df, here::here("tests/testthat/fixtures/df_norm.rds"))

test_that("get_hdr(method = method_kde()) calculations are consistent", {

  data <- readRDS(test_path("fixtures", "df_norm.rds"))

  res <- get_hdr(data, method_kde())

  # fhat_discretized should be normalized to sum to 1
  expect_equal(sum(res$df_est$fhat_discretized), 1)

  # By default, estimate is evaluated on the same range as original data
  expect_equal(range(res$df_est$x), range(data$x))
  expect_equal(range(res$df_est$y), range(data$y))

  # default grid is 100 x 100:
  expect_equal(nrow(res$df_est), 100 * 100)

  # Checksums:
  expect_equal(round(sum(res$df_est$fhat)), 185)
  expect_equal(as.numeric(round(res$breaks, 4)), c(0.0017, 0.0083, 0.0303, 0.0731, Inf))
  expect_equal(sort(unique(res$df_est$hdr)), c(.5, .8, .95, .99, 1))
  expect_equal(as.numeric(table(res$df_est$hdr)), c(858, 1149, 1597, 1771, 4625))


  # Checking non-default args ------------------------

  res <- get_hdr(data, method_kde(adjust = .4), probs = c(.97, .85, .4, .1), n = c(100, 200), rangex = c(-3, 2), rangey = c(-1, 3))
  # fhat_discretized should be normalized to sum to 1
  expect_equal(sum(res$df_est$fhat_discretized), 1)

  # Was the custom range used
  expect_equal(range(res$df_est$x), c(-3, 2))
  expect_equal(range(res$df_est$y), c(-1, 3))

  # default grid is 100 x 100:
  expect_equal(nrow(res$df_est), 100 * 200)

  # Checksums:
  expect_equal(round(sum(res$df_est$fhat)), 808)
  expect_equal(as.numeric(round(res$breaks, 4)), c(0.0105, 0.0352, 0.1036, 0.1522, Inf))
  expect_equal(sort(unique(res$df_est$hdr)), c(.1, .4, .85, .97, 1))
  expect_equal(as.numeric(table(res$df_est$hdr)), c(495, 1923, 5584, 4524, 7474))

})

# TODO: above, for other methods




test_that("get_hdr() works with custom function factory supplied to `method`", {

  data <- readRDS(test_path("fixtures", "df_norm.rds"))

  method_mvnorm_ind <- function() {

    function(data) {

      mean_x <- mean(data$x); s_x <- sd(data$x)
      mean_y <- mean(data$y); s_y <- sd(data$y)

      function(x, y) dnorm(x, mean = mean_x, sd = s_x) * dnorm(y, mean = mean_y, sd = s_y)

    }

  }

  res <- get_hdr(data, method = method_mvnorm_ind())

  # fhat_discretized should be normalized to sum to 1
  expect_equal(sum(res$df_est$fhat_discretized), 1)

  # By default, estimate is evaluated on the same range as original data
  expect_equal(range(res$df_est$x), range(data$x))
  expect_equal(range(res$df_est$y), range(data$y))

  # default grid is 100 x 100:
  expect_equal(nrow(res$df_est), 100 * 100)

  # Checksums:
  expect_equal(round(sum(res$df_est$fhat)), 185)
  expect_equal(as.numeric(round(res$breaks, 4)), c(0.0017, 0.0078, 0.031, 0.078, Inf))
  expect_equal(sort(unique(res$df_est$hdr)), c(.5, .8, .95, .99, 1))
  expect_equal(as.numeric(table(res$df_est$hdr)), c(826, 1090, 1642, 1863, 4579))

})



test_that("get_hdr() works with custom function factory supplied to `method`", {

  data <- readRDS(test_path("fixtures", "df_norm.rds"))

  method_fixed_grid <- function() {

    function(data, n, rangex, rangey) {

      df_grid <- expand.grid(
        x = seq(rangex[1], rangex[2], length.out = n),
        y = seq(rangey[1], rangey[2], length.out = n)
      )

      df_grid$fhat <- dnorm(df_grid$x) * dnorm(df_grid$y)

      df_grid

    }

  }

  res <- get_hdr(data, method = method_fixed_grid())

  # fhat_discretized should be normalized to sum to 1
  expect_equal(sum(res$df_est$fhat_discretized), 1)

  # By default, estimate is evaluated on the same range as original data
  expect_equal(range(res$df_est$x), range(data$x))
  expect_equal(range(res$df_est$y), range(data$y))

  # default grid is 100 x 100:
  expect_equal(nrow(res$df_est), 100 * 100)

  # Checksums:
  expect_equal(round(sum(res$df_est$fhat)), 185)
  expect_equal(as.numeric(round(res$breaks, 4)), c(0.0017, 0.008, 0.0321, 0.0796, Inf))
  expect_equal(sort(unique(res$df_est$hdr)), c(.5, .8, .95, .99, 1))
  expect_equal(as.numeric(table(res$df_est$hdr)), c(806, 1065, 1603, 1824, 4702))

})


test_that("get_hdr() fails if `method != 'fun' and `data` isn't provided", {

  expect_error(get_hdr(method = method_kde()), regexp = ".data. must be provided")

})

test_that("fun argument of get_hdr() requires rangex/y", {

  expect_error(get_hdr(method = "fun", fun = function(x, y) dexp(x) * dexp(y)), regexp = ".rangey. must be provided")

})


test_that("fun argument of get_hdr() works", {

  res <- get_hdr(method = "fun", fun = function(x, y) dexp(x) * dexp(y), rangex = c(0, 10), rangey = c(0, 10))

  # Structure of res is as expected
  expect_type(res, "list")
  expect_equal(length(res), 3)
  expect_equal(names(res), c("df_est", "breaks", "data"))

  expect_null(res$data)

  # fhat_discretized should be normalized to sum to 1
  expect_equal(sum(res$df_est$fhat_discretized), 1)

  expect_equal(range(res$df_est$x), c(0, 10))
  expect_equal(range(res$df_est$y), c(0, 10))

  # default grid is 100 x 100:
  expect_equal(nrow(res$df_est), 100 * 100)

  # Checksums:
  expect_equal(round(sum(res$df_est$fhat)), 108)
  expect_equal(as.numeric(round(res$breaks, 4)), c(0.0014, 0.0096, 0.0534, 0.1987, Inf))
  expect_equal(sort(unique(res$df_est$hdr)), c(.5, .8, .95, .99, 1))
  expect_equal(as.numeric(table(res$df_est$hdr)), c(145, 306, 669, 1045, 7835))
})





































