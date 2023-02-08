test_that("res_to_df_1d returns correct structure for each value of output", {

  data <- readRDS(test_path("fixtures", "df_norm.rds"))
  probs <- c(.99, .95, .80, .50)

  res <- get_hdr_1d(data$x, method_kde_1d(), probs)

  # Checking output == "rug"
  df_rug <- res_to_df_1d(res, probs, group = 1, output = "rug")
  expect_type(df_rug, "list")
  expect_equal(colnames(df_rug), c("x", "fhat", "fhat_discretized", "probs"))
  expect(is.ordered(df_rug$probs), "probs is an ordered object")
  expect_equal(levels(df_rug$probs), scales::percent_format(accuracy = 1)(probs))

})
