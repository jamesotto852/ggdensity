test_that("res_to_df returns correct structure for each value of output", {

  data <- readRDS(test_path("fixtures", "df_norm.rds"))
  probs <- c(.99, .95, .80, .50)

  res <- get_hdr(data, method_kde(), probs)

  # Checking output == "bands"
  df_bands <- res_to_df(res, probs, group = 1, output = "bands")
  expect_type(df_bands, "list")
  expect_equal(colnames(df_bands), c("x", "y", "piece", "group", "subgroup", ".size", "probs"))
  expect(is.ordered(df_bands$probs), "probs is an ordered object")
  expect_equal(levels(df_bands$probs), scales::percent_format(accuracy = 1)(probs))

  # Checking output == "lines"
  df_lines <- res_to_df(res, probs, group = 1, output = "lines")
  expect_type(df_lines, "list")
  expect_equal(colnames(df_lines), c("x", "y", "piece", "group", ".size", "probs"))
  expect(is.ordered(df_lines$probs), "probs is an ordered object")
  expect_equal(levels(df_lines$probs), scales::percent_format(accuracy = 1)(probs))

  # Checking output == "points"
  df_points <- res_to_df(res, probs, group = 1, output = "points")
  expect_type(df_points, "list")
  expect_equal(colnames(df_points), c("x", "y", "probs"))
  expect(is.ordered(df_points$probs), "probs is an ordered object")
  expect_equal(levels(df_points$probs), scales::percent_format(accuracy = 1)(c(1, probs)))

})
