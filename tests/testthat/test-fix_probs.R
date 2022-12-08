test_that("fix_probs() works as intended", {

  # Check defaults
  expect_equal(fix_probs(c(.99, .95, .80, .50)), c(.99, .95, .80, .50))

  # Reorders probabilities correctly
  expect_equal(fix_probs(c(.80, .50, .99, .95)), c(.99, .95, .80, .50))

  # Works with vectors of length 1
  expect_equal(fix_probs(.50), .5)

  # Issues error if any probabilites are outside (0, 1)
  expect_error(fix_probs(c(1.1, .80, .5)), regexp = "must be between")
  expect_error(fix_probs(c(.80, .5, -1)), regexp = "must be between")
  expect_error(fix_probs(c(1)), regexp = "must be between")
  expect_error(fix_probs(c(0)), regexp = "must be between")

})
