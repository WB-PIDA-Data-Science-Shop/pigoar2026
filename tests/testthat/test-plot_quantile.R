df_valid <- data.frame(
  income_group = rep(c("Low income", "High income"), each = 20),
  score        = c(runif(20, 0, 0.4), runif(20, 0.6, 1)),
  indicator    = "test_indicator"
)

test_that("plot_quantile returns a ggplot object for valid input", {
  p <- plot_quantile(df_valid, x = "income_group", y = "score", quantile_group = "indicator")
  expect_s3_class(p, "gg")
})

test_that("plot_quantile handles NA values in y without error", {
  df <- df_valid
  df$score[1] <- NA
  expect_no_error(
    plot_quantile(df, x = "income_group", y = "score", quantile_group = "indicator")
  )
})

test_that("plot_quantile errors when a column does not exist", {
  expect_error(
    plot_quantile(df_valid, x = "nonexistent_col", y = "score", quantile_group = "indicator")
  )
  expect_error(
    plot_quantile(df_valid, x = "income_group", y = "nonexistent_col", quantile_group = "indicator")
  )
})

test_that("classify_quantile adds quantile_indicator column with correct levels", {
  result <- classify_quantile(df_valid, var = "score", quantile_group = "indicator")

  expect_true("quantile_indicator" %in% names(result))
  expect_s3_class(result$quantile_indicator, "factor")
  expect_equal(levels(result$quantile_indicator), c("Strong", "Emerging", "Weak"))
})

test_that("classify_quantile assigns all three tiers when data is spread", {
  result <- classify_quantile(df_valid, var = "score", quantile_group = "indicator")
  tiers <- unique(as.character(result$quantile_indicator))

  expect_true(all(c("Weak", "Emerging", "Strong") %in% tiers))
})

test_that("classify_quantile handles NA values in var without error", {
  df <- df_valid
  df$score[1] <- NA

  expect_no_error(classify_quantile(df, var = "score", quantile_group = "indicator"))

  result <- classify_quantile(df, var = "score", quantile_group = "indicator")
  expect_true(is.na(result$quantile_indicator[1]))
})
