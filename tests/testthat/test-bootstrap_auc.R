test_that("bootstrap_auc returns valid output", {

  # Simulate example binary classification data
  set.seed(123)

  data <- data.frame(
    outcome = rbinom(100, 1, 0.5),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )

  # Fit logistic regression model
  model <- glm(
    outcome ~ x1 + x2,
    data = data,
    family = binomial
  )

  # Run bootstrap AUC
  result <- bootstrap_auc(
    model = model,
    data = data,
    outcome = "outcome",
    B = 100
  )

  # Check returned object
  expect_true(is.list(result))

  # Check AUC estimate is valid
  expect_true(
    result$estimate >= 0 &&
      result$estimate <= 1
  )

  # Check confidence interval exists
  expect_length(
    result$confidence_interval,
    2
  )

  # Check bootstrap distribution size
  expect_length(
    result$bootstrap_distribution,
    100
  )

})
