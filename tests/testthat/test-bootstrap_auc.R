test_that("bootstrap_auc returns valid output", {

  set.seed(123)

  data <- data.frame(
    outcome = rbinom(100,1,0.5),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )


  model <- glm(
    outcome ~ x1 + x2,
    data = data,
    family = binomial
  )


  result <- bootstrap_auc(
    model,
    data,
    "outcome",
    B = 100
  )


  expect_true(is.list(result))


  expect_true(
    result$estimate >= 0 &&
      result$estimate <= 1
  )


  expect_length(
    result$confidence_interval,
    2
  )


  expect_length(
    result$bootstrap_distribution,
    100
  )

})


test_that("bootstrap_auc supports multiple confidence intervals", {

  set.seed(123)


  data <- data.frame(
    outcome = rbinom(100,1,0.5),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )


  model <- glm(
    outcome ~ x1 + x2,
    data=data,
    family=binomial
  )


  percentile <- bootstrap_auc(
    model,
    data,
    "outcome",
    B=50,
    ci_method="percentile"
  )


  normal <- bootstrap_auc(
    model,
    data,
    "outcome",
    B=50,
    ci_method="normal"
  )


  expect_equal(
    percentile$ci_method,
    "percentile"
  )


  expect_equal(
    normal$ci_method,
    "normal"
  )


})


test_that("invalid confidence interval methods fail", {

  expect_error(
    bootstrap_auc(
      NULL,
      NULL,
      "outcome",
      ci_method="wrong"
    )
  )

})
