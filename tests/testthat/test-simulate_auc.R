test_that("simulation returns expected outputs", {

  set.seed(123)

  result <- simulate_auc_performance(
    n = 50,
    simulations = 5,
    B = 50
  )


  expect_true(
    is.data.frame(result)
  )


  expect_true(
    "coverage" %in% names(result)
  )


  expect_true(
    "average_width" %in% names(result)
  )


  expect_true(
    "bias" %in% names(result)
  )


})
