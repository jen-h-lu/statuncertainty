test_that("plot_bootstrap_auc returns a ggplot object", {


  model <- glm(
    am ~ mpg + hp,
    data = mtcars,
    family = binomial
  )


  result <- bootstrap_auc(
    model,
    mtcars,
    "am",
    B = 20
  )


  plot <- plot_bootstrap_auc(result)


  expect_s3_class(
    plot,
    "ggplot"
  )


})
