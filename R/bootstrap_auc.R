#' Bootstrap AUC Confidence Intervals
#'
#' Estimates uncertainty in the area under the ROC curve (AUC)
#' using bootstrap resampling.
#'
#' This function repeatedly resamples the dataset, refits the model,
#' and estimates the sampling distribution of AUC. Confidence intervals
#' can be calculated using different bootstrap approaches.
#'
#' @param model A fitted binary classification model.
#' @param data Dataset used to fit the model.
#' @param outcome Character string specifying the binary outcome variable.
#' @param B Number of bootstrap repetitions.
#' @param ci_method Confidence interval method. Options include
#' "percentile", "basic", or "normal".
#'
#' @return A list containing:
#' \describe{
#' \item{estimate}{Mean bootstrap AUC estimate}
#' \item{confidence_interval}{Bootstrap confidence interval}
#' \item{bootstrap_distribution}{Vector of bootstrap AUC values}
#' }
#'
#' @details
#' The bootstrap approximates the sampling distribution of AUC by
#' repeatedly sampling from the empirical distribution of observations.
#'
#' @examples
#' fit <- glm(am ~ mpg + hp,
#'            data = mtcars,
#'            family = binomial)
#'
#' bootstrap_auc(
#'   fit,
#'   mtcars,
#'   "am",
#'   B = 100
#' )
#'
#' @export
bootstrap_auc <- function(
    model,
    data,
    outcome,
    B = 1000,
    ci_method = "percentile"
) {

  if (!ci_method %in% c("percentile", "basic", "normal")) {
    stop(
      "ci_method must be 'percentile', 'basic', or 'normal'"
    )
  }


  auc_values <- numeric(B)


  for (i in seq_len(B)) {

    boot_data <- data[
      sample(
        seq_len(nrow(data)),
        replace = TRUE
      ),
    ]


    boot_model <- update(
      model,
      data = boot_data
    )


    predictions <- predict(
      boot_model,
      newdata = boot_data,
      type = "response"
    )


    roc_object <- pROC::roc(
      boot_data[[outcome]],
      predictions,
      quiet = TRUE
    )


    auc_values[i] <- as.numeric(
      pROC::auc(roc_object)
    )

  }


  estimate <- mean(auc_values)


  confidence_interval <- switch(
    ci_method,

    percentile =
      quantile(
        auc_values,
        probs = c(0.025, 0.975)
      ),

    basic =
      2 * estimate -
      rev(
        quantile(
          auc_values,
          probs = c(0.025, 0.975)
        )
      ),

    normal =
      estimate +
      c(-1, 1) *
      1.96 *
      sd(auc_values)

  )


  list(
    estimate = estimate,

    confidence_interval =
      confidence_interval,

    bootstrap_distribution =
      auc_values,

    ci_method =
      ci_method
  )

}
