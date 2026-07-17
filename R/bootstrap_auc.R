#' Bootstrap AUC Confidence Interval
#'
#' Estimates the uncertainty of a model's area under the ROC curve (AUC)
#' using bootstrap resampling.
#'
#' @param model A fitted binary classification model.
#' @param data The dataset used to fit the model.
#' @param outcome Character string specifying the binary outcome variable.
#' @param B Number of bootstrap repetitions.
#'
#' @return A list containing the bootstrap AUC estimate,
#' confidence interval, and bootstrap distribution.
#'
#' @examples
#' # Example:
#' # fit <- glm(y ~ x1 + x2, data = df, family = binomial)
#' # bootstrap_auc(fit, df, "y")
#'
#' @export
bootstrap_auc <- function(model, data, outcome, B = 1000) {

  # Store bootstrap estimates
  auc_values <- numeric(B)

  # Bootstrap loop
  for (i in seq_len(B)) {

    # Sample rows with replacement
    boot_data <- data[sample(
      seq_len(nrow(data)),
      replace = TRUE
    ), ]

    # Refit model
    boot_model <- update(
      model,
      data = boot_data
    )

    # Predicted probabilities
    predictions <- predict(
      boot_model,
      newdata = boot_data,
      type = "response"
    )

    # Calculate AUC
    roc_obj <- pROC::roc(
      boot_data[[outcome]],
      predictions,
      quiet = TRUE
    )

    auc_values[i] <- as.numeric(
      pROC::auc(roc_obj)
    )
  }

  # Return results
  list(
    estimate = mean(auc_values),
    confidence_interval = quantile(
      auc_values,
      probs = c(0.025, 0.975)
    ),
    bootstrap_distribution = auc_values
  )
}
