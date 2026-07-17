#' Simulate Bootstrap AUC Performance
#'
#' Evaluates bootstrap confidence interval performance through repeated
#' simulation of binary classification datasets.
#'
#' The function estimates confidence interval coverage probability,
#' average interval width, and bias across repeated simulations.
#'
#' @param n Sample size for each simulated dataset.
#' @param simulations Number of simulation repetitions.
#' @param B Number of bootstrap repetitions for each dataset.
#' @param true_auc Approximate target AUC used to generate outcomes.
#'
#' @return A data frame containing:
#' \describe{
#' \item{coverage}{Proportion of intervals containing the true AUC}
#' \item{average_width}{Average confidence interval width}
#' \item{bias}{Average difference between estimated and true AUC}
#' }
#'
#' @examples
#' simulate_auc_performance(
#'   n = 100,
#'   simulations = 10,
#'   B = 100
#' )
#'
#' @export
simulate_auc_performance <- function(
    n = 100,
    simulations = 100,
    B = 1000,
    true_auc = 0.75
) {


  coverage_results <- logical(simulations)

  interval_widths <- numeric(simulations)

  auc_estimates <- numeric(simulations)


  for (i in seq_len(simulations)) {


    x <- rnorm(n)


    probability <- plogis(
      qnorm(true_auc) * x
    )


    y <- rbinom(
      n,
      size = 1,
      prob = probability
    )


    data <- data.frame(
      outcome = y,
      predictor = x
    )


    model <- glm(
      outcome ~ predictor,
      data = data,
      family = binomial
    )


    result <- bootstrap_auc(
      model,
      data,
      "outcome",
      B = B
    )


    ci <- result$confidence_interval


    coverage_results[i] <-
      true_auc >= ci[1] &&
      true_auc <= ci[2]


    interval_widths[i] <-
      diff(ci)


    auc_estimates[i] <-
      result$estimate

  }


  data.frame(

    coverage =
      mean(coverage_results),

    average_width =
      mean(interval_widths),

    bias =
      mean(auc_estimates) - true_auc

  )

}
