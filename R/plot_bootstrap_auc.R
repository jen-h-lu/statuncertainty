#' Plot Bootstrap AUC Distribution
#'
#' Creates a visualization of the bootstrap distribution of AUC estimates.
#' The plot displays the uncertainty distribution, estimated AUC,
#' and bootstrap confidence interval boundaries.
#'
#' @param bootstrap_result Object returned from \code{bootstrap_auc()}.
#' @param bins Number of histogram bins.
#'
#' @return A ggplot object.
#'
#' @examples
#' fit <- glm(
#'   am ~ mpg + hp,
#'   data = mtcars,
#'   family = binomial
#' )
#'
#' result <- bootstrap_auc(
#'   fit,
#'   mtcars,
#'   "am",
#'   B = 100
#' )
#'
#' plot_bootstrap_auc(result)
#'
#' @export
plot_bootstrap_auc <- function(
    bootstrap_result,
    bins = 30
) {


  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required. Install it first."
    )
  }


  distribution <-
    data.frame(
      auc =
        bootstrap_result$bootstrap_distribution
    )


  ci <-
    bootstrap_result$confidence_interval


  estimate <-
    bootstrap_result$estimate



  ggplot2::ggplot(
    distribution,
    ggplot2::aes(x = auc)
  ) +

    ggplot2::geom_histogram(
      bins = bins
    ) +

    ggplot2::geom_vline(
      xintercept = estimate,
      linewidth = 1
    ) +

    ggplot2::geom_vline(
      xintercept = ci,
      linetype = "dashed"
    ) +

    ggplot2::labs(
      title =
        "Bootstrap Distribution of AUC",

      subtitle =
        paste0(
          "Mean AUC = ",
          round(estimate,3)
        ),

      x =
        "Bootstrap AUC",

      y =
        "Frequency"
    ) +

    ggplot2::theme_minimal()

}
