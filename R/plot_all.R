#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @param predictions
#' @return
#' @author Nick Golding
#' @export
plot_all <- function(data, predictions) {
  plot_data(data) +
    plot_reason_for_test(predictions) +
    plot_ascertainment(predictions) +
    plot_layout(
      guides = "collect"
    ) &
    theme(
      legend.position = "bottom"
    )
}
