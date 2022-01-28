#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @return
#' @author Nick Golding
#' @export
plot_data <- function(data) {
  
  # plot simulated datastreams and predictions
  data %>%
    ggplot(
      aes(
        x = date,
        y = value,
        colour = parameter
      )
    ) +
    geom_line(
      size = 1.5
    ) +
    coord_cartesian(
      ylim = c(0, 1)
    ) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    ggtitle(
      "Simulated data timeseries"
    )
}

