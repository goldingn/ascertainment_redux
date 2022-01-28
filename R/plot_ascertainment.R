#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param predictions
#' @return
#' @author Nick Golding
#' @export
plot_ascertainment <- function(predictions) {
  
  predictions %>%
    ggplot(
      aes(
        x = date,
        y = ascertainment
      )
    ) +
    geom_line(
      size = 1.5
    ) +
    coord_cartesian(
      ylim = c(0, 1)
    ) +
    theme_minimal() +
    ggtitle(
      "Ascertainment prediction"
    )
}

