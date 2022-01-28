#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param predictions
#' @return
#' @author Nick Golding
#' @export
plot_reason_for_test <- function(predictons) {
  
  predictions %>%
    pivot_longer(
      starts_with("fraction_tested_"),
      names_to = "reason for test",
      values_to = "fraction",
      names_prefix = "fraction_tested_"
    ) %>%
    mutate(
      `reason for test` = factor(
        `reason for test`,
        levels = c("contact", "symptoms", "screening")
      )
    ) %>%
    ggplot(
      aes(
        x = date,
        y = fraction,
        fill = `reason for test`
      )
    ) +
    geom_area(
    ) +
    coord_cartesian(
      ylim = c(0, 1)
    ) +
    theme_minimal() +
    scale_fill_brewer(
      palette = "Accent"
    ) +
    theme(
      legend.position = "bottom"
    ) +
    ggtitle(
      "Reason for test"
    )
}