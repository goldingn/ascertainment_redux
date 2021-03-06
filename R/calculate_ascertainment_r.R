#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @return
#' @author Nick Golding
#' @export
# given ascertainment datastreams, compute ascertainment fraction and reason for
# test split
calculate_ascertainment_r <- function(data) {
  
  data %>%
    pivot_wider(
      names_from = parameter,
      values_from = value
    ) %>%
    mutate(
      predict_ascertainment(
        contact_fraction = contact_fraction,
        contact_test_prob = contact_test_prob,
        symptomatic_fraction = symptomatic_fraction,
        symptomatic_test_prob = symptomatic_test_prob,
        screenable_fraction = screenable_fraction,
        screening_test_prob = screening_test_prob
      ),
      .after = date
    ) %>%
    mutate(
      ascertainment = 1 - undetected
    ) %>%
    # filter down to what we want
    select(
      date,
      starts_with("detected"),
      ascertainment
    ) %>%
    # normalise detection fractions
    pivot_longer(
      starts_with("detected_"),
      names_to = "reason_for_test",
      values_to = "fraction",
      names_prefix = "detected_"
    ) %>%
    group_by(
      date
    ) %>%
    mutate(
      fraction = fraction / sum(fraction)
    ) %>%
    pivot_wider(
      names_from = reason_for_test,
      values_from = fraction,
      names_prefix = "fraction_tested_"
    )
  
}