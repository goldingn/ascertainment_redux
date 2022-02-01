#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @param sample_size
#' @param days_between_surveys
#' @return
#' @author Nick Golding
#' @export
# simulate fake reason_for_test fractions over time (every day)

sim_reason_for_test_survey <- function(data, 
                                       sample_size = 100, 
                                       days_between_surveys = 7) {
  
  data %>%
    # keep data every 'days_between_surveys' days
    filter(
      sim_days %% days_between_surveys == 1
    ) %>%
    calculate_ascertainment_abm_data() %>%
    pivot_longer(
      cols = starts_with("fraction_tested"),
      names_to = "reason_for_test",
      values_to = "fraction",
      names_prefix = "fraction_tested_"
    ) %>%
    group_by(
      sim_days
    ) %>%
    mutate(
      count = rmultinom(
        n = 1,
        size = sample_size,
        prob = fraction
      )[, 1]
    ) %>%
    ungroup() %>%
    select(
      date_num,
      reason_for_test,
      count
    ) %>%
    pivot_wider(
      names_from = reason_for_test,
      values_from = count,
      names_prefix = "count_tested_"
    ) 
  
}
