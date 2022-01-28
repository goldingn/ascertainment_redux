#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export

sim_parameters <- function() {
  # make fake ascertainment data streams
  tibble(
    date_num = -30:150,
    date = Sys.Date() + date_num
  ) %>%
    mutate(
      contact_fraction = random_trend(date_num, 0.3, variance = 2),
      contact_test_prob = random_trend(date_num, 0.9),
      symptomatic_fraction = random_trend(date_num, 0.5),
      symptomatic_test_prob = random_trend(date_num, 0.8, variance = 1),
      screenable_fraction = random_trend(date_num, 0.1),
      # tested 5 random days per week, 3 days of RAT positivity
      screening_test_prob = random_trend(date_num, 1 - ((1 - 5/7) ^ 34))
    ) %>%
    pivot_longer(
      cols = c(-date, -date_num),
      names_to = "parameter",
      values_to = "value"
    ) %>%
    select(
      -date_num
    )
  
}
