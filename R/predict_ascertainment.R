#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param close_contact_test_prob
#' @param symptomatic_test_prob
#' @param screening_test_prob
#' @param symptomatic_fraction
#' @param close_contact_fraction
#' @return
#' @author Nick Golding
#' @export
predict_ascertainment <- function(
  contact_fraction,
  contact_test_prob,
  symptomatic_fraction,
  symptomatic_test_prob,
  screenable_fraction,
  screening_test_prob
) {
  
  output <- mapply(
    FUN = predict_ascertainment_once,
    contact_fraction = contact_fraction,
    contact_test_prob = contact_test_prob,
    symptomatic_fraction = symptomatic_fraction,
    symptomatic_test_prob = symptomatic_test_prob,
    screenable_fraction = screenable_fraction,
    screening_test_prob = screening_test_prob
  )
  
  t(output) %>%
    as_tibble()
  
}
