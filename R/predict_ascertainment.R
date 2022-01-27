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
  close_contact_test_prob,
  symptomatic_test_prob,
  screening_test_prob,
  symptomatic_fraction,
  close_contact_fraction
) {
  
  output <- mapply(
    FUN = predict_ascertainment_once,
    close_contact_test_prob = close_contact_test_prob,
    symptomatic_test_prob = symptomatic_test_prob,
    screening_test_prob = screening_test_prob,
    symptomatic_fraction = symptomatic_fraction,
    close_contact_fraction = close_contact_fraction
  )
  
}
