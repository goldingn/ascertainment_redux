#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param p_detected_contact
#' @param p_detected_symptoms
#' @param p_detected_screening
#' @return
#' @author Nick Golding
#' @export
# calculate expected ascertainment and reason for test fractions from R objects
# or greta arrays for key parameters
calculate_expected_fractions <- function(
  p_detected_contact,
  p_detected_symptoms,
  p_detected_screening,
  method = c("analytical", "numerical")
) {
  
  # select the method to compute the stable state
  method <- match.arg(method)

  fun <- switch(
    method,
    numerical = get_stable_state_numerical,
    analytical = get_stable_state_analytical
  )

  # compute the stable state via the require method  
  stable_state <- fun(
    p_detected_contact = p_detected_contact,
    p_detected_symptoms = p_detected_symptoms,
    p_detected_screening = p_detected_screening
  )

  # pull out the modelled ascertainment rate
  ascertainment <- 1 - stable_state[, 4]
  
  # normalise reason for test components to get fractions of all tests
  reason_test_fraction_raw <- stable_state[, 1:3]
  reason_test_fraction_normalisation <- rowSums(reason_test_fraction_raw)
  reason_test_fraction <- sweep(reason_test_fraction_raw, 1, reason_test_fraction_normalisation, FUN = "/")
  
  # return these two greta arrays as a list
  list(
    ascertainment = ascertainment,
    reason_test_fraction = reason_test_fraction
  )
  
}
