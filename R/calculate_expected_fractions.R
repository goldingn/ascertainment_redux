#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param contact_fraction
#' @param contact_test_prob
#' @param symptomatic_fraction
#' @param symptomatic_test_prob
#' @param screenable_fraction
#' @param screening_test_prob
#' @return
#' @author Nick Golding
#' @export
# calculate expected ascertainment and reason for test fractions from R objects
# or greta arrays for key parameters
calculate_expected_fractions <- function(
  contact_fraction,
  contact_test_prob,
  symptomatic_fraction,
  symptomatic_test_prob,
  screenable_fraction,
  screening_test_prob
) {
  
  # need to be a known contact and seek a test
  p_detected_contact <- contact_fraction * contact_test_prob
  # need to be symptomatic and seek a test
  p_detected_symptoms <- symptomatic_fraction * symptomatic_test_prob
  # need to be in the fraction of people screened, and test positive is screened whilst positive
  p_detected_screening <- screenable_fraction * screening_test_prob
  
  p_detected_not_contact <- 1 - p_detected_contact
  p_detected_not_symptoms <- 1 - p_detected_symptoms
  p_detected_not_screening <- 1 - p_detected_screening
  
  # build array of markov matrices
  
  # transitions from a detected infection to each other state
  from_detected <- cbind(
    # detected with reason for test being contact
    p_detected_contact,
    # detected with reason for test being symptoms
    p_detected_not_contact * p_detected_symptoms,
    # detected with reason for test being screening
    p_detected_not_contact * p_detected_not_symptoms * p_detected_screening,
    # not detected
    p_detected_not_contact * p_detected_not_symptoms * p_detected_not_screening
  )
  
  # from an undetected infection to each other state
  from_undetected <- cbind(
    # detected with reason for test being contact (0)
    zeros(length(p_detected_symptoms)),
    # detected with reason for test being symptoms
    p_detected_symptoms,
    # detected with reason for test being screening
    p_detected_not_symptoms * p_detected_screening,
    # not detected
    p_detected_not_symptoms * p_detected_not_screening
  )
  
  # coerce 'from_detected' to a greta array if the user passed in only R
  # objects. Not needed from 'from_undetected' becaue it gets coerced by the
  # zeros object
  if (!inherits(from_detected, "greta_array")) {
    from_detected <- as_data(from_detected)
  }
  
  matrices <- abind(
    from_detected,
    from_detected,
    from_detected,
    from_undetected,
    along = 3
  )
  
  # solve stable states of Markov processes for each configuration
  solution <- greta.dynamics::iterate_matrix(
    matrices
  )
  stable_state <- solution$stable_distribution
  dim(stable_state) <- dim(stable_state)[1:2]
  
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
