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
get_stable_state_numerical <- function(
  p_detected_contact,
  p_detected_symptoms,
  p_detected_screening
) {
  
  # build array of markov matrices
  
  # pre-calculate complements
  p_detected_not_contact <- 1 - p_detected_contact
  p_detected_not_symptoms <- 1 - p_detected_symptoms
  p_detected_not_screening <- 1 - p_detected_screening
  
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
  solution <- greta.dynamics::iterate_matrix(matrices)
  stable_state <- solution$stable_distribution
  dim(stable_state) <- dim(stable_state)[1:2]
  
  stable_state
  
}