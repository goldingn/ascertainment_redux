#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return ' @author Nick Golding ' @export calculate the ascertainment
#probability and reasons for test for detected cases for a set of scalar dates
predict_ascertainment_once <- function(
  contact_fraction,
  contact_test_prob,
  symptomatic_fraction,
  symptomatic_test_prob,
  screenable_fraction,
  screening_test_prob
) {
  
  # the probabilities of being detected based on having a known contact, having
  # symptoms (if no contact), or screening (if no contact or symptoms),

  # need to be symptomatic and seek a test
  p_detected_symptoms <- symptomatic_fraction * symptomatic_test_prob
  
  # need to be in the fraction of people screened, and test positive is screened whilst positive
  p_detected_screening <- screenable_fraction * screening_test_prob
  
  # need to be a known contact and seek a test
  p_detected_contact <- contact_fraction * contact_test_prob

  # build a 4x4 matrix of transition between sequential pairs of infections:
  # detected_contact, detected_symptoms, detected_screening, undetected.
  
  # assume hierarchy of detection, such that if person has a known contact, they
  # put that as the reason, if not tand they have symptoms, that's the reason,
  # and otherwise, it must be screening
  
  # transitions from a detected infection to each other state
  from_detected <- c(
    # detected with reason for test being contact
    p_detected_contact,
    # detected with reason for test being symptoms
    (1 - p_detected_contact) * p_detected_symptoms,
    # detected with reason for test being screening
    (1 - p_detected_contact) * (1 - p_detected_symptoms) * p_detected_screening,
    # not detected
    (1 - p_detected_contact) * (1 - p_detected_symptoms) * (1 - p_detected_screening)
  )
  
  # from an undetected infection to each other state
  from_undetected <- c(
    # detected with reason for test being contact
    0,
    # detected with reason for test being symptoms
    p_detected_symptoms,
    # detected with reason for test being screening
    (1 - p_detected_symptoms) * p_detected_screening,
    # not detected
    (1 - p_detected_symptoms) * (1 - p_detected_screening)
  )
  
  mat <- cbind(
    from_detected, from_detected, from_detected, from_undetected
  )
  
  # analyse the matrix to get the fraction in each bin
  stable_state <- Re(eigen(mat)$vectors[, 1])
  stable_state <- stable_state / sum(stable_state)
  
  # the ascertainment rate is the stable fraction in the first bin (proportion
  # of infections in the detected bin at each iteration)
  names(stable_state) <- c("detected_contact", "detected_symptoms", "detected_screening", "undetected")
  
  # note - this has an analytic solution. I'm not calculating it as we will
  # likely switch to multiple states (reasons for testing), and non-stable
  # state solutions
  
  stable_state
  
}