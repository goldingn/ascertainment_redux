#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
# calculate the ascertainment probability for a set of scalar dates
predict_ascertainment_once <- function(
  close_contact_test_prob,
  symptomatic_test_prob,
  screening_test_prob,
  symptomatic_fraction,
  close_contact_fraction
) {
  
  # the probability of being detected based on symptoms alone, screening alone,
  # close contact notification alone
  p_detected_symptoms <- symptomatic_fraction * symptomatic_test_prob
  p_detected_screening <- screening_test_prob
  p_detected_cc <- close_contact_test_prob
  
  # probability of being a close contact
  p_cc <- close_contact_fraction
  p_ncc <- 1 - p_cc
  
  # for all detected cases, the fraction becoming to close contacts is
  # 'close_contact_fraction', so the fraction being not close contacts is 1
  # minus that
  
  # for close contacts, the probability of nondetection is calculated from the
  # probabilities of each other type of detection, and the probability of
  # detection is the complement of this
  p_undetected_cc <- (1 - p_detected_cc) *
    (1 - p_detected_screening) *
    (1 - p_detected_symptoms)
  
  p_detected_cc <- 1 - p_undetected_cc
  
  # for non-close contacts, detection is solely dure to symptomatic test seeking
  p_detected_ncc <- p_detected_symptoms
  p_undetected_ncc <- 1 - p_detected_ncc
  
  # combine with the probability of being a close contact to get all elements of
  # the matrix
  
  # if source is detected, the offspring are a mixture of close contacts and
  # non-close contacts, each with their own detection probabilities
  detected_detected <- p_cc * p_detected_cc + p_ncc * p_detected_ncc
  detected_undetected <- p_cc * p_undetected_cc + p_ncc * p_undetected_ncc
  
  # if the source was undetected, the offspring are all non-close contacts
  undetected_detected <- p_detected_ncc
  undetected_undetected <- p_undetected_ncc
  
  # build the 2x2 matrix, where the two states are: detected, undetected, in
  # that order
  mat <- matrix(
    c(
      detected_detected,
      detected_undetected,
      undetected_detected,
      undetected_undetected
    ),
    nrow = 2,
    ncol = 2
  )
  
  # analyse the matrix to get the fraction in each bin
  stable_state <- eigen(mat)$vectors[, 1]
  stable_state <- stable_state / sum(stable_state)
  
  # the ascertainment rate is the stable fraction in the first bin (proportion
  # of infections in the detected bin at each iteration)
  ascertainment <- stable_state[1]
  
  # note - this has an alnalytic solution. I'm not calculating it as we will
  # likely switch to multiple states (reasons for testing), and non-stable
  # state solutions
  
  ascertainment
  
}