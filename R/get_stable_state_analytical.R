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
get_stable_state_analytical <- function(
  p_detected_contact,
  p_detected_symptoms,
  p_detected_screening
) {
  
  # use analytical solution to the stable state of the matrix described in
  # get_stable_state_numerical(), normalise to get fractions and return
  a <- p_detected_contact
  b <- p_detected_symptoms
  c <- p_detected_screening
  
  # matrix:
  # a,                 a,                 a,                 0,
  # (1-a)*b,           (1-a)*b,           (1-a)*b,           b,
  # (1-a)*(1-b)*c,     (1-a)*(1-b)*c,     (1-a)*(1-b)*c,     (1-b)*c,
  # (1-a)*(1-b)*(1-c), (1-a)*(1-b)*(1-c), (1-a)*(1-b)*(1-c), (1-b)*(1-c)
  
  # this is a Markov matrix (all parameters are probabilities and columns sum to
  # 1), so the dominant eigenvalue is 1. The corresponding eigenvector, as
  # calculated by Tas Symons, is:
  
  p1 <- a * (b * c - b - c) / ((a - 1) * (b - 1) * (c - 1))
  p2 <- b / ((b - 1) * (c - 1))
  p3 <- -c / (c - 1)
  p4 <- ones(length(a))
  
  stable_state_raw <- cbind(
    p1,
    p2,
    p3,
    p4
  )
  
  # normalise stable state
  stable_state_normalisation <- rowSums(stable_state_raw)
  stable_state <- sweep(stable_state_raw, 1, stable_state_normalisation, FUN = "/")
  
  stable_state
  
}