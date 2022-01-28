test_that("analytical and numerical stable state solutions agree with each other", {
  
  n <- 100
  p_detected_contact <- as_data(runif(n))
  p_detected_symptoms <- as_data(runif(n))
  p_detected_screening <- as_data(runif(n))
  
  analytical <- get_stable_state_analytical(
    p_detected_contact = p_detected_contact,
    p_detected_symptoms = p_detected_symptoms,
    p_detected_screening = p_detected_screening
  )
  
  numerical <- get_stable_state_numerical(
    p_detected_contact = p_detected_contact,
    p_detected_symptoms = p_detected_symptoms,
    p_detected_screening = p_detected_screening
  )
  
  values <- calculate(
    analytical,
    numerical
  )
  
  testthat::expect_equal(values$analytical, values$numerical, tolerance = 1e-5)
  
})
