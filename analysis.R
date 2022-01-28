# a demo analysis in greta

# simulating from the ascertainment model in pure R code (not yet greta)
set.seed(2)

# simulate fake parameters
data <- sim_parameters()

# set up key parameters
params <- data %>%
  pivot_wider(
    names_from = parameter,
    values_from = value
  ) %>%
  mutate(
    # need to be symptomatic and seek a test
    p_detected_symptoms = symptomatic_fraction * symptomatic_test_prob,
    # need to be in the fraction of people screened, and test positive is screened whilst positive
    p_detected_screening = screenable_fraction * screening_test_prob,
    # need to be a known contact and seek a test
    p_detected_contact = contact_fraction * contact_test_prob
  )

p_detected_contact <- params$p_detected_contact
p_detected_symptoms <- params$p_detected_symptoms
p_detected_screening <- params$p_detected_screening

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
  # detected with reason for test being contact
  rep(0, nrow(params)),
  # detected with reason for test being symptoms
  p_detected_symptoms,
  # detected with reason for test being screening
  p_detected_not_symptoms * p_detected_screening,
  # not detected
  p_detected_not_symptoms * p_detected_not_screening
)


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

# get output from greta model to confirm it matches

estimates <- calculate(ascertainment, reason_test_fraction)

predictions <- estimates$reason_test_fraction %>%
  as_tibble() %>%
  setNames(
    paste0("fraction_tested_", c("contact", "symptoms", "screening"))
  ) %>%
  mutate(
    date = params$date,
    ascertainment = estimates$ascertainment[, 1],
    .before = everything()
  )

combined_plot <- plot_all(data, predictions)

ggsave(
  "figures/ascertainment_sim_greta.png",
  plot = combined_plot,
  bg = "white",
  width = 12,
  height = 5
)
