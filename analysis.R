# a demo analysis in greta

# simulating from the ascertainment model in pure R code (not yet greta)
set.seed(2)

# simulate fake parameters
data <- sim_parameters()

# extract required parameters
params <- data %>%
  pivot_wider(
    names_from = parameter,
    values_from = value
  )

expected_fractions <- calculate_expected_fractions(
  contact_fraction = params$contact_fraction,
  contact_test_prob = params$contact_test_prob,
  symptomatic_fraction = params$symptomatic_fraction,
  symptomatic_test_prob = params$symptomatic_test_prob,
  screenable_fraction = params$screenable_fraction,
  screening_test_prob = params$screening_test_prob
)

ascertainment <- expected_fractions$ascertainment
reason_test_fraction <- expected_fractions$reason_test_fraction

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
