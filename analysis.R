# a demo analysis in greta

# simulating from the ascertainment model in pure R code (not yet greta)
set.seed(2)

# simulate fake parameters
data <- sim_parameters()

# simulate fake reason_for_test counts over time, with these parameters
reason_test_count_data <- sim_reason_for_test_data(data, sample_size = 200, days_between_surveys = 7)

# extract required parameters
params <- data %>%
  pivot_wider(
    names_from = parameter,
    values_from = value
  )

# define the contact fraction as an unknown variable - logit-linear model on date
date_num <- as.numeric(params$date - min(params$date))
contact_fraction_intercept <- normal(0, 1)
contact_fraction_slope <- normal(0, 1)
logit_contact_fraction <- contact_fraction_intercept + contact_fraction_slope * date_num
contact_fraction <- ilogit(logit_contact_fraction)


expected_fractions <- calculate_expected_fractions(
  contact_fraction = contact_fraction,
  contact_test_prob = params$contact_test_prob,
  symptomatic_fraction = params$symptomatic_fraction,
  symptomatic_test_prob = params$symptomatic_test_prob,
  screenable_fraction = params$screenable_fraction,
  screening_test_prob = params$screening_test_prob
)

ascertainment <- expected_fractions$ascertainment
reason_test_fraction <- expected_fractions$reason_test_fraction

# pull out the modelled reason for test fractions corresponding to the data
idx <- match(reason_test_count_data$date, params$date)
observed_reason_counts <- reason_test_count_data %>%
  select(
    -date
  ) %>%
  as_data()

distribution(observed_reason_counts) <- multinomial(
  size = rowSums(observed_reason_counts),
  prob = reason_test_fraction[idx, ]
)


# define and fit model
m <- model(contact_fraction_intercept, contact_fraction_slope)
draws <- mcmc(m)


plot(draws)


# compute posterior means and CIs of ascertainment and key parameter

# use only data dates when fitting, then predict post-hoc



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
