# a demo analysis in greta

# simulating from the ascertainment model in pure R code (not yet greta)
set.seed(2)

# simulate fake dataset timeseries (including those we wouldn't observe)
data <- sim_parameters()

# get 'true' ascertainment
truth <- calculate_ascertainment_r(data)

# simulate reason_for_test counts over time, with these parameters
reason_test_count_data <- sim_reason_for_test_data(
  data = data,
  sample_size = 200,
  days_between_surveys = 7
)

# extract required parameters (true and biased) for the full timeseries
params_all <- data %>%
  pivot_wider(
    names_from = parameter,
    values_from = value
  ) %>%
  # compute detection probabilities via each pathway from these parameters
  mutate(
    # need to be a known contact and seek a test
    p_detected_contact = contact_fraction * contact_test_prob,
    # need to be symptomatic and seek a test
    p_detected_symptoms = symptomatic_fraction * symptomatic_test_prob,
    # need to be in the fraction of people screened, and test positive is screened whilst positive
    p_detected_screening = screenable_fraction * screening_test_prob
  ) %>%
  mutate(
    # make some of these parameters biased, to re-estimate them in the model
    p_detected_symptoms_biased = plogis(qlogis(p_detected_symptoms) - 1),
    p_detected_screening_biased = plogis(qlogis(p_detected_screening) - 1)
  ) %>%
  select(
    date,
    starts_with("p_")
  )

# get parameters for this subset of data (throw out dates for which we don't
# have reason for test data, and parameter time series we wouldn't observe)
params_data <- params_all %>%
  filter(
    date %in% reason_test_count_data$date
  ) %>%
  select(
    -p_detected_contact,
    -p_detected_symptoms,
    # -p_detected_screening
  )

# define the contact fraction as an unknown variable - logit model on date

# define a transformation on dates to help with starting values and priors
start_date <- min(params_data$date)
date_num <- as.numeric(params_data$date - start_date)
date_scaling <- max(date_num)
date_num <- date_num / date_scaling

# Gaussian process hyperparameters - reuse these for all GPs
kernel_sd <- normal(0, 1, truncation = c(0, Inf))
kernel_variance <- kernel_sd ^ 2
kernel_lengthscale <- lognormal(0, 1)
kernel <- bias(1) + mat52(lengthscales = kernel_lengthscale, variance = kernel_variance) 

# define logit-gp over date for probability of detection via contact
logit_p_detected_contact <- gp(date_num, kernel = kernel)
p_detected_contact <- ilogit(logit_p_detected_contact)

# define a logit-gp over date forprobability of detection via symptomatic
# testing, using a biased estimate as the prior mean on the logit scale

# the temporally-varying bias and error term
logit_p_detected_symptoms_error <- gp(date_num, kernel = kernel)
# the prior mean (transform estimate to logit scale)
logit_p_detected_symptoms_prior <- qlogis(params_data$p_detected_symptoms_biased)
# combine these to get the logit estimate, and transform to probability scale
logit_p_detected_symptoms <- logit_p_detected_symptoms_prior + logit_p_detected_symptoms_error
p_detected_symptoms <- ilogit(logit_p_detected_symptoms)


expected_fractions <- calculate_expected_fractions(
  p_detected_contact = p_detected_contact,
  p_detected_symptoms = p_detected_symptoms,
  p_detected_screening = params_data$p_detected_screening
)

ascertainment <- expected_fractions$ascertainment
reason_test_fraction <- expected_fractions$reason_test_fraction

# pull out the modelled reason for test fractions corresponding to the data
observed_reason_counts <- reason_test_count_data %>%
  select(
    -date
  ) %>%
  as.matrix()

distribution(observed_reason_counts) <- multinomial(
  size = rowSums(observed_reason_counts),
  prob = reason_test_fraction
)


# define and fit model
m <- model(kernel_sd, kernel_lengthscale)
draws <- mcmc(m)


plot(draws)
coda::gelman.diag(
  x = draws,
  autoburnin = FALSE,
  multivariate = FALSE
)

# check convergence of the GP latent parameters too
gp_latent_draws <- calculate(
  logit_p_detected_contact,
  logit_p_detected_symptoms_error,
  values = draws
)
coda::gelman.diag(
  x = gp_latent_draws,
  autoburnin = FALSE,
  multivariate = FALSE
)


# use these to predict contact_fraction, symptomatic_test_prob, and ascertainment over time
date_num_predict <- as.numeric(params_all$date - start_date) / date_scaling

logit_p_detected_contact_predict <- project(logit_p_detected_contact, date_num_predict)
p_detected_contact_predict <- ilogit(logit_p_detected_contact_predict)

logit_p_detected_symptoms_error_predict <- project(logit_p_detected_symptoms_error, date_num_predict)
logit_p_detected_symptoms_predict <- qlogis(params_all$p_detected_symptoms_biased) + logit_p_detected_symptoms_error_predict
p_detected_symptoms_predict <- ilogit(logit_p_detected_symptoms_predict)

expected_fractions_predict <- calculate_expected_fractions(
  p_detected_contact = p_detected_contact_predict,
  p_detected_symptoms = p_detected_symptoms_predict,
  p_detected_screening = params_all$p_detected_screening
)

ascertainment_predict <- expected_fractions_predict$ascertainment

# compute posterior means and CIs of ascertainment and key parameter
predict_sims <- calculate(
  p_detected_contact_predict,
  p_detected_symptoms_predict,
  ascertainment_predict,
  values = draws,
  nsim = 1000
)

p_detected_contact_predict_mean <- colMeans(predict_sims$p_detected_contact_predict)[, 1]
p_detected_contact_predict_cis <- apply(predict_sims$p_detected_contact_predict, 2, quantile, c(0.025, 0.975))

p_detected_symptoms_predict_mean <- colMeans(predict_sims$p_detected_symptoms_predict)[, 1]
p_detected_symptoms_predict_cis <- apply(predict_sims$p_detected_symptoms_predict, 2, quantile, c(0.025, 0.975))

ascertainment_predict_mean <- colMeans(predict_sims$ascertainment_predict)[, 1]
ascertainment_predict_cis <- apply(predict_sims$ascertainment_predict, 2, quantile, c(0.025, 0.975))

predictions <- tibble(
    date = params_all$date,
    ascertainment = ascertainment_predict_mean,
    ascertainment_low = ascertainment_predict_cis[1, ],
    ascertainment_high = ascertainment_predict_cis[2, ],
    p_detected_contact = p_detected_contact_predict_mean,
    p_detected_contact_low = p_detected_contact_predict_cis[1, ],
    p_detected_contact_high = p_detected_contact_predict_cis[2, ],
    p_detected_symptoms = p_detected_symptoms_predict_mean,
    p_detected_symptoms_low = p_detected_symptoms_predict_cis[1, ],
    p_detected_symptoms_high = p_detected_symptoms_predict_cis[2, ],
  )

p_detected_contact_plot <- predictions %>%
  ggplot(
    aes(
      x = date,
      y = p_detected_contact
    )
  ) +
  geom_line(
    data = params_all,
    linetype = 2
  ) +
  geom_ribbon(
    aes(
      ymax = p_detected_contact_high,
      ymin = p_detected_contact_low
    ),
    alpha = 0.3
  ) +
  geom_line() +
  theme_minimal() +
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  ggtitle(
    "p(detected contact) estimate"
  )

p_detected_symptoms_plot <- predictions %>%
  ggplot(
    aes(
      x = date,
      y = p_detected_symptoms
    )
  ) +
  geom_line(
    data = params_all,
    linetype = 2
  ) +
  geom_line(
    aes(
      y = p_detected_symptoms_biased
    ),
    data = params_all,
    col = "red"
  ) +
  geom_ribbon(
    aes(
      ymax = p_detected_symptoms_high,
      ymin = p_detected_symptoms_low
    ),
    alpha = 0.3
  ) +
  geom_line() +
  theme_minimal() +
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  ggtitle(
    "p(detected symptoms) estimate"
  )

ascertainment_plot <- predictions %>%
  ggplot(
    aes(
      x = date,
      y = ascertainment
    )
  ) +
  geom_line(
    data = truth,
    linetype = 2
  ) +
  geom_ribbon(
    aes(
      ymax = ascertainment_high,
      ymin = ascertainment_low
    ),
    alpha = 0.3
  ) +
  geom_line() +
  theme_minimal() +
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  ggtitle(
    "Ascertainment estimate"
  )

fit_plot <- ascertainment_plot + p_detected_contact_plot + p_detected_symptoms_plot

ggsave(
  "figures/estimate_greta.png",
  plot = fit_plot,
  bg = "white",
  width = 14,
  height = 5
)
