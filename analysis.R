# a demo analysis in greta

# simulating from the ascertainment model in pure R code (not yet greta)
set.seed(2)

# simulate fake parameters
data <- sim_parameters()

# get true ascertainment
truth <- calculate_ascertainment_r(data)

# extract required parameters for full timeseries
params_all <- data %>%
  pivot_wider(
    names_from = parameter,
    values_from = value
  )

# simulate fake reason_for_test counts over time, with these parameters
reason_test_count_data <- sim_reason_for_test_data(data, sample_size = 200, days_between_surveys = 7)

# get parameters for this subset of data
params_data <- params_all %>%
  filter(
    date %in% reason_test_count_data$date
  )

# define the contact fraction as an unknown variable - logit model on date

# define a transformation on dates to help with starting values and priors
start_date <- min(params_data$date)
date_num <- as.numeric(params_data$date - start_date)
date_scaling <- max(date_num)
date_num <- date_num / date_scaling

# Gaussian process hyperparameters
kernel_sd <- normal(0, 1, truncation = c(0, Inf))
kernel_variance <- kernel_sd ^ 2
kernel_lengthscale <- lognormal(0, 1)

# define logit-gp over date
kernel <- bias(1) + mat52(lengthscales = kernel_lengthscale, variance = kernel_variance) 
logit_contact_fraction <- gp(date_num, kernel = kernel)
contact_fraction <- ilogit(logit_contact_fraction)

expected_fractions <- calculate_expected_fractions(
  contact_fraction = contact_fraction,
  contact_test_prob = params_data$contact_test_prob,
  symptomatic_fraction = params_data$symptomatic_fraction,
  symptomatic_test_prob = params_data$symptomatic_test_prob,
  screenable_fraction = params_data$screenable_fraction,
  screening_test_prob = params_data$screening_test_prob
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

# check convergence of the GP latent parameters too
plot(calculate(logit_contact_fraction, values = draws))


# use these to predict contact_fraction and ascertainment over time
date_num_predict <- as.numeric(params_all$date - start_date) / date_scaling
logit_contact_fraction_predict <- project(logit_contact_fraction, date_num_predict)
contact_fraction_predict <- ilogit(logit_contact_fraction_predict)

expected_fractions_predict <- calculate_expected_fractions(
  contact_fraction = contact_fraction_predict,
  contact_test_prob = params_all$contact_test_prob,
  symptomatic_fraction = params_all$symptomatic_fraction,
  symptomatic_test_prob = params_all$symptomatic_test_prob,
  screenable_fraction = params_all$screenable_fraction,
  screening_test_prob = params_all$screening_test_prob
)

ascertainment_predict <- expected_fractions_predict$ascertainment

# compute posterior means and CIs of ascertainment and key parameter
predict_sims <- calculate(
  contact_fraction_predict,
  ascertainment_predict,
  values = draws,
  nsim = 1000
)

contact_fraction_predict_mean <- colMeans(predict_sims$contact_fraction_predict)[, 1]
contact_fraction_predict_cis <- apply(predict_sims$contact_fraction_predict, 2, quantile, c(0.025, 0.975))

ascertainment_predict_mean <- colMeans(predict_sims$ascertainment_predict)[, 1]
ascertainment_predict_cis <- apply(predict_sims$ascertainment_predict, 2, quantile, c(0.025, 0.975))

predictions <- tibble(
    date = params_all$date,
    ascertainment = ascertainment_predict_mean,
    ascertainment_low = ascertainment_predict_cis[1, ],
    ascertainment_high = ascertainment_predict_cis[2, ],
    contact_fraction = contact_fraction_predict_mean,
    contact_fraction_low = contact_fraction_predict_cis[1, ],
    contact_fraction_high = contact_fraction_predict_cis[2, ]
  )

contact_fraction_plot <- predictions %>%
  ggplot(
    aes(
      x = date,
      y = contact_fraction
    )
  ) +
  geom_line(
    data = params_all,
    linetype = 2
  ) +
  geom_ribbon(
    aes(
      ymax = contact_fraction_high,
      ymin = contact_fraction_low
    ),
    alpha = 0.3
  ) +
  geom_line() +
  theme_minimal() +
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  ggtitle(
    "contact_fraction estimate"
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

fit_plot <- ascertainment_plot +  contact_fraction_plot

ggsave(
  "figures/estimate_greta.png",
  plot = fit_plot,
  bg = "white",
  width = 10,
  height = 5
)
