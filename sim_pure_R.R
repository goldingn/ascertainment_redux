# simulating from the ascertainment model in pure R code (not yet greta)
set.seed(2)

# make fake ascertainment data streams
data <- tibble(
  date_num = -30:150,
  date = Sys.Date() + date_num
) %>%
  mutate(
    close_contact_test_prob = random_trend(date_num, 0.95),
    symptomatic_test_prob = random_trend(date_num, 0.8, variance = 1),
    screening_test_prob = random_trend(date_num, 0.1),
    symptomatic_fraction = random_trend(date_num, 0.5),
    close_contact_fraction = random_trend(date_num, 0.9, variance = 2)
  ) %>%
  pivot_longer(
    cols = c(-date, -date_num),
    names_to = "parameter",
    values_to = "value"
  )

# compute ascertainment over these
predictions <- data %>%
  pivot_wider(
    names_from = parameter,
    values_from = value
  ) %>%
  mutate(
    ascertainment = predict_ascertainment(
      close_contact_test_prob = close_contact_test_prob,
      symptomatic_test_prob = symptomatic_test_prob,
      screening_test_prob = screening_test_prob,
      symptomatic_fraction = symptomatic_fraction,
      close_contact_fraction = close_contact_fraction
    ),
    .after = date
  )

# plot simulated datastreams and predictions
data_plot <- data %>%
  ggplot(
    aes(
      x = date,
      y = value,
      colour = parameter
    )
  ) +
  geom_line(
    size = 1.5
  ) +
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  theme_minimal() +
  ggtitle(
    "Simulated data timeseries"
  )


pred_plot <- predictions %>%
  ggplot(
    aes(
      x = date,
      y = ascertainment
    )
  ) +
  geom_line(
    size = 1.5
  ) +
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  theme_minimal() +
  ggtitle(
    "Ascertainment prediction"
  )

ggsave(
  "figures/ascertainment_sim.png",
  plot = data_plot + pred_plot,
  bg = "white",
  width = 10,
  height = 4
)
