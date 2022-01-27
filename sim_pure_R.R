# simulating from the ascertainment model in pure R code (not yet greta)
set.seed(2)

# make fake ascertainment data streams
data <- tibble(
  date_num = -30:150,
  date = Sys.Date() + date_num
) %>%
  mutate(
    contact_fraction = random_trend(date_num, 0.9, variance = 2),
    contact_test_prob = random_trend(date_num, 0.95),
    symptomatic_fraction = random_trend(date_num, 0.5),
    symptomatic_test_prob = random_trend(date_num, 0.8, variance = 1),
    screenable_fraction = random_trend(date_num, 0.1),
    # tested 5 random days per week, 3 days of RAT positivity
    screening_test_prob = random_trend(date_num, 1 - ((1 - 5/7) ^ 34))
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
    predict_ascertainment(
      contact_fraction = contact_fraction,
      contact_test_prob = contact_test_prob,
      symptomatic_fraction = symptomatic_fraction,
      symptomatic_test_prob = symptomatic_test_prob,
      screenable_fraction = screenable_fraction,
      screening_test_prob = screening_test_prob
    ),
    .after = date
  ) %>%
  mutate(
    ascertainment = 1 - undetected
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
  theme(legend.position = "bottom") +
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

reason_plot <- predictions %>%
  select(
    date,
    starts_with("detected_")
  ) %>%
  pivot_longer(
    starts_with("detected_"),
    names_to = "reason for test",
    values_to = "fraction",
    names_prefix = "detected_"
  ) %>%
  mutate(
    `reason for test` = factor(
      `reason for test`,
      levels = c("contact", "symptoms", "screening")
    )
  ) %>%
  group_by(
    date
  ) %>%
  mutate(
    fraction = fraction / sum(fraction)
  ) %>%
  ggplot(
    aes(
      x = date,
      y = fraction,
      fill = `reason for test`
    )
  ) +
  geom_area(
  ) +
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  theme_minimal() +
  scale_fill_brewer(
    palette = "Accent"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  ggtitle(
    "Reason for test"
  )


combined_plot <- data_plot + reason_plot + pred_plot +
  plot_layout(
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom"
  )

ggsave(
  "figures/ascertainment_sim.png",
  plot = combined_plot,
  bg = "white",
  width = 12,
  height = 5
)
