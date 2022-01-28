# simulating from the ascertainment model in pure R code (not yet greta)
set.seed(2)

# simulate fake parameters
data <- sim_parameters()

# calculate ascertainment according to the model
predictions <- calculate_ascertainment_r(data)


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
  pivot_longer(
    starts_with("fraction_tested_"),
    names_to = "reason for test",
    values_to = "fraction",
    names_prefix = "fraction_tested_"
  ) %>%
  mutate(
    `reason for test` = factor(
      `reason for test`,
      levels = c("contact", "symptoms", "screening")
    )
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
