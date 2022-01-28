# simulating from the ascertainment model in pure R code (not yet greta)
set.seed(2)

# simulate fake parameters
data <- sim_parameters()

# calculate ascertainment according to the model
predictions <- calculate_ascertainment_r(data)

combined_plot <- plot_data(data) +
  plot_reason_for_test(predictions) +
  plot_ascertainment(predictions) +
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
