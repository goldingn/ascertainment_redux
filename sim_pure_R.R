# simulating from the ascertainment model in pure R code (not yet greta)
set.seed(2)

# simulate fake parameters
data <- sim_parameters()

# calculate ascertainment according to the model
predictions <- calculate_ascertainment_r(data)

# plot
combined_plot <- plot_all(data, predictions)

ggsave(
  "figures/ascertainment_sim_r.png",
  plot = combined_plot,
  bg = "white",
  width = 12,
  height = 5
)
