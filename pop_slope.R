library(tidyverse)
library(broom)
library(janitor)
library(readxl)
library(ggrepel)

# Load and clean data
pop_slope_raw <- readxl::read_excel(
  "data/pop_slope_raw.xlsx",
  sheet = "Site List"
) |>
  clean_names() |> 
  select(
    state,
    city,
    address,
    pop_2024_1_mi:pop_2024_30_mi
  ) |> 
  select(where(~ any(!is.na(.)))) |> 
  mutate(
    across(
      .cols = pop_2024_1_mi:pop_2024_30_mi,
      .fns = ~ round(.x, 0)
    ),
    label = city  # Use only city as the label for plotting
  )

# Reshape data for slope calculation
pop_slope_long <- pop_slope_raw |>
  pivot_longer(
    cols = pop_2024_1_mi:pop_2024_30_mi,
    names_to = "radius",
    values_to = "population"
  ) |>
  mutate(
    radius = case_when(
      radius == "pop_2024_1_mi" ~ 1,
      radius == "pop_2024_3_mi" ~ 3,
      radius == "pop_2024_5_mi" ~ 5,
      radius == "pop_2024_15_mi" ~ 15,
      radius == "pop_2024_30_mi" ~ 30
    )
  )

# Calculate slopes for each store using linear regression
slopes <- pop_slope_long |>
  group_by(state, city, address, label) |>
  nest() |>
  mutate(
    model = map(data, ~ lm(population ~ radius, data = .)),
    tidied = map(model, tidy)
  ) |>
  unnest(tidied) |>
  filter(term == "radius") |>
  select(state, city, address, label, slope = estimate)

# Classify areas based on updated slope thresholds
slopes <- slopes |>
  mutate(
    area_type = case_when(
      slope > 80000 ~ "Urban",
      slope > 15000 & slope <= 80000 ~ "Suburban",
      slope <= 15000 ~ "Rural"
    )
  )

# Summarize area type counts
area_summary <- slopes |>
  count(area_type) |>
  mutate(percentage = n / sum(n) * 100)

# Print summary
message("Summary of area types:")
print(area_summary)

# Visualize slopes with ggplot2
slope_plot <- ggplot(slopes, aes(x = slope, fill = area_type)) +
  geom_histogram(binwidth = 10000, color = "black") +
  scale_fill_manual(values = c("Urban" = "red", "Suburban" = "blue", "Rural" = "green")) +
  labs(
    title = "Distribution of Population Slopes by Area Type",
    x = "Population Slope (people per mile radius)",
    y = "Number of Stores",
    fill = "Area Type"
  ) +
  theme_minimal()

# Save the plot (fixed syntax)
ggsave("slope_distribution.png", plot = slope_plot, width = 8, height = 6)

# First plot: Visualize population curves, faceted by area_type only
sample_data <- pop_slope_long |>
  inner_join(slopes, by = c("state", "city", "address", "label")) |>
  filter(label %in% head(pop_slope_raw$label, 35)) # Top 35 stores for visualization

curve_plot <- ggplot(sample_data, aes(x = radius, y = population, color = label)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  # Add city labels at the end of each line (radius = 30)
  geom_text_repel(
    data = sample_data %>% filter(radius == 30),
    aes(label = label),
    size = 3,
    nudge_x = 2,
    direction = "y",
    segment.size = 0.2,
    show.legend = FALSE
  ) +
  facet_wrap(~area_type, scales = "free_y") +
  labs(
    title = "Population vs. Radius by Area Type",
    x = "Radius (miles)",
    y = "Population"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

curve_plot # View plot

# Save the first curve plot
ggsave("population_curves.png", plot = curve_plot, width = 10, height = 6)

# Second plot: Visualize population curves, faceted by area_type and state
curve_plot_state <- ggplot(sample_data, aes(x = radius, y = population, color = label)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  # Add city labels at the end of each line (radius = 30)
  geom_text_repel(
    data = sample_data %>% filter(radius == 30),
    aes(label = label),
    size = 3,
    nudge_x = 2,
    direction = "y",
    segment.size = 0.2,
    show.legend = FALSE
  ) +
  facet_grid(area_type ~ state, scales = "free_y") +
  labs(
    title = "Population vs. Radius by Area Type and State",
    x = "Radius (miles)",
    y = "Population"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 8)
  )

curve_plot_state # View plot

# Save the second curve plot with increased width for more facets
ggsave("population_curves_state.png", plot = curve_plot_state, width = 14, height = 8)

# Save the slopes data
write_csv(slopes, "pop_slope_results.csv")