library(tidyverse)
library(broom)
library(janitor)

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
  mutate(across(
    .cols = pop_2024_1_mi:pop_2024_30_mi,
    .fns = ~ round(.x, 0)
  ))

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
  group_by(state, city, address) |>
  nest() |>
  mutate(
    model = map(data, ~ lm(population ~ radius, data = .)),
    tidied = map(model, tidy)
  ) |>
  unnest(tidied) |>
  filter(term == "radius") |>
  select(state, city, address, slope = estimate)

# Classify areas based on slope thresholds
# These thresholds are illustrative; adjust based on domain knowledge
slopes <- slopes |>
  mutate(
    area_type = case_when(
      slope > 100000 ~ "Urban",
      slope > 10000 & slope <= 100000 ~ "Suburban",
      slope <= 10000 ~ "Rural"
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

# Save the plot
ggsave("slope_distribution.png", plot = slope_plot, width = 8, height = 6)

# Visualize population curves for a sample of stores
sample_data <- pop_slope_long |>
  inner_join(slopes, by = c("state", "city", "address")) |>
  filter(address %in% head(pop_slope_raw$address, 25)) # Top 5 stores for visualization

curve_plot <- ggplot(sample_data, aes(x = radius, y = population, color = address)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(~area_type, scales = "free_y") +
  labs(
    title = "Population vs. Radius by Area Type",
    x = "Radius (miles)",
    y = "Population",
    color = "Address"
  ) +
  theme_minimal()

# Print the curve plot
curve_plot


# Save the curve plot
ggsave("population_curves.png", plot = curve_plot, width = 10, height = 6)

# Save the slopes data
write_csv(slopes, "pop_slope_results.csv")
