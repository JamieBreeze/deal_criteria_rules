pop_slope_raw |> View()

# Check for missing values
missing_summary <- pop_slope_raw |>
  summarise(across(
    .cols = everything(),
    .fns = ~ sum(is.na(.)),
    .names = "missing_{.col}"
  ))
message("Missing values in pop_slope_raw:")
print(missing_summary)

# Check for duplicates
duplicates_summary <- pop_slope_raw |>
  group_by(state, city, address) |>
  summarise(duplicate_count = n()) |>
  filter(duplicate_count > 1)
message("Duplicate entries in pop_slope_raw:")
print(duplicates_summary)

# Check for unique combinations of state, city, and address
unique_combinations <- pop_slope_raw |>
  distinct(state, city, address) |>
  summarise(unique_count = n())
message("Unique combinations of state, city, and address in pop_slope_raw:")
print(unique_combinations)


# Check for outliers in population columns
pop_slope_raw |>
  select(pop_2024_1_mi:pop_2024_30_mi) |>
  summary() |>
  print()
# Check for negative values in population columns
negative_values <- pop_slope_raw |>
  select(pop_2024_1_mi:pop_2024_30_mi) |>
  summarise(across(everything(), ~ sum(. < 0)))
message("Negative values in population columns:")
print(negative_values)
# Check for zero values in population columns
zero_values <- pop_slope_raw |>
  select(pop_2024_1_mi:pop_2024_30_mi) |>
  summarise(across(everything(), ~ sum(. == 0)))
message("Zero values in population columns:")
print(zero_values)

# Check for NA values in population columns
na_values <- pop_slope_raw |>
  select(pop_2024_1_mi:pop_2024_30_mi) |>
  summarise(across(everything(), ~ sum(is.na(.))))
message("NA values in population columns:")
print(na_values)

# Check for extreme values in population columns
extreme_values <- pop_slope_raw |>
  select(pop_2024_1_mi:pop_2024_30_mi) |>
  summarise(across(everything(), ~ sum(. > 1000000)))
message("Extreme values in population columns:")
print(extreme_values)

# Check for duplicates in the entire dataset
duplicates_summary <- pop_slope_raw |>
  group_by(state, city, address) |>
  summarise(duplicate_count = n()) |>
  filter(duplicate_count > 1)
message("Duplicate entries in pop_slope_raw:")
print(duplicates_summary)
