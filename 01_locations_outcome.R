library(tidyverse)
library(janitor)

# Read in all Locations tables ----
# Define data directory
data_dir <- "data"

# Read and clean locations_parent
locations_parent <- read_csv(file.path(data_dir, "locations_parent_2025-04-16.csv")) |>
  clean_names(case = "snake") |>
  mutate(
    location_id = as.character(location_id),
    across(where(is.character), ~ na_if(.x, "")),
    across(where(is.numeric), ~ replace(.x, is.nan(.x) | is.infinite(.x), NA))
  )

# Read and clean locations_details
locations_details <- read_csv(file.path(data_dir, "locations_detail_2025-04-16.csv")) |>
  clean_names(case = "snake") |>
  mutate(
    location_id = as.character(location_id),
    across(where(is.character), ~ na_if(.x, "")),
    across(where(is.numeric), ~ replace(.x, is.nan(.x) | is.infinite(.x), NA)),
    across(matches("date$"), as.Date, format = "%Y-%m-%d")
  ) |> 
  rename(acq_stage = acq_stage_gs_lkup)

# Read and clean locations_batch
locations_batch <- read_csv(file.path(data_dir, "locations_batch_sitewise_data_2025-04-17.csv")) |>
  clean_names(case = "snake")

# Left join to create locations tibble
locations_processed <- locations_parent |>
  left_join(locations_batch, 
            by = c("location_city_name" = "location_city_name_input_data", "street_number" = "street_number_input_data"),
            multiple = "first") |> 
  left_join(locations_details, by = "location_id") |>
  # Calculate total_vpd as sum of columns ending in cpd or wpd
  mutate(
    total_vpd = rowSums(across(matches("cpd$|wpd$"), .fns = ~ replace(.x, is.na(.x), 0)), na.rm = TRUE)
  ) |>
  # Relocate total_vpd after breeze_brand
  relocate(total_vpd, .after = breeze_brand) |>
  # Filter for total_vpd > 0
  filter(total_vpd > 0) |>
  # Create outcome column based on specified logic
  mutate(
    outcome = case_when(
      !is.na(passed_active_status) ~ "pass",
      acq_stage %in% c("19 Post Close", "20 Closed", "21 Closed by Brick") ~ "acquire",
      TRUE ~ NA_character_
    )
  ) |> 
  relocate(outcome, .after = location_id) |> 
  mutate(outcome = as_factor(outcome)) 
  

# Inspect the result
glimpse(locations_processed)

# Optionally save the result
# write_csv(locations_processed, file.path(data_dir, "locations_processed_2025-04-16.csv"))

# Select relevant columns for analysis ----
df_locs_raw <- locations_processed |>
  select(
    outcome,
    location_id,
    location_city_name,
    breeze_brand,
    total_vpd,
    lube_bays,
    repair_bays,
    lof_cpd,
    sscw_wpd,
    ecw_wpd,
    fcw_wpd,
    ibacw_wpd,
    crw_tnl_wpd = crw_tnl_wpd_gs_lkup,
    lat_sj,
    latitude_input_data_x,
    long_sj,
    longitude_input_data_x,
    number_miles_to_closest_oc_loc_sj,
    brand_sub_format,
    opportunity_name = opportunity_name.x,
    opportunity_id,
    traffic,
    sitewise_batch,
    acq_stage,
    co_located_w_lube,
    co_located_w_car_wash,
    co_located_w_repair,
    nearest_streetlight_day_part_aadt_5_mi,
    x2024_estimate_5_mi,
    x2029_projection_5_mi,
    state_abb,
    city,
    street_number,
    city_state,
    zcta_code
  ) |> 
  filter(!is.na(outcome))

# Optionally save the result
write_csv(df_locs_raw, file.path(data_dir, "df_locs_raw.csv"))

df_locs_raw |> count(outcome)
