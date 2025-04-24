library(tidyverse)
library(janitor)

# Read in all Locations tables ----
# Define data directory
data_dir <- "data"

# Read and clean locations_parent
locations_parent <- read_csv(file.path(data_dir, "locations_parent_2025-04-23.csv")) |>
  clean_names(case = "snake") |>
  mutate(
    location_id = as.character(location_id),
    across(where(is.character), ~ na_if(.x, "")),
    across(where(is.numeric), ~ replace(.x, is.nan(.x) | is.infinite(.x), NA))
  )

# Read and clean locations_details
locations_details <- read_csv(file.path(data_dir, "locations_detail_2025-04-23.csv")) |>
  clean_names(case = "snake") |>
  mutate(
    location_id = as.character(location_id),
    across(where(is.character), ~ na_if(.x, "")),
    across(where(is.numeric), ~ replace(.x, is.nan(.x) | is.infinite(.x), NA)),
    across(matches("date$"), as.Date, format = "%Y-%m-%d")
  ) |> 
  rename(acq_stage = acq_stage_gs_lkup)

# Read and clean locations_batch
locations_batch <- readRDS("~/Documents/GitHub/deal_criteria_rules/data/data_all_batches_combined_2025-04-21.rds") |>
  clean_names(case = "snake") |> 
  rename(location_id = row_id)

# Left join to create locations tibble
locations_processed <- locations_parent |>
  left_join(locations_batch, 
            by = c("location_id"),
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

# Define column selector as a vector
mod_columns <- c(
  "outcome",
  "location_id",
  "location_city_name",
  "breeze_brand",
  "total_vpd",
  "lube_bays",
  "repair_bays",
  "lof_cpd",
  "sscw_wpd",
  "ibacw_wpd",
  "crw_tnl_wpd" = "crw_tnl_wpd_gs_lkup",  # Named element for renaming
  "latitude_input_data",
  "longitude_input_data",
  "opportunity_name" = "opportunity_name.x",
  "opportunity_id",
  "sitewise_batch",
  "acq_stage",
  "traffic" = "nearest_streetlight_day_part_aadt_5_mi",
  "pop_2024" = "col_2024_estimate_5_mi",
  "pop_2029" = "col_2029_projection_5_mi",
  "direct_chains" = "count_of_chainxy_vt_oil_and_lube_5_mi",
  "indirect_chains" = "count_of_chainxy_vt_tires_and_auto_service_5_mi",
  "oci" = "count_of_oil_changers_locations_vt_open_5_mi",
  "state_abb",
  "city",
  "street_number",
  "city_state",
  "zcta_code"
)

# Select relevant columns for analysis ----
df_locs_raw <- locations_processed |>
  select(all_of(mod_columns)) |>  # Use all_of() to ensure strict column matching
  mutate(pop2shop = pop_2024 / direct_chains) |>
  filter(!is.na(outcome))

# Temporary data cleanup
df_locs_raw <- df_locs_raw |> 
  # Replace all NA for breeze_brand column to "OC"
  mutate(breeze_brand = replace_na(breeze_brand, "OC")) |>
  # Replace any elements for traffic column <2000 with 2000
  mutate(traffic = ifelse(traffic < 2000, 2000, traffic)) |> 
  # Filter out placeholder records that contain "fake"
  filter(!str_detect(location_city_name, "Fake")) |> 
  # If direct_chains is 0, then pop2shop is equal to pop_2024; round to integer
  mutate(pop2shop = ifelse(direct_chains == 0, round(pop_2024), pop2shop))

# Optionally save the result
write_csv(df_locs_raw, file.path(data_dir, "df_locs_raw.csv"))

df_locs_raw |> count(outcome)
