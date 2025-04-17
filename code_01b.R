library(tidyverse)
library(janitor)


# Define acquisition stage filter values
acq_stage_filter <- c(
  "19 Post Close",
  "20 Closed",
  "06 Corp Dev Pass",
  "08 Ops Pass",
  "10 BoD Pass",
  "18 Chopping Block"
)

# Read and process locations (including Passed: Active Status)
locations <- read_csv("data/locations_2025-04-16.csv") |>
  select(
    location_id = LocationID,
    breeze_brand = `Breeze Brand`,
    lof_cpd = `LOF CPD`,
    pass_active_status = `Passed: Active Status`  # Corrected column name
  ) |>
  mutate(
    breeze_brand = as_factor(breeze_brand),
    lof_cpd = as.numeric(lof_cpd),
    pass_active_status = as.character(pass_active_status)  # Ensure it's character for non-empty check
  ) |>
  clean_names()

# Read and process locations details
locations_detail <- read_csv("data/locations_detail_2025-04-16.csv") |>
  select(
    location_id = `Location ID`,
    lube_bays = `Lube Bays`,
    repair_bays = `Repair Bays`,
    sscw_wpd = `SSCW WPD`,
    ecw_wpd = `ECW WPD`,
    fcw_wpd = `FCW WPD`,
    ibacw_wpd = `IBACW WPD`,
    crw_tnl_wpd_gs_lkup = `CRW TNL WPD GS LKUP`,
    acq_stage_gs_lkup = `Acq Stage GS LKUP`
  ) |>
  mutate(
    location_id = as.character(location_id),
    lube_bays = as.numeric(lube_bays),
    repair_bays = as.numeric(repair_bays),
    sscw_wpd = as.numeric(sscw_wpd),
    ecw_wpd = as.numeric(ecw_wpd),
    fcw_wpd = as.numeric(fcw_wpd),
    ibacw_wpd = as.numeric(ibacw_wpd),
    crw_tnl_wpd_gs_lkup = as.numeric(crw_tnl_wpd_gs_lkup),
    acq_stage_gs_lkup = as_factor(acq_stage_gs_lkup)
  ) |>
  clean_names()

# Read and process locations sitewise
locations_sitewise <- read_csv("data/locations_sitewise_2025-04-16.csv") |>
  select(
    location_id = LocationID,
    population_2029_5mi = `Population 2029 5mi`,
    traffic_1mi = `Traffic 1mi`
  ) |>
  mutate(
    population_2029_5mi = as.numeric(population_2029_5mi),
    traffic_1mi = as.numeric(traffic_1mi)
  ) |>
  clean_names()

# Join datasets and create df_raw with updated filter and outcome logic
df_raw <- locations |>
  left_join(locations_detail, by = "location_id") |>  # Join with locations_detail to get acq_stage_gs_lkup
  left_join(locations_sitewise, by = "location_id") |>  # Then join with locations_sitewise
  # Filter for specified acquisition stages OR non-empty Passed: Active Status
  filter(
    (acq_stage_gs_lkup %in% acq_stage_filter) | (!is.na(pass_active_status) & pass_active_status != "")
  ) |>
  # Calculate total_vpd using across
  mutate(
    total_vpd = rowSums(across(.cols = c(ends_with("cpd"), ends_with("wpd")), .fns = as.numeric), na.rm = TRUE)
  ) |>  # Corrected pipe operator
  # Relocate total_vpd after breeze_brand
  relocate(total_vpd, .after = breeze_brand) |>  # Corrected pipe operator
  # Create outcome column with updated logic
  mutate(
    outcome = case_when(
      acq_stage_gs_lkup %in% c("19 Post Close", "20 Closed") ~ "acquire",
      !acq_stage_gs_lkup %in% c("19 Post Close", "20 Closed") ~ "pass",
      TRUE ~ NA_character_
    ),
    outcome = as_factor(outcome)
  ) |>  # Corrected pipe operator
  # Filter for positive total_vpd
  filter(total_vpd > 0)

# Inspect the result
glimpse(df_raw)
