library(tidyverse)
library(janitor)
library(readr)

# Define acquisition stage filter
acq_stage_filter <- c(
  "19 Post Close",
  "20 Closed",
  "06 Corp Dev Pass",
  "08 Ops Pass",
  "10 BoD Pass",
  "18 Chopping Block"
)

# Read and process locations
locations <- read_csv("data/locations_2025-04-16.csv") |>
  select(
    location_id = LocationID,
    breeze_brand = `Breeze Brand`,
    lof_cpd = `LOF CPD`
  ) |>
  mutate(
    breeze_brand = as_factor(breeze_brand),
    lof_cpd = as.numeric(lof_cpd)
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

# Join datasets
df_raw <- locations |>
  left_join(locations_detail, by = "location_id") |>
  left_join(locations_sitewise, by = "location_id") |>
  # Filter for specified acquisition stages
   #filter(acq_stage_gs_lkup %in% acq_stage_filter) |>
  # Calculate total_vpd using across
  mutate(
    total_vpd = rowSums(across(.cols = c(ends_with("cpd"), ends_with("wpd")), .fns = as.numeric), na.rm = TRUE)
  ) |>
  # Relocate total_vpd after breeze_brand
  relocate(total_vpd, .after = breeze_brand) |>
  # Create outcome column
  mutate(
    outcome = case_when(
      acq_stage_gs_lkup %in% c("19 Post Close", "20 Closed") ~ "acquire",
      acq_stage_gs_lkup %in% c("06 Corp Dev Pass", "08 Ops Pass", "10 BoD Pass", "18 Chopping Block") ~ "pass",
      TRUE ~ NA_character_
    ),
    outcome = as_factor(outcome)
  ) |>
  # Filter for positive total_vpd
  filter(total_vpd > 0) |>
  relocate(outcome, .after = location_id)

# Inspect the result
glimpse(df_raw)
