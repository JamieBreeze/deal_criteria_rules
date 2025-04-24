# Load libraries
library(datapasta)
library(tidyverse)
library(janitor)

# Use datapasta to paste tribble
undernda <- tibble::tribble(
  ~OpportunityID,
  "Ubs4lqXS9z4Du0bj110RKd",
  "zmQGH1G0FM4qmfKbMnRZ39",
  "KBW0R64QMu4fIuxQF3CCE9",
  "IYwCXH9ghT4BITLI2_aMye",
  "03cmsymqrOilt33mWax3EA",
  "ObcmPuXS7rrMljteHjS9Kr",
  "AvSgGJZKAQ4A6YIC8iz3L3",
  "n9y6f548BB4-I_ZzNUUpe3",
  "F5dUvdve5lVJC&n&3wwN'",
  "Wx6oF-KshG4AQBHQLDCeS6",
  "khXwtTrdsL47Q-2dnTToPf",
  "O6ytdOBm3n4520ZxdtAZ7b",
  "IVc1kekBGRNimfyZ5JuKc6",
  "gC6L0PjsGOwCIoAFxmb8C3"
)

# Create location_undernda tibble
location_undernda <- locations_processed |> 
  # Filter for locations if opportunity_id is in undernda$OpportunityID
  filter(opportunity_id %in% undernda$OpportunityID)

# Select relevant columns for prediction ----
location_undernda <- locations_processed |>
  select(all_of(mod_columns)) |>  # Use all_of() to ensure strict column matching
  # Filter for locations if opportunity_id is in undernda$OpportunityID
  filter(opportunity_id %in% undernda$OpportunityID) |> 
  # Create pop2shop variable
  mutate(pop2shop = pop_2024 / direct_chains) |> View()