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


# Select relevant columns for prediction ----
location_undernda <- locations_processed |>
  select(all_of(mod_columns)) |>  # Use all_of() to ensure strict column matching
  # Filter for locations if opportunity_id is in undernda$OpportunityID
  filter(opportunity_id %in% undernda$OpportunityID) |> 
  # Impute missing data
  mutate(
    across(
      .cols = c(
        ends_with("_bays"),
        contains("cpd"),
        contains("wpd")
      ),
      .fns = ~ replace_na(.x, 0)
    )
  )

location_undernda |> View()

# Optionally save the result as CSV
write_csv(location_undernda, file.path(data_dir, "location_undernda.csv"))
# Optionally save the result as RDS
write_rds(location_undernda, file.path(data_dir, "location_undernda.rds"))
