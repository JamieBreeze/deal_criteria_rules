library(tidyverse)
library(tidymodels)
library(rpart)
library(rpart.plot)

# Load the training data (df_mod.csv, as used in 04_rpart.R)
df_mod <- read_csv("data/df_mod.csv")

# Prepare locations_undernda for prediction
# Select and rename columns to match df_mod, impute missing values
locations_undernda_prepared <- locations_undernda %>%
  # Select relevant columns and rename to match df_mod
  select(
    outcome,
    breeze_brand,
    crw_tnl_wpd,
    ibacw_wpd,
    # fcw_wpd and ecw_wpd are not in locations_undernda, set to 0 (as they were in df_mod)
    fcw_wpd = 0,
    ecw_wpd = 0,
    sscw_wpd,
    lof_cpd,
    lube_bays,
    repair_bays,
    x2024_estimate_5_mi = pop_2024,
    # nearest_streetlight_day_part_aadt_5_mi not available, impute with median from df_mod
    nearest_streetlight_day_part_aadt_5_mi = traffic
  ) %>%
  # Impute missing values (following the logic from 02_locations_outcome_impute.R)
  mutate(
    across(
      .cols = c(
        ends_with("_bays"),
        contains("cpd"),
        contains("wpd")
      ),
      .fns = ~ replace_na(.x, 0)
    ),
    # Impute nearest_streetlight_day_part_aadt_5_mi with median from df_mod
    nearest_streetlight_day_part_aadt_5_mi = replace_na(
      nearest_streetlight_day_part_aadt_5_mi,
      median(df_mod$nearest_streetlight_day_part_aadt_5_mi, na.rm = TRUE)
    )
  )

# Define the decision tree model specification (using reasonable defaults based on prior work)
dt_spec <- decision_tree(
  tree_depth = 10,           # From prior tuning range
  min_n = 5,                # From prior tuning range
  cost_complexity = 0.01    # From prior tuning range
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

# Create a workflow
dt_workflow <- workflow() %>%
  add_model(dt_spec) %>%
  add_formula(outcome ~ breeze_brand + crw_tnl_wpd + ibacw_wpd + 
                fcw_wpd + ecw_wpd + sscw_wpd + lof_cpd + lube_bays + repair_bays +
                x2024_estimate_5_mi + nearest_streetlight_day_part_aadt_5_mi)

# Fit the model on df_mod
final_dt_fit <- dt_workflow %>%
  fit(data = df_mod)

# Predict outcomes for locations_undernda
predictions <- final_dt_fit %>%
  predict(new_data = locations_undernda_prepared) %>%
  bind_cols(locations_undernda_prepared) %>%
  select(location_id = locations_undernda$location_id, predicted_outcome = .pred_class)

# Save the predictions
write_csv(predictions, "data/locations_undernda_predictions.csv")

# Print the first few predictions
predictions %>%
  head() %>%
  print()