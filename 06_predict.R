library(tidyverse)
library(tidymodels)

# Define data directory (consistent with other scripts)
data_dir <- "data"

# Load the trained decision tree model
final_dt_fit <- readRDS(file.path(data_dir, "decision_tree_model_enhanced.rds"))

# Load locations_undernda
locations_undernda <- readRDS(file.path(data_dir, "locations_undernda.rds"))

# Prepare locations_undernda for prediction
# It already has the correct columns and imputation from 05_undernda.R, but ensure factor levels match
locations_undernda_prepared <- locations_undernda %>%
  mutate(
    breeze_brand = factor(breeze_brand, levels = levels(final_dt_fit$fit$fit$fit$frame$var))
  )

# Predict outcomes for locations_undernda
predictions <- final_dt_fit %>%
  predict(new_data = locations_undernda_prepared) %>%
  bind_cols(locations_undernda_prepared) %>%
  select(
    location_id,
    location_city_name,
    predicted_outcome = .pred_class
  )

# View the predictions
predictions |> View()

# Save the predictions
write_csv(predictions, file.path(data_dir, "locations_undernda_predictions.csv"))
write_rds(predictions, file.path(data_dir, "locations_undernda_predictions.rds"))

# Print the first few predictions
predictions %>%
  head() %>%
  print()
