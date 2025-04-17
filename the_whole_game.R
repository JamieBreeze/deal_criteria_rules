# Step 1: Create df_raw
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
    pass_active_status = `Passed: Active Status`
  ) |>
  mutate(
    breeze_brand = as_factor(breeze_brand),
    lof_cpd = as.numeric(lof_cpd),
    pass_active_status = as.character(pass_active_status)
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

# Create df_raw
df_raw <- locations |>
  left_join(locations_detail, by = "location_id") |>
  left_join(locations_sitewise, by = "location_id") |>
  filter(
    (acq_stage_gs_lkup %in% acq_stage_filter) | (!is.na(pass_active_status) & pass_active_status != "")
  ) |>
  mutate(
    total_vpd = rowSums(across(.cols = c(ends_with("cpd"), ends_with("wpd")), .fns = as.numeric), na.rm = TRUE)
  ) |>
  relocate(total_vpd, .after = breeze_brand) |>
  mutate(
    outcome = case_when(
      acq_stage_gs_lkup %in% c("19 Post Close", "20 Closed") ~ "acquire",
      !acq_stage_gs_lkup %in% c("19 Post Close", "20 Closed") ~ "pass",
      TRUE ~ NA_character_
    ),
    outcome = as_factor(outcome)
  ) |>
  filter(total_vpd > 0)

# Step 2: Create df_imputed
df_imputed <- df_raw |>
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

# Step 3: Create df_mod (include all predictors used in the formula)
df_mod <- df_imputed |>
  select(
    location_id,
    outcome,
    breeze_brand,
    total_vpd,
    lof_cpd,
    crw_tnl_wpd_gs_lkup,
    ibacw_wpd,    # Added
    fcw_wpd,      # Added
    ecw_wpd,      # Added
    sscw_wpd,     # Added
    lube_bays,
    repair_bays
  )

# Step 4: Build and evaluate the decision tree model
library(tidymodels)
library(rpart.plot)

# Set seed for reproducibility
set.seed(123)

# Define the decision tree model specification
dt_spec <- decision_tree(
  tree_depth = tune(),
  min_n = tune(),
  cost_complexity = tune()
) |>
  set_engine("rpart", model = TRUE) |>  # Keep model data for rpart.rules
  set_mode("classification")

# Create a workflow
dt_workflow <- workflow() |>
  add_model(dt_spec) |>
  add_formula(outcome ~ breeze_brand + crw_tnl_wpd_gs_lkup + ibacw_wpd + 
                fcw_wpd + ecw_wpd + sscw_wpd + lof_cpd + lube_bays + repair_bays)

# Define a grid for tuning
dt_grid <- grid_regular(
  tree_depth(range = c(5, 10)),
  min_n(range = c(2, 15)),
  cost_complexity(range = c(-3, -1)),
  levels = 4
)

# Split data into training and testing sets
data_split <- initial_split(df_mod, prop = 0.8, strata = outcome)
train_data <- training(data_split)
test_data <- testing(data_split)

# Perform cross-validation
cv_folds <- vfold_cv(train_data, v = 5, strata = outcome)

# Tune the model
dt_tune <- tune_grid(
  dt_workflow,
  resamples = cv_folds,
  grid = dt_grid,
  metrics = metric_set(accuracy, roc_auc)
)

# Select the best model
best_dt <- select_best(dt_tune, metric = "accuracy")

# Finalize the workflow with the best parameters
final_dt_workflow <- finalize_workflow(dt_workflow, best_dt)

# Fit the final model on the full training data
final_dt_fit <- final_dt_workflow |>
  fit(data = train_data)

# Extract the rpart model for visualization
rpart_model <- final_dt_fit |> 
  extract_fit_engine()

# Visualize the decision tree with 14-point font and number of observations
rpart.plot(
  rpart_model, 
  roundint = FALSE, 
  type = 4, 
  extra = 101,  # Displays the number of observations at each node
  cex = 1.167,  # Sets font size to 14 points (14/12 = 1.167)
  box.palette = "auto",
  branch.lty = 3,
  shadow.col = "gray"
)

# Evaluate on test data
test_results <- final_dt_fit |>
  predict(new_data = test_data) |>
  bind_cols(test_data) |>
  metrics(truth = outcome, estimate = .pred_class)

# View test set performance
test_results

# Extract and analyze
rpart.rules(rpart_model, style = "tall", roundint = FALSE)
rpart_model$variable.importance
print(rpart_model$cptable)

# Optional: Save the final model
saveRDS(final_dt_fit, "decision_tree_model.rds")
