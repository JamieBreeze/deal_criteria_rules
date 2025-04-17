library(tidyverse)
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
  set_engine("rpart") |>
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
  cex = 0.70,  # Sets font size to 12 points (12/12 = 1.000)
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
rpart.rules(rpart_model, style = "tall")
rpart_model$variable.importance
print(rpart_model$cptable)

# Optional: Save the final model
saveRDS(final_dt_fit, "decision_tree_model.rds")
