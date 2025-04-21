library(tidyverse)
library(tidymodels)
library(rpart)
library(rpart.plot)

# Set seed for reproducibility
set.seed(123)

# Load data (assuming df_mod.csv is in the working directory)
# df_mod <- read_csv("df_mod.csv")

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
  add_formula(outcome ~ breeze_brand + crw_tnl_wpd + ibacw_wpd + 
                fcw_wpd + ecw_wpd + sscw_wpd + lof_cpd + lube_bays + repair_bays +
                x2024_estimate_5_mi + nearest_streetlight_day_part_aadt_5_mi)

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
  metrics = metric_set(accuracy, roc_auc, sens, spec)
)

# Select the best model based on accuracy
best_dt <- select_best(dt_tune, metric = "accuracy")

# Show best hyperparameters
print("Best Hyperparameters:")
print(best_dt)

# Finalize the workflow with the best parameters
final_dt_workflow <- finalize_workflow(dt_workflow, best_dt)

# Fit the final model on the full training data
final_dt_fit <- final_dt_workflow |>
  fit(data = train_data)

# Extract the rpart model
rpart_model <- final_dt_fit |> 
  extract_fit_engine()

# Enhanced visualization of the decision tree
png("decision_tree_plot.png", width = 1200, height = 800, res = 100)
rpart.plot(
  rpart_model, 
  roundint = FALSE, 
  type = 4,           # Show splits and probabilities
  extra = 104,       # Show probability of class and number of observations
  cex = 0.9,         # Larger font size for readability
  box.palette = list("Blues", "Reds"), # Color for pass/acquire
  branch.lty = 1,    # Solid branches
  shadow.col = "gray80",
  main = "Decision Tree for M&A Outcome",
  fallen.leaves = TRUE, # Place terminal nodes at the bottom
  tweak = 1.2        # Adjust text spacing
)
dev.off()

# Evaluate on test data with detailed metrics
test_results <- final_dt_fit |>
  predict(new_data = test_data) |>
  bind_cols(test_data) |>
  metrics(truth = outcome, estimate = .pred_class)

# Confusion matrix
conf_mat <- final_dt_fit |>
  predict(new_data = test_data) |>
  bind_cols(test_data) |>
  conf_mat(truth = outcome, estimate = .pred_class)

# Additional metrics: precision, recall
precision <- final_dt_fit |>
  predict(new_data = test_data) |>
  bind_cols(test_data) |>
  precision(truth = outcome, estimate = .pred_class, event_level = "first")

recall <- final_dt_fit |>
  predict(new_data = test_data) |>
  bind_cols(test_data) |>
  recall(truth = outcome, estimate = .pred_class, event_level = "first")

# Print results
cat("\nTest Set Performance:\n")
print(test_results)
cat("\nConfusion Matrix:\n")
print(conf_mat)
cat("\nPrecision (for acquire):\n")
print(precision)
cat("\nRecall (for acquire):\n")
print(recall)

# Variable importance
cat("\nVariable Importance:\n")
print(rpart_model$variable.importance)

# Extract decision rules
cat("\nDecision Rules:\n")
rpart_rules <- rpart.rules(rpart_model, style = "tall", roundint = FALSE)
print(rpart_rules)

# Complexity parameter table
cat("\nComplexity Parameter Table:\n")
print(rpart_model$cptable)

# Save the final model
saveRDS(final_dt_fit, "decision_tree_model_enhanced.rds")
