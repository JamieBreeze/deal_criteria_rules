library(tidyverse)
library(tidymodels)
library(C50)

set.seed(123)

# Data split
data_split <- initial_split(df_mod, prop = 0.8, strata = outcome)
train_data <- training(data_split)
test_data <- testing(data_split)

# CV folds
cv_folds <- vfold_cv(train_data, v = 5, strata = outcome)

# Define tunable model spec (min_n only for now)
dt_spec <- decision_tree(
  cost_complexity = NULL,
  tree_depth = NULL,
  min_n = tune()
) %>%
  set_engine("C5.0", control = C5.0Control(CF = 0.25)) %>%
  set_mode("classification")

# Workflow
dt_workflow <- workflow() %>%
  add_model(dt_spec) %>%
  add_formula(outcome ~ breeze_brand + crw_tnl_wpd_gs_lkup + ibacw_wpd + 
                fcw_wpd + ecw_wpd + sscw_wpd + lof_cpd + lube_bays + repair_bays)

# Create manual tuning grid including trials and min_n
dt_grid <- expand_grid(
  trials = c(1, 3, 5, 7),
  min_n = c(1, 3, 5)
)

# Loop through grid manually
results <- dt_grid %>%
  mutate(
    workflow = map2(trials, min_n, ~ {
      spec <- decision_tree(min_n = .y) %>%
        set_engine("C5.0", trials = .x, control = C5.0Control(CF = 0.25)) %>%
        set_mode("classification")
      
      workflow() %>%
        add_model(spec) %>%
        add_formula(outcome ~ breeze_brand + crw_tnl_wpd_gs_lkup + ibacw_wpd + 
                      fcw_wpd + ecw_wpd + sscw_wpd + lof_cpd + lube_bays + repair_bays)
    }),
    tuned = map(workflow, ~ fit_resamples(
      .x,
      resamples = cv_folds,
      metrics = metric_set(accuracy, roc_auc),
      control = control_resamples(save_pred = TRUE)
    ))
  )

# Collect and unnest results
tune_results <- results %>%
  mutate(metrics = map(tuned, collect_metrics)) %>%
  unnest(metrics)

# Find best by accuracy
best_model_row <- tune_results %>%
  filter(.metric == "accuracy") %>%
  arrange(desc(mean)) %>%
  slice(1)

best_params <- best_model_row %>%
  select(trials, min_n) %>%
  distinct()

best_trials <- best_params$trials
best_min_n <- best_params$min_n


# Final spec and fit
final_spec <- decision_tree(min_n = best_min_n) %>%
  set_engine("C5.0", trials = best_trials, control = C5.0Control(CF = 0.25)) %>%
  set_mode("classification")

final_workflow <- workflow() %>%
  add_model(final_spec) %>%
  add_formula(outcome ~ breeze_brand + crw_tnl_wpd_gs_lkup + ibacw_wpd + 
                fcw_wpd + ecw_wpd + sscw_wpd + lof_cpd + lube_bays + repair_bays)

final_fit <- final_workflow %>% fit(data = train_data)

# Predict and evaluate
final_fit %>%
  predict(new_data = test_data) %>%
  bind_cols(test_data) %>%
  metrics(truth = outcome, estimate = .pred_class)


# Refit the final model using best parameters
final_spec <- decision_tree(min_n = best_min_n) %>%
  set_engine("C5.0", trials = best_trials, control = C5.0Control(CF = 0.25)) %>%
  set_mode("classification")

final_workflow <- workflow() %>%
  add_model(final_spec) %>%
  add_formula(outcome ~ breeze_brand + crw_tnl_wpd_gs_lkup + ibacw_wpd + 
                fcw_wpd + ecw_wpd + sscw_wpd + lof_cpd + lube_bays + repair_bays)

final_fit <- final_workflow %>% fit(data = train_data)

# Extract the C5.0 engine
c50_model <- extract_fit_engine(final_fit)

# Plot the decision tree (14-point font = cex = 1.167)
if (inherits(c50_model, "C5.0")) {
  plot(c50_model, main = "Best C5.0 Decision Tree", cex = 1.167)
} else {
  warning("Model is not a C5.0 object. Falling back to summary.")
  print(summary(c50_model))
}
