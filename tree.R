library(tidyverse)
library(tidymodels)
library(rpart)
library(rpart.plot)
library(janitor)

# Set seed for reproducibility
set.seed(123)

# Ensure df_raw is available (assumes it was created by previous script)
# If not loaded, run the data processing script (process_deal_criteria.R)

# Prepare data: select relevant columns and check for missing target
df_model <- df_mod |>
  select(
    acq_stage_gs_lkup,
    breeze_brand,
    lof_cpd,
    lube_bays,
    repair_bays,
    population_2029_5mi,
    traffic_1mi
  ) |>
  filter(!is.na(acq_stage_gs_lkup)) # Remove rows with missing target

# Check if data is sufficient
if (nrow(df_model) == 0) {
  stop("No rows with non-missing acq_stage_gs_lkup. Check data quality.")
}

# Summarize missing values before imputation
missing_summary <- df_model |>
  summarise(across(
    .cols = everything(),
    .fns = ~sum(is.na(.)),
    .names = "missing_{.col}"
  ))
message("Missing values before imputation:")
print(missing_summary)

# Define tidymodels recipe for imputation
impute_recipe <- recipe(
  acq_stage_gs_lkup ~ breeze_brand + lof_cpd + lube_bays + 
    repair_bays + population_2029_5mi + traffic_1mi,
  data = df_model
) |>
  step_impute_median(lof_cpd, lube_bays, repair_bays, 
                     population_2029_5mi, traffic_1mi) |>
  step_impute_mode(breeze_brand)

# Define tidymodels workflow
dt_spec <- decision_tree(
  mode = "classification",
  engine = "rpart"
)

# Create workflow with recipe
dt_workflow <- workflow() |>
  add_recipe(impute_recipe) |>
  add_model(dt_spec)

# Fit the model
dt_fit <- dt_workflow |>
  fit(data = df_model)

# Extract the rpart model for plotting
rpart_model <- dt_fit |>
  extract_fit_engine()

# Print model summary
print(rpart_model)

# Visualize the decision tree
tryCatch({
  png("decision_tree_plot.png", width = 14, height = 10, units = "in", res = 300)
  rpart.plot(
    rpart_model,
    roundint = FALSE,
    main = "Decision Tree for Acquisition Stage",
    extra = 104, # Display probability and class
    fallen.leaves = TRUE,
    box.palette = "Blues" # Explicitly set valid palette
  )
  dev.off()
}, error = function(e) {
  message("Error in plotting decision tree: ", e$message)
  message("Try checking rpart.plot package version or reinstall