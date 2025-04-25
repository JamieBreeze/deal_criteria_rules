library(tidyverse)
library(tidymodels)

# Assuming df_raw is already loaded from the previous script
# Create df_imputed with explicit zero imputation for specified columns
df_imputed <- df_locs_outcomes |>
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

# Inspect the result
glimpse(df_imputed)


# Viewer
# df_imputed |> View()

# Save to CSV
write_csv(df_imputed, file.path(data_dir, "df_imputed.csv"))
# Save to RDS
write_rds(df_imputed, file.path(data_dir, "df_imputed.rds"))
# Read the saved RDS
# df_imputed <- read_rds(file.path(data_dir, "df_imputed.rds"))