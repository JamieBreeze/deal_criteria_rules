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
df_imputed |> View()
