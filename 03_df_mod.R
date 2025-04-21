library(tidyverse)

# Create df_mod by selecting specified columns from df_imputed
df_mod <- df_imputed |>
  select(
    outcome,
    breeze_brand,
    crw_tnl_wpd,
    ibacw_wpd,
    fcw_wpd,
    ecw_wpd,
    sscw_wpd,
    lof_cpd,
    lube_bays,
    repair_bays,
    x2024_estimate_5_mi,
    nearest_streetlight_day_part_aadt_5_mi
  ) |> 
  # filter(if_all(.cols = everything(), .fns = ~ !is.na(.x)))
  filter(!is.na(x2024_estimate_5_mi) | !is.na(nearest_streetlight_day_part_aadt_5_mi) | nearest_streetlight_day_part_aadt_5_mi < 1)

# Inspect the result
glimpse(df_mod)

# Optionally save the result
write_csv(df_mod, file.path(data_dir, "df_mod.csv"))
