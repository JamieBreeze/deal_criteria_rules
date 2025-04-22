library(tidyverse)

# Create df_mod by selecting specified columns from df_imputed
df_mod <- df_imputed |>
  select(
    outcome,
    breeze_brand,
    lof_cpd,
    ibacw_wpd,
    sscw_wpd,
    crw_tnl_wpd,
    lube_bays,
    repair_bays,
    pop_2024,
    traffic,
    pop2shop
  ) |> 
  # filter(if_all(.cols = everything(), .fns = ~ !is.na(.x)))
  filter(!is.na(pop_2024) | !is.na(traffic) | traffic < 1)

# Inspect the result
glimpse(df_mod)

# Optionally save the result
write_csv(df_mod, file.path(data_dir, "df_mod.csv"))
