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
    pop2shop,
    direct_chains,
    indirect_chains,
    oci,
    independents,
    income_discretionary_median,
    transport_public,
    pop_density,
    hh_density,
    hh_2024,
    vehicles_2024,
    income_public_assistance_prop,
    income_retirement_prop
  ) |> 
  # filter(if_all(.cols = everything(), .fns = ~ !is.na(.x)))
  filter(!is.na(pop_2024) | !is.na(traffic) | traffic < 1)

# Inspect the result
glimpse(df_mod)

# Optionally save the result as CSV
write_csv(df_mod, file.path(data_dir, "df_mod.csv"))
# Optionally save the result as RDS
write_rds(df_mod, file.path(data_dir, "df_mod.rds"))
