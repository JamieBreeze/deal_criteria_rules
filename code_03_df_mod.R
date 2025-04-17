library(tidyverse)

# Create df_mod by selecting specified columns from df_imputed
df_mod <- df_imputed |>
  select(
    outcome,
    breeze_brand, crw_tnl_wpd_gs_lkup, ibacw_wpd, fcw_wpd, ecw_wpd, sscw_wpd, lof_cpd,
    lube_bays,
    repair_bays
  )

# Inspect the result
glimpse(df_mod)
