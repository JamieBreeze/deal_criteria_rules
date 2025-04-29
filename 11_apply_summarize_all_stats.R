
# Load df_mod
df_mod <- readRDS("~/Documents/GitHub/deal_criteria_rules/data/df_mod.rds")

# Apply to df_mod
result <- summarize_all_stats(df_mod)

# View result
result |> glimpse()
