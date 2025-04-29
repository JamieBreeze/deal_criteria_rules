library(tidyverse)

summarize_all_stats <- function(df) {
  # Identify all columns
  all_cols <- names(df)
  numeric_cols <- df |> select(where(is.numeric)) |> names()
  non_numeric_cols <- setdiff(all_cols, numeric_cols)
  
  # Numeric columns: summaries (rounded to nearest whole number)
  numeric_exprs <- map(numeric_cols, function(col) {
    col_sym <- sym(col)
    stats <- list(
      count = expr(round(n())),
      median = expr(round(median(!!col_sym, na.rm = TRUE))),
      sd = expr(round(sd(!!col_sym, na.rm = TRUE))),
      min = expr(round(min(!!col_sym, na.rm = TRUE))),
      max = expr(round(max(!!col_sym, na.rm = TRUE))),
      q25 = expr(round(quantile(!!col_sym, 0.25, na.rm = TRUE))),
      q75 = expr(round(quantile(!!col_sym, 0.75, na.rm = TRUE))),
      na_count = expr(round(sum(is.na(!!col_sym))))
    )
    purrr::set_names(stats, str_c(col, "__", names(stats)))
  })
  
  # Non-numeric columns: count, na_count (rounded), others as NA
  non_numeric_exprs <- map(non_numeric_cols, function(col) {
    col_sym <- sym(col)
    stats <- list(
      count = expr(round(n())),
      median = expr(NA_real_),
      sd = expr(NA_real_),
      min = expr(NA_real_),
      max = expr(NA_real_),
      q25 = expr(NA_real_),
      q75 = expr(NA_real_),
      na_count = expr(round(sum(is.na(!!col_sym))))
    )
    purrr::set_names(stats, str_c(col, "__", names(stats)))
  })
  
  # Combine all expressions
  all_exprs <- purrr::flatten(c(numeric_exprs, non_numeric_exprs))
  
  # Summarize
  summary_df <- df |> summarise(!!!all_exprs)
  
  # Pivot to desired format
  summary_df |> 
    pivot_longer(
      everything(),
      names_to = c("measure", "statistic"),
      names_sep = "__",
      values_to = "value"
    ) |> 
    pivot_wider(
      names_from = statistic,
      values_from = value
    ) |> 
    arrange(match(measure, all_cols)) # Match original column order
}

# Load df_mod
df_mod <- readRDS("~/Documents/GitHub/deal_criteria_rules/data/df_mod.rds")

# Apply to df_mod
result <- summarize_all_stats(df_mod)

# View result
result |> glimpse()
