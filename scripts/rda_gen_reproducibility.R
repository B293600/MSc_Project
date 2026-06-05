library(tidyverse)
library(ggplot2)
library(patchwork)
load("correlation_functions_updated.rda")

get_spearman_r <- function(data, col_x, col_y) {
  df <- data %>%
    filter(!is.na(.data[[col_x]]), !is.na(.data[[col_y]]),
           is.finite(.data[[col_x]]), is.finite(.data[[col_y]]))
  cor(df[[col_x]], df[[col_y]], method = "spearman")
}

plot_hl_pair <- function(data, fitting_strategy, rep_x, rep_y,
                         type = c("halflife", "fc"),
                         show_diagonal = TRUE) {
  type <- match.arg(type)
  
  if (type == "halflife") {
    x_small <- paste0(rep_x, "_small_estimate_half_life_by_", fitting_strategy)
    y_small <- paste0(rep_y, "_small_estimate_half_life_by_", fitting_strategy)
    x_large <- paste0(rep_x, "_large_estimate_half_life_by_", fitting_strategy)
    y_large <- paste0(rep_y, "_large_estimate_half_life_by_", fitting_strategy)
    
    p_small <- scatter_cor(data, x_small, y_small,
                           paste0(rep_x, " half-life — small cells (log2)"),
                           paste0(rep_y, " half-life — small cells (log2)"),
                           paste0("Half-life reproducibility — small cells (", fitting_strategy, ")"),
                           show_diagonal = show_diagonal)
    p_large <- scatter_cor(data, x_large, y_large,
                           paste0(rep_x, " half-life — large cells (log2)"),
                           paste0(rep_y, " half-life — large cells (log2)"),
                           paste0("Half-life reproducibility — large cells (", fitting_strategy, ")"),
                           show_diagonal = show_diagonal)
    p_small + p_large
    
  } else {
    x_col <- paste0(rep_x, "_hl_fc_by_", fitting_strategy)
    y_col <- paste0(rep_y, "_hl_fc_by_", fitting_strategy)
    
    scatter_cor(data, x_col, y_col,
                paste0(rep_x, " log2 half-life fold change (large/small)"),
                paste0(rep_y, " log2 half-life fold change (large/small)"),
                paste0("Fold change reproducibility (", fitting_strategy, ")"),
                show_diagonal = show_diagonal)
  }
}

plot_reproducibility_matrix <- function(r_summary) {
  r_long <- r_summary %>%
    pivot_longer(cols      = starts_with("R_"),
                 names_to  = "comparison",
                 values_to = "R") %>%
    mutate(comparison = recode(comparison,
                               R_small_hl = "Half-life\nsmall cells",
                               R_large_hl = "Half-life\nlarge cells",
                               R_fc       = "Fold change\nlarge/small"))
  
  ggplot(r_long, aes(x = fitting_strategy, y = comparison, fill = R)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(R, 3)), size = 4) +
    scale_fill_viridis_c(limits = c(0, 1), option = "plasma") +
    labs(x     = "Fitting strategy",
         y     = NULL,
         title = "Spearman R — MS96 vs MS1039 replicate reproducibility",
         fill  = "R") +
    correlation_theme
}



save(get_spearman_r,
     plot_hl_pair,
     plot_reproducibility_matrix,
     file = "reproducibility_functions.rda")

cat("Saved successfully\n")
cat("Functions saved:\n")
cat("  get_spearman_r\n")
cat("  plot_hl_pair        -- show_diagonal defaults to TRUE (like-for-like comparisons)\n")
cat("  plot_reproducibility_matrix\n")