```{r create_gba_functions}
## Run this chunk once to save gba_functions.rda
## Do not include in main analysis Rmd

library(tidyverse)
library(ggplot2)
library(Hmisc)

col_small <- "#784283"
col_large <- "#ffa8cb"
col_A     <- "#e5000c"

cell_theme <- theme_classic(base_size = 14) +
  theme(
    axis.title        = element_text(size = 12, colour = "black"),
    axis.text         = element_text(size = 11, colour = "black"),
    axis.line         = element_line(colour = "black", linewidth = 0.4),
    axis.ticks        = element_line(colour = "black", linewidth = 0.4),
    plot.tag          = element_text(size = 14, face = "bold"),
    plot.tag.position = "topleft",
    plot.title        = element_text(size = 12),
    plot.background   = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA)
  )

r_label <- function(x, y) {
  keep <- !is.na(x) & !is.na(y) & is.finite(x) & is.finite(y)
  r    <- round(cor(x[keep], y[keep], method = "spearman"), 3)
  n    <- sum(keep)
  paste0("R = ", r, "\nn = ", format(n, big.mark = ","))
}

scatter_binned <- function(data, x_col, y_col, x_lab, y_lab,
                           title = "", bins = 20,
                           point_col = col_small,
                           ylim = NULL,
                           xlim = NULL) {
  df <- data %>%
    dplyr::filter(!is.na(.data[[x_col]]), !is.na(.data[[y_col]]),
                  is.finite(.data[[x_col]]), is.finite(.data[[y_col]]))
  
  bin_sum <- Hmisc::cut2(df[[x_col]], g = bins) %>%
    bind_cols(df[[x_col]], df[[y_col]]) %>%
    dplyr::rename(bin = 1, x = 2, y = 3) %>%
    drop_na() %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(x = mean(x), y = mean(y), .groups = "drop")
  
  lab <- r_label(df[[x_col]], df[[y_col]])
  
  p <- ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_point(size = 0.8, alpha = 0.2, colour = point_col) +
    geom_line(data = bin_sum, aes(x = x, y = y),
              colour = "black", linewidth = 0.8) +
    annotate("text", x = -Inf, y = Inf,
             label = lab, hjust = -0.1, vjust = 1.4,
             size = 4, colour = "black") +
    labs(x = x_lab, y = y_lab, title = title) +
    cell_theme
  
  if (!is.null(ylim) & !is.null(xlim)) {
    p <- p + coord_cartesian(ylim = ylim, xlim = xlim)
  } else if (!is.null(ylim)) {
    p <- p + coord_cartesian(ylim = ylim)
  } else if (!is.null(xlim)) {
    p <- p + coord_cartesian(xlim = xlim)
  }
  p
}

violin_comparison <- function(data, flag_col, value_col,
                              x_lab, y_lab, title = "",
                              sensitive_label = "Sensitive",
                              other_label     = "Other",
                              ylim = NULL) {
  df <- data %>%
    dplyr::filter(!is.na(.data[[flag_col]]),
                  !is.na(.data[[value_col]]),
                  is.finite(.data[[value_col]])) %>%
    dplyr::mutate(group = ifelse(.data[[flag_col]],
                                 sensitive_label, other_label),
                  group = factor(group,
                                 levels = c(sensitive_label, other_label)))
  
  sens  <- df[[value_col]][df$group == sensitive_label]
  other <- df[[value_col]][df$group == other_label]
  wt    <- wilcox.test(sens, other)
  pval  <- signif(wt$p.value, 3)
  plab  <- if (pval < 0.001) "p < 0.001" else paste0("p = ", pval)
  
  med_sens  <- round(median(sens,  na.rm = TRUE), 3)
  med_other <- round(median(other, na.rm = TRUE), 3)
  
  p <- ggplot(df, aes(x = group, y = .data[[value_col]],
                      fill = group)) +
    geom_violin(alpha = 0.6, linewidth = 0.4, trim = TRUE) +
    geom_boxplot(width = 0.12, outlier.shape = NA,
                 linewidth = 0.4, fill = "white") +
    scale_fill_manual(values = c(col_small, "grey70")) +
    annotate("text", x = 1.5, y = Inf,
             label = plab, vjust = 1.5, size = 4) +
    annotate("text", x = 1, y = -Inf,
             label = paste0("median ", med_sens),
             vjust = -0.5, size = 3.5, colour = col_small) +
    annotate("text", x = 2, y = -Inf,
             label = paste0("median ", med_other),
             vjust = -0.5, size = 3.5, colour = "grey40") +
    labs(x = x_lab, y = y_lab, title = title) +
    cell_theme +
    theme(legend.position = "none")
  
  if (!is.null(ylim)) p <- p + coord_cartesian(ylim = ylim)
  p
}

save(cell_theme, r_label, scatter_binned, violin_comparison,
     col_small, col_large, col_A,
     file = "gba_functions.rda")

cat("Saved gba_functions.rda\n")
```