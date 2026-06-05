library(tidyverse)
library(ggplot2)
library(patchwork)
library(hexbin)

cor_label <- function(x, y, method = "spearman") {
  x <- as.numeric(x)
  y <- as.numeric(y)
  keep <- !is.na(x) & !is.na(y) & is.finite(x) & is.finite(y)
  r <- round(cor(x[keep], y[keep], method = method), 3)
  paste0("R = ", r)
}

scatter_cor <- function(data, x_col, y_col, x_lab, y_lab, title,
                        show_diagonal = FALSE) {
  df_plot <- data %>%
    filter(!is.na(.data[[x_col]]), !is.na(.data[[y_col]]),
           .data[[x_col]] > 0, .data[[y_col]] > 0) %>%
    mutate(x = log2(.data[[x_col]]),
           y = log2(.data[[y_col]]))
  p <- ggplot(df_plot, aes(x = x, y = y)) +
    geom_point(size = 1.5, alpha = 0.2, color = "royalblue3") +
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
    annotate("text", x = -Inf, y = Inf,
             label = cor_label(df_plot$x, df_plot$y),
             hjust = -0.1, vjust = 2) +
    labs(x = x_lab, y = y_lab, title = title) +
    theme_minimal()
  if (show_diagonal) {
    p <- p + geom_abline(slope = 1, intercept = 0, colour = "grey")
  }
  p
}

scatter_cor_pair <- function(data,
                             x_small, y_small,
                             x_large, y_large,
                             x_lab, y_lab,
                             title_small, title_large,
                             show_diagonal = FALSE) {
  p_small <- scatter_cor(data, x_small, y_small, x_lab, y_lab,
                         title_small, show_diagonal = show_diagonal)
  p_large <- scatter_cor(data, x_large, y_large, x_lab, y_lab,
                         title_large, show_diagonal = show_diagonal)
  p_small + p_large
}

hex_cor <- function(data, x_col, y_col, x_lab, y_lab, title,
                    bins = 50, show_diagonal = FALSE) {
  df_plot <- data %>%
    filter(!is.na(.data[[x_col]]), !is.na(.data[[y_col]]),
           .data[[x_col]] > 0, .data[[y_col]] > 0) %>%
    mutate(x = log2(.data[[x_col]]),
           y = log2(.data[[y_col]]))
  p <- ggplot(df_plot, aes(x = x, y = y)) +
    geom_hex(bins = bins) +
    scale_fill_viridis_c(trans = "log2") +
    coord_equal() +
    annotate("text", x = -Inf, y = Inf,
             label = cor_label(df_plot$x, df_plot$y),
             hjust = -0.1, vjust = 2) +
    labs(x = x_lab, y = y_lab, title = title) +
    theme_minimal()
  if (show_diagonal) {
    p <- p + geom_abline(slope = 1, intercept = 0, colour = "grey")
  }
  p
}

hex_cor_pair <- function(data,
                         x_small, y_small,
                         x_large, y_large,
                         x_lab, y_lab,
                         title_small, title_large,
                         bins = 50, show_diagonal = FALSE) {
  p_small <- hex_cor(data, x_small, y_small, x_lab, y_lab,
                     title_small, bins = bins, show_diagonal = show_diagonal)
  p_large <- hex_cor(data, x_large, y_large, x_lab, y_lab,
                     title_large, bins = bins, show_diagonal = show_diagonal)
  p_small + p_large
}

save_pair <- function(plot, filename, width = 10, height = 5, dpi = 300) {
  ggsave(filename, plot, width = width, height = height, dpi = dpi)
}

save_single <- function(plot, filename, width = 5, height = 5, dpi = 300) {
  ggsave(filename, plot, width = width, height = height, dpi = dpi)
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

save(cor_label,
     scatter_cor,
     scatter_cor_pair,
     hex_cor,
     hex_cor_pair,
     save_pair,
     save_single,
     file = "~/desktop/correlation_functions_updated.rda")

cat("Saved successfully\n")