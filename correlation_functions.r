library(tidyverse)
library(ggplot2)

cor_label <- function(x, y, method = "spearman") {
  x <- as.numeric(x)
  y <- as.numeric(y)
  keep <- !is.na(x) & !is.na(y) & is.finite(x) & is.finite(y)
  r <- round(cor(x[keep], y[keep], method = method), 3)
  paste0("R = ", r)
}

scatter_cor <- function(data, x_col, y_col, x_lab, y_lab, title) {
  df_plot <- data %>%
    filter(!is.na(.data[[x_col]]), !is.na(.data[[y_col]]),
           .data[[x_col]] > 0, .data[[y_col]] > 0) %>%
    mutate(x = log2(.data[[x_col]]),
           y = log2(.data[[y_col]]))
  ggplot(df_plot, aes(x = x, y = y)) +
    geom_point(size = 1.5, alpha = 0.2, color = "royalblue3") +
    geom_abline(slope = 1, intercept = 0, colour = "grey") +
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
    annotate("text", x = -Inf, y = Inf,
             label = cor_label(df_plot$x, df_plot$y),
             hjust = -0.1, vjust = 2) +
    labs(x = x_lab, y = y_lab, title = title) +
    theme_minimal()


}

save(cor_label, scatter_cor, file = "correlation_functions.rda")

