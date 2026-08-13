library(tidyverse)

df_sc_final <- read_tsv("../tables/df_sc_final.txt", show_col_types = FALSE)

df_pca_1 <- df_sc_final %>%
  filter(exp_group_4 == "A", TPMmin4 > 2) %>%
  pivot_wider(id_cols = c("gene_id", "gene_name"),
              names_from = sample, values_from = "log2TPMnorm4")

pca_1 <- df_pca_1 %>%
  dplyr::select(where(is.numeric)) %>%
  prcomp(scale. = FALSE, center = FALSE)

pca_variance <- summary(pca_1)$importance[2, ] %>%
  as_tibble(rownames = "PC") %>%
  rename(proportion_variance = value) %>%
  mutate(percent_variance = round(proportion_variance * 100, 2))

# Sanity check - should sum to ~100
sum(pca_variance$proportion_variance)

# Print PC1 through PC20 so we can see where it actually levels off
print(pca_variance %>% slice_head(n = 20), n = 20)

# Scree plot - standard way to visually spot a genuine elbow/plateau
plot(1:nrow(pca_variance), pca_variance$percent_variance,
     type = "b", pch = 16,
     xlab = "Principal Component", ylab = "% variance explained",
     main = "Scree plot: variance explained per PC",
     xlim = c(1, 20))