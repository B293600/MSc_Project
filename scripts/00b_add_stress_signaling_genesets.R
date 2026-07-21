# 00b_add_stress_signaling_genesets.R
# Builds TF/regulator target gene sets from Phospho_Residues_StressSignaling.xlsx
# (GCN2, SNF1, Gis1, RTG, Tod6/Dot6), in the same named-list-of-gene-IDs format
# used elsewhere in the pipeline (e.g. build_go_genesets(), genesets_a/c/e), so
# they can be passed directly into mitch_calc() or fgsea() alongside the
# existing Mahendrawada TF gene sets.
#
# Source: Phospho_Residues_StressSignaling.xlsx, sheets "GCN2 Targets",
# "GCN2 Regulators", "SNF1 Targets", "RTG targets", "Gis1 Targets", "Tod6 Dot6".
# Note: unlike Tables A/C/E, this data has no binding-vs-regulated distinction
# and no log2FC magnitude - each sheet is a simple curated target gene list, so
# gene sets are built as presence/absence (like Table A), not activated/repressed.
#
# Assumes this script sits in Adriana_analysis/scripts/, with tables/ as a
# sibling folder containing the source Excel file.

library(tidyverse)
library(readxl)
library(rstudioapi)

select <- dplyr::select

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

excel_path <- "../tables/Phospho_Residues_StressSignaling.xlsx"

# RTG targets uses gene_id = systematic name, gene_name = common name, as
# expected. SNF1 Targets, Gis1 Targets, and Tod6 Dot6 all have these two
# columns swapped in the source file - corrected here rather than in the
# Excel file itself, so the fix is visible and can be checked against the
# original. (GCN2 Targets and GCN2 Regulators are also in the expected
# systematic-first order.)
read_target_sheet <- function(sheet_name, swap_id_name = FALSE) {
  df <- read_excel(excel_path, sheet = sheet_name)
  
  if (swap_id_name) {
    df <- df %>% rename(gene_id_tmp = gene_name, gene_name = gene_id) %>%
      rename(gene_id = gene_id_tmp)
  }
  
  df %>%
    filter(!is.na(gene_id)) %>%
    pull(gene_id) %>%
    unique()
}

gcn2_targets    <- read_target_sheet("GCN2 Targets")
gcn2_regulators <- read_target_sheet("GCN2 Regulators")
snf1_targets    <- read_target_sheet("SNF1 Targets", swap_id_name = TRUE)
rtg_targets     <- read_target_sheet("RTG targets")
gis1_targets    <- read_target_sheet("Gis1 Targets", swap_id_name = TRUE)
tod6_dot6_lit   <- read_target_sheet("Tod6 Dot6", swap_id_name = TRUE)

stress_signaling_genesets <- list(
  GCN2_targets     = gcn2_targets,
  GCN2_regulators  = gcn2_regulators,
  SNF1_targets     = snf1_targets,
  RTG_targets      = rtg_targets,
  Gis1_targets     = gis1_targets,
  Tod6_Dot6_lit    = tod6_dot6_lit
)

cat("Stress signaling gene sets built:\n")
for (nm in names(stress_signaling_genesets)) {
  n <- length(stress_signaling_genesets[[nm]])
  flag <- if (n < 5) "  <- below the pipeline's usual minimum gene set size (5)" else ""
  cat(" ", nm, ":", n, "genes", flag, "\n")
}

# Sanity check: gene ID format. If a set's IDs don't look like systematic
# ORF names (e.g. "YGL184C"), it silently won't overlap with the PCA data
# and mitch will drop it with no warning - catch that here instead.
cat("\nGene ID format check (should all start with 'Y' and look systematic):\n")
for (nm in names(stress_signaling_genesets)) {
  ids <- stress_signaling_genesets[[nm]]
  looks_systematic <- mean(grepl("^Y[A-P][LR][0-9]{3}[CW](-[A-Z])?$", ids))
  cat(" ", nm, ":", round(looks_systematic * 100), "% look like systematic IDs\n")
}

save(stress_signaling_genesets, file = "../tables/stress_signaling_genesets.rda")
cat("\nSaved to tables/stress_signaling_genesets.rda\n")
