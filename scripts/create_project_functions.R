# create_project_functions.R
# Generates project_functions.rda: reusable functions shared across
# the enrichment scripts, so they're defined once and loaded rather
# than repeated inline. Run once to (re)generate the .rda.
#
# Saves project_functions.rda into Adriana_analysis/ (the project
# root), regardless of where this script is run from, so every other
# script can find it via project_dir <- "..".

library(tidyverse)
library(rstudioapi)

# Set working directory to this script's location, then go up one
# level to the project root (Adriana_analysis/), since this script
# is expected to live in Adriana_analysis/scripts/
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

# Sets up the standard tables/figures/output_tables folder structure
# relative to project_dir, creating figures/ and output_tables/ if
# they don't already exist. Returns paths as a named list.
setup_project_paths <- function(project_dir = "..") {
  tables_dir  <- file.path(project_dir, "tables")
  figures_dir <- file.path(project_dir, "figures")
  output_dir  <- file.path(project_dir, "output_tables")
  
  dir.create(figures_dir, showWarnings = FALSE)
  dir.create(output_dir, showWarnings = FALSE)
  
  list(
    project_dir = project_dir,
    tables_dir  = tables_dir,
    figures_dir = figures_dir,
    output_dir  = output_dir
  )
}

# Shortens a GO term description for plot axis labels: truncates
# anything over trunc_width characters (with an ellipsis), then
# wraps the result onto multiple lines at wrap_width.
truncate_wrap_description <- function(description, trunc_width = 60, wrap_width = 40) {
  short <- if_else(
    nchar(description) > trunc_width,
    str_trunc(description, trunc_width, ellipsis = "..."),
    description
  )
  str_wrap(short, width = wrap_width)
}

# Truncates a label to width characters with an ellipsis, without
# wrapping - used for single-line labels (e.g. ggrepel points on 2D
# scatter plots) where truncate_wrap_description's wrapping isn't wanted.
truncate_label <- function(label, width = 40) {
  if_else(
    nchar(label) > width,
    str_trunc(label, width, ellipsis = "..."),
    label
  )
}

save(setup_project_paths, truncate_wrap_description, truncate_label, file = "project_functions.rda")
