# create_project_functions.R
# Generates project_functions.rda: reusable functions shared across
# the enrichment scripts, so they're defined once and loaded rather
# than repeated inline. Run once to (re)generate the .rda.

library(tidyverse)

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

save(setup_project_paths, truncate_wrap_description, file = "project_functions.rda")
