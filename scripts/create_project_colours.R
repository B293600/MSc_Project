# create_project_colours.R
# Generates project_colours.rda: shared colour palette for all
# dissertation figures (scripts 02-08). Run once to (re)generate
# the .rda; saves into Adriana_analysis/ (the project root),
# regardless of where this script is run from, so every other
# script can find it via project_dir <- "..".

library(rstudioapi)

# Set working directory to this script's location, then go up one
# level to the project root (Adriana_analysis/), since this script
# is expected to live in Adriana_analysis/scripts/
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

# Colour palette keyed by colour name - matches how project_colours
# is referenced throughout scripts 02-08 (e.g. project_colours["blue"],
# project_colours["red"]). White/grey are the fallback extras for when
# more than four colours are needed on one plot.
project_colours <- c(
  pink   = "#ffa8cb",
  blue   = "#4575b4",
  red    = "#e5000c",
  purple = "#784283",
  white  = "#ffffff",
  grey   = "#999999"
)

save(project_colours, file = "project_colours.rda")
