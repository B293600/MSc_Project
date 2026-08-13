library(tidyverse)

go_pc2_top <- read_tsv("../output_tables/go_clusterprofiler_pc2_top10pct.txt",
                       show_col_types = FALSE)

go_pc2_top %>%
  filter(str_detect(Description, regex("amino acid biosynth", ignore_case = TRUE))) %>%
  dplyr::select(ID, Description, GeneRatio, BgRatio, pvalue, p.adjust, qvalue, Count) %>%
  print(n = Inf)