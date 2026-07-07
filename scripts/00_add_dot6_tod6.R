# 00_add_dot6_tod6.R
# Adds Dot6 and Tod6 gene sets to Supp Table 3 (Mahendrawada et al. format).
# Table A (binding) uses YEASTRACT documented associations (simultaneous
# binding + expression evidence). Table C (regulated) uses Huber et al. 2011
# EMBO J mRNA-seq (sch9-as dot6 tod6 vs sch9-as FC). Table E (direct) is the
# intersection of YEASTRACT targets and Huber 2011 regulated genes.
#
# Source files: YEASTRACT exported gene lists for Dot6p and Tod6p, and the
# Huber 2011 supplementary file (emboj2011221s2.xls).
#
# Assumes this script sits in Adriana_analysis/scripts/, with tables/
# and figures/ as sibling folders (data/ was assumed but doesn't exist -
# emboj2011221s2.xls actually lives in tables/, updated below).

library(tidyverse)
library(readxl)
library(writexl)
library(rstudioapi)

select <- dplyr::select
rename <- dplyr::rename

# Set working directory to this script's location for relative paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

huber_path  <- "../tables/emboj2011221s2.xls"
supp_path   <- "../tables/Supp Table 3.xlsx"
output_path <- "../tables/Supp Table 3 updated.xlsx"

# YEASTRACT documented target genes (systematic ORF IDs)
dot6_yeastract <- c(
  "YBR093C","YBR129C","YBR135W","YBR157C","YBR166C","YBR244W",
  "YBR265W","YCL027W","YCL025C","YDL227C","YDL117W","YDL012C",
  "YDR019C","YDR036C","YDR122W","YDR174W","YDR213W","YDR309C",
  "YDR313C","YDR327W","YDR339C","YEL037C","YER029C","YER066W",
  "YGL080W","YGL079W","YGL062W","YGR012W","YGR049W","YGR134W",
  "YGR139W","YGR146C","YGR219W","YGR262C","YHL031C","YHR018C",
  "YHR074W","YHR094C","YHR127W","YHR135C","YHR146W","YHR179W",
  "YHR200W","YHR214W","YIL169C","YIL135C","YIL103W","YIL064W",
  "YIR016W","YJL167W","YJL164C","YJR003C","YJR011C","YJR083C",
  "YJR086W","YJR112W","YJR130C","YJR150C","YKL206C","YKL028W",
  "YKL001C","YKR020W","YLL050C","YLL022C","YLR015W","YLR049C",
  "YLR099C","YLR154C","YLR164W","YLR231C","YLR232W","YLR265C",
  "YLR268W","YLR269C","YLR292C","YLR297W","YLR341W","YLR359W",
  "YLR398C","YLR416C","YLR428C","YML114C","YML107C","YML007W",
  "YMR023C","YMR034C","YMR042W","YMR072W","YMR085W","YMR144W",
  "YMR145C","YMR151W","YMR171C","YMR173W-A","YMR175W","YMR177W",
  "YMR191W","YMR199W","YMR236W","YMR276W","YMR291W","YMR298W",
  "YMR314W","YMR321C","YNL337W","YNL309W","YNL307C","YNL289W",
  "YNL264C","YNL258C","YNL210W","YNL160W","YNL152W","YNL146W",
  "YNL107W","YNL097C","YNL096C","YNL065W","YNL031C","YNR018W",
  "YNR066C","YNR077C","YOL155C","YOL136C","YOL111C","YOL072W",
  "YOR008C","YOR018W","YOR043W","YOR051C","YOR073W","YOR074C",
  "YOR075W","YOR212W","YOR229W","YOR324C","YOR367W","YPL273W",
  "YPL191C","YPL163C","YPL156C","YPL066W","YPL049C","YPL034W",
  "YPL016W","YPR009W","YPR066W","YPR167C","YPR199C","YAL044C",
  "YAL018C","YAR015W","YAR018C","YAR068W","YBL042C","YBL016W",
  "YBR026C","YBR071W","YJL076W","YJL044C"
)

tod6_yeastract <- c(
  "YBR089W","YBR101C","YBR129C","YBR177C","YBR204C","YBR244W",
  "YBR264C","YBR265W","YCL069W","YDL198C","YDL127W","YDL048C",
  "YDL029W","YDR019C","YDR099W","YDR154C","YEL036C","YER028C",
  "YER029C","YER064C","YFR025C","YFR041C","YGL162W","YGL085W",
  "YGL062W","YGR012W","YGR015C","YGR033C","YGR035C","YGR065C",
  "YGR108W","YGR138C","YGR139W","YGR142W","YGR146C","YGR253C",
  "YHL031C","YHR018C","YHR124W","YHR146W","YHR200W","YIL118W",
  "YIR010W","YIR030C","YJL124C","YKL211C","YKL206C","YKR017C",
  "YKR032W","YLR099C","YLR183C","YLR297W","YLR332W","YLR384C",
  "YML121W","YML027W","YMR023C","YMR034C","YMR104C","YMR144W",
  "YMR145C","YMR189W","YMR199W","YMR200W","YMR303C","YMR314W",
  "YNL310C","YNL289W","YNL240C","YNL180C","YNL117W","YNL116W",
  "YNL090W","YNL065W","YNR035C","YNR041C","YOL101C","YOL047C",
  "YOR026W","YOR043W","YOR074C","YOR092W","YOR202W","YOR229W",
  "YOR354C","YOR367W","YPL250C","YPL163C","YPL016W","YPL014W",
  "YPL012W","YPR074C","YAL044C","YBL006C","YBR035C","YBR054W",
  "YJL076W","YJL052W","YJL044C","YBR219C"
)

cat("Dot6 YEASTRACT targets:", length(dot6_yeastract), "\n")
cat("Tod6 YEASTRACT targets:", length(tod6_yeastract), "\n")
cat("Shared:", length(intersect(dot6_yeastract, tod6_yeastract)), "\n")

# Load Huber 2011 fold-change data (double mutant vs sch9-as)
cat("\nLoading Huber 2011 data...\n")
huber <- readxl::read_xls(huber_path, sheet = "Fold") %>%
  rename(gene_id = ORF) %>%
  filter(!is.na(`sch9-as dot6 tod6`))

cat("Huber genes with FC data:", nrow(huber), "\n")

# Load existing Supp Table 3 sheets to update
cat("Loading Supp Table 3...\n")
table_a <- read_excel(supp_path, sheet = "Table-S3a")
table_c <- read_excel(supp_path, sheet = "Table-S3c")
table_e <- read_excel(supp_path, sheet = "Table-S3e")

cat("Table A:", nrow(table_a), "genes\n")
cat("Table C:", nrow(table_c), "genes\n")
cat("Table E:", nrow(table_e), "genes\n")

# Table A: flag documented Dot6/Tod6 targets as binary columns
table_a_updated <- table_a %>%
  mutate(
    Dot6 = as.integer(gene_id %in% dot6_yeastract),
    Tod6 = as.integer(gene_id %in% tod6_yeastract)
  )

cat("\nTable A additions:\n")
cat("  Dot6 targets in Table A:", sum(table_a_updated$Dot6), "\n")
cat("  Tod6 targets in Table A:", sum(table_a_updated$Tod6), "\n")

# Table C: add combined Dot6_Tod6 fold-change column from Huber 2011
# NOTE FOR ADRIANA: this is a double-mutant (dot6Δ tod6Δ) signal
# standing in for two separate single-TF columns, since individual
# dot6Δ / tod6Δ FC data wasn't available in Huber 2011 — bear this
# in mind if attributing effects to Dot6 or Tod6 individually.
huber_fc <- huber %>%
  select(gene_id, Dot6_Tod6 = `sch9-as dot6 tod6`)

table_c_updated <- table_c %>%
  left_join(huber_fc, by = "gene_id")

cat("\nTable C additions:\n")
cat("  Genes with Dot6_Tod6 FC:", sum(!is.na(table_c_updated$Dot6_Tod6)), "\n")
cat("  Repressed by Dot6/Tod6 (FC < 0):",
    sum(table_c_updated$Dot6_Tod6 < 0, na.rm = TRUE), "\n")
cat("  Activated by Dot6/Tod6 (FC > 0):",
    sum(table_c_updated$Dot6_Tod6 > 0, na.rm = TRUE), "\n")

# Table E: keep Dot6_Tod6 FC only for genes also bound per YEASTRACT
yeastract_union <- union(dot6_yeastract, tod6_yeastract)

table_e_updated <- table_e %>%
  left_join(huber_fc, by = "gene_id") %>%
  mutate(Dot6_Tod6 = if_else(gene_id %in% yeastract_union,
                             Dot6_Tod6, NA_real_))

cat("\nTable E additions:\n")
cat("  Direct Dot6_Tod6 targets (YEASTRACT + Huber FC):",
    sum(!is.na(table_e_updated$Dot6_Tod6)), "\n")

# Write updated sheets to a new Supp Table 3 file
cat("\nSaving updated Supp Table 3 to:\n", output_path, "\n")
write_xlsx(
  list(
    "Table-S3a" = table_a_updated,
    "Table-S3c" = table_c_updated,
    "Table-S3e" = table_e_updated
  ),
  path = output_path
)

cat("\nDone!\n")
cat("Summary:\n")
cat("  Table A: added Dot6 (", sum(table_a_updated$Dot6),
    "targets) and Tod6 (", sum(table_a_updated$Tod6),
    "targets) binary columns\n")
cat("  Table C: added Dot6_Tod6 FC column (Huber 2011)\n")
cat("  Table E: added Dot6_Tod6 direct targets (",
    sum(!is.na(table_e_updated$Dot6_Tod6)), "genes)\n")
