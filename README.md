This repository contains a single, one-click R script (main_analysis.R) to reproduce the main tables and figures for the manuscript using Web of Science (WoS) Plain Text exports.
Quick start
1) Place your WoS Plain Text export file(s) under:
./data/wos/savedrecs.txt
2) Run the script from the repository root:
source("main_analysis.R")
3) Outputs will be written to a timestamped folder under:
./outputs/run_<YYYYMMDD_HHMMSS>/
Requirements
R version: 4.5.0 (2025-04-11 ucrt)
OS: Windows 11 x64 (build 26100)
Platform: x86_64-w64-mingw32/x64
Required R packages: bibliometrix (5.2.0), dplyr (1.1.4), tidyr (1.3.1), stringr (1.6.0), tibble (3.3.0), ggplot2 (4.0.1), scales (1.4.0), openxlsx (4.2.8.1), patchwork (1.3.2), ragg (1.5.0)
What the script does
1.   Repairs WoS plaintext endings and reads records with bibliometrix::convert2df().
2.   Filters publications by year (default: 2015–2025).
3.  Deduplicates records: first by DOI; if DOI is missing, by normalized Title + First Author + Year + Source.
4. Extracts device/manufacturer and generator-type evidence from metadata (title/abstract/keywords).
5. Generates Table 1 (dataset overview), Table 2 (evidence structure by explicit modality), and Figures 2–4.
Outputs
All outputs are written to a timestamped folder:
./outputs/run_<YYYYMMDD_HHMMSS>/
Tables (CSV): 
tables/Table_1_DatasetOverview.csv
tables/Table_2_EvidenceStructure_Explicit.csv
Figures (TIFF + PNG, 600 dpi):
figures/Figure_2.tif and figures/Figure_2.png
figures/Figure_3.tif and figures/Figure_3.png
figures/Figure_4.tif and figures/Figure_4.png
For questions or issues, please open a GitHub Issue in this repository.
