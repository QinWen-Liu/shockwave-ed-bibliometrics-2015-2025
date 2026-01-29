# Reproducible Analysis Script (Main Manuscript)
#
# Inputs:
#   - ./data/wos/savedrecs*.txt
#
# Outputs:
#   - ./outputs/run_<timestamp>/figures/Figure_2|3|4.(tif|png) 
#   - ./outputs/run_<timestamp>/tables/Table_1_DatasetOverview.csv
#   - ./outputs/run_<timestamp>/tables/Table_2_EvidenceStructure_Explicit.csv

# 0) Global settings
Sys.setenv(LANGUAGE = "en")
options(stringsAsFactors = FALSE)
rm(list = ls()); gc()
set.seed(123)

YEAR_START <- 2015
YEAR_END   <- 2025
FIG_RES    <- 600


proj_root   <- "."
wos_dir     <- file.path(proj_root, "data", "wos")
exclude_xlsx <- file.path(wos_dir, "excluded_records.xlsx")


run_tag    <- format(Sys.time(), "%Y%m%d_%H%M%S")
run_dir    <- file.path(proj_root, "outputs", paste0("run_", run_tag))
table_dir  <- file.path(run_dir, "tables")
fig_dir    <- file.path(run_dir, "figures")
repair_dir <- file.path(run_dir, "_repaired_wos")

dir.create(run_dir,    showWarnings = FALSE, recursive = TRUE)
dir.create(table_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir,    showWarnings = FALSE, recursive = TRUE)
dir.create(repair_dir, showWarnings = FALSE, recursive = TRUE)

message("WoS dir: ", wos_dir)
message("Run dir: ", run_dir)

# 1) Packages
needed_pkgs <- c(
  "bibliometrix",
  "dplyr","tidyr","stringr","tibble",
  "ggplot2","scales",
  "openxlsx",
  "patchwork",
  "RColorBrewer",
  "ragg"
)

for (p in needed_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE, repos = "https://cloud.r-project.org")
  }
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}

# 2) Plot theme + save helper
FONT_FAMILY <- "sans"

COL_FOCUSED <- "#2C7FB8"
COL_RADIAL  <- "#E76F51"
COL_MIXED   <- "#2A9D8F"
COL_UNSPEC  <- "#D9D9D9"
COL_GRAY    <- "grey35"
COL_GRID    <- "grey90"

theme_jsm <- function(base_size = 12, legend_pos = "top") {
  ggplot2::theme_minimal(base_size = base_size, base_family = FONT_FAMILY) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = base_size + 1),
      axis.text  = ggplot2::element_text(color = "black", size = base_size),
      panel.grid.major = ggplot2::element_line(color = COL_GRID, linewidth = 0.6),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = legend_pos,
      legend.title     = ggplot2::element_text(size = base_size),
      legend.text      = ggplot2::element_text(size = base_size),
      plot.margin      = ggplot2::margin(t = 10, r = 14, b = 10, l = 12)
    )
}

.safe_dev_off <- function() {
  if (grDevices::dev.cur() != 1) grDevices::dev.off()
}

save_plot_dual <- function(plot_obj, filename_stem, width = 8, height = 5, res = FIG_RES) {
  tiff_path <- file.path(fig_dir, paste0(filename_stem, ".tif"))
  png_path  <- file.path(fig_dir, paste0(filename_stem, ".png"))
  
  tryCatch({
    ragg::agg_tiff(tiff_path, width = width, height = height, units = "in", res = res, compression = "lzw")
    print(plot_obj)
  }, error = function(e) message("Plot failed [tif] ", filename_stem, ": ", e$message),
  finally = .safe_dev_off())
  
  tryCatch({
    ragg::agg_png(png_path, width = width, height = height, units = "in", res = res)
    print(plot_obj)
  }, error = function(e) message("Plot failed [png] ", filename_stem, ": ", e$message),
  finally = .safe_dev_off())
}

panel_tag_theme <- ggplot2::theme(
  plot.tag = ggplot2::element_text(face = "bold", size = 13, family = FONT_FAMILY),
  plot.tag.position = c(0.01, 0.99)
)

# 3) Helpers
get_text <- function(x) ifelse(is.na(x), "", as.character(x))

clean_text <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x[x %in% c("", "NA", "N/A", "na", "n/a", "Unknown", "unknown")] <- NA_character_
  x
}

clean_doi <- function(x) {
  x <- tolower(trimws(get_text(x)))
  x <- gsub("^doi:\\s*", "", x)
  x <- gsub("^https?://(dx\\.)?doi\\.org/", "", x)
  gsub("\\s+", "", x)
}

get_first_author <- function(authors) {
  if (is.na(authors) || authors == "") return("")
  trimws(strsplit(authors, ";")[[1]][1])
}

norm_title <- function(ti) {
  x <- tolower(get_text(ti))
  gsub("[^a-z0-9]+", "", x)
}

read_exclusion_uts <- function(xlsx_path) {
  if (!file.exists(xlsx_path)) return(character(0))
  sh <- openxlsx::getSheetNames(xlsx_path)
  pick <- sh[grepl("^Excluded", sh, ignore.case = TRUE)]
  sheet_use <- if (length(pick) > 0) pick[1] else sh[1]
  df <- tryCatch(openxlsx::read.xlsx(xlsx_path, sheet = sheet_use), error = function(e) NULL)
  if (is.null(df) || !"UT" %in% names(df)) return(character(0))
  uts <- unique(trimws(as.character(df$UT)))
  uts <- uts[nzchar(uts)]
  uts
}

get_text <- function(x) ifelse(is.na(x), "", as.character(x))

clean_text <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x[x %in% c("", "NA", "N/A", "na", "n/a", "Unknown", "unknown")] <- NA_character_
  x
}

normalize_country <- function(x) {
  x2 <- clean_text(x)
  if (all(is.na(x2))) return(x2)
  
  x2 <- ifelse(
    is.na(x2),
    NA_character_,
    gsub(
      "Taiwan|Hong Kong|Macao|Macau|People's Republic of China|Peoples R China|Mainland China|PR China|P R China",
      "China",
      x2,
      ignore.case = TRUE
    )
  )
  
  recode_map <- c(
    "UNITED STATES" = "United States",
    "U S A" = "United States",
    "USA" = "United States",
    
    "UNITED KINGDOM" = "United Kingdom",
    "UK" = "United Kingdom",
    "ENGLAND" = "United Kingdom",
    "SCOTLAND" = "United Kingdom",
    "WALES" = "United Kingdom",
    "NORTHERN IRELAND" = "United Kingdom",
    
    "PEOPLES R CHINA" = "China",
    "PEOPLE'S REPUBLIC OF CHINA" = "China",
    "PR CHINA" = "China",
    "P R CHINA" = "China",
    "CHINA" = "China",
    
    "REPUBLIC OF KOREA" = "South Korea",
    "KOREA" = "South Korea",
    "SOUTH KOREA" = "South Korea",
    
    "RUSSIAN FEDERATION" = "Russia",
    "IRAN (ISLAMIC REPUBLIC OF)" = "Iran",
    "VIET NAM" = "Vietnam"
  )
  
  key_upper <- toupper(x2)
  mapped <- dplyr::recode(key_upper, !!!recode_map, .default = NA_character_)
  ifelse(!is.na(mapped), mapped, stringr::str_to_title(x2))
}

normalize_journal <- function(x) {
  x <- clean_text(x)
  if (all(is.na(x))) return(x)
  
  y <- stringr::str_to_title(x)
  small_words <- c("A","An","The","And","Or","Of","In","On","For","To","With","At","By")
  for (w in small_words) {
    y <- stringr::str_replace_all(y, paste0("\\b", w, "\\b"), tolower(w))
  }
  y <- stringr::str_replace(y, "^the\\b", "The")
  y <- stringr::str_replace_all(y, "\\bMens\\b", "Men's")
  
  ifelse(
    stringr::str_detect(y, regex("^Journal of Sexual Medicine$", ignore_case = TRUE)),
    "The Journal of Sexual Medicine",
    y
  )
}

# 4) Import WoS plaintext 
files <- list.files(
  path = wos_dir,
  pattern = "^savedrecs.*\\.txt$",
  full.names = TRUE,
  ignore.case = TRUE
)
if (length(files) == 0) stop("No savedrecs*.txt found under: ", wos_dir)
files <- sort(files)

repair_wos_plaintext <- function(infile, outfile) {
  x <- readLines(infile, warn = FALSE, encoding = "UTF-8")
  if (length(x) == 0) stop("Empty file: ", infile)
  x[1] <- sub("^\ufeff+", "", x[1])
  x <- x[trimws(x) != "EF"]
  while (length(x) > 0 && trimws(tail(x, 1)) == "") x <- x[-length(x)]
  x <- c(x, "", "EF")
  writeLines(x, outfile, useBytes = TRUE)
}

files_repaired <- file.path(repair_dir, basename(files))
for (i in seq_along(files)) repair_wos_plaintext(files[i], files_repaired[i])

M_raw <- bibliometrix::convert2df(
  file = files_repaired,
  dbsource = "wos",
  format = "plaintext",
  remove.duplicates = FALSE
)
message("Imported records: ", nrow(M_raw))

# 5) Inclusion (2015–2025) + optional UT exclusion + dedup

M <- M_raw

if ("PY" %in% names(M)) M$PY <- suppressWarnings(as.integer(M$PY))
if (any(is.na(M$PY)) && "DA" %in% names(M)) {
  idx_na <- which(is.na(M$PY) & !is.na(M$DA) & nchar(M$DA) >= 4)
  M$PY[idx_na] <- suppressWarnings(as.integer(substr(M$DA[idx_na], 1, 4)))
}

years_use <- YEAR_START:YEAR_END
M <- M %>% dplyr::filter(!is.na(PY) & PY %in% years_use)
message("After year filter: ", nrow(M))

exclude_uts <- read_exclusion_uts(exclude_xlsx)
if (length(exclude_uts) > 0 && "UT" %in% names(M)) {
  before <- nrow(M)
  M <- M %>% dplyr::filter(!(UT %in% exclude_uts))
  message("Applied manual exclusions (UT): removed ", before - nrow(M))
  write.csv(
    data.frame(Excluded_UT = exclude_uts),
    file.path(table_dir, "AppliedExclusions_UT_List.csv"),
    row.names = FALSE, fileEncoding = "UTF-8"
  )
} else {
  message("No exclusion list applied (file missing or UT not available).")
}


M$orig_id <- seq_len(nrow(M))
if (!"TC" %in% names(M)) M$TC <- 0
M$TC <- suppressWarnings(as.integer(M$TC))
M$TC[is.na(M$TC)] <- 0L

M$score <- 0
for (fld in c("CR","AB","DE","ID")) {
  if (fld %in% names(M)) M$score <- M$score + nchar(get_text(M[[fld]]))
}

M$doi_clean <- clean_doi(M$DI)
M$has_doi <- M$doi_clean != ""

M_with_doi <- M %>%
  dplyr::filter(has_doi) %>%
  dplyr::group_by(doi_clean) %>%
  dplyr::arrange(dplyr::desc(score), dplyr::desc(TC), .by_group = TRUE) %>%
  dplyr::mutate(dup_rank = dplyr::row_number()) %>%
  dplyr::ungroup()

drop_doi_ids <- M_with_doi %>% dplyr::filter(dup_rank > 1) %>% dplyr::pull(orig_id)

M_no_doi <- M %>% dplyr::filter(!has_doi)
title2 <- norm_title(M_no_doi$TI)
fa2 <- tolower(vapply(M_no_doi$AU, get_first_author, FUN.VALUE = character(1)))
so2 <- tolower(get_text(M_no_doi$SO))
M_no_doi$key_all <- paste(title2, fa2, M_no_doi$PY, so2, sep = "_")

M_no_doi_ranked <- M_no_doi %>%
  dplyr::group_by(key_all) %>%
  dplyr::arrange(dplyr::desc(score), dplyr::desc(TC), .by_group = TRUE) %>%
  dplyr::mutate(dup_rank = dplyr::row_number()) %>%
  dplyr::ungroup()

drop_key_ids <- M_no_doi_ranked %>% dplyr::filter(dup_rank > 1) %>% dplyr::pull(orig_id)

drop_ids <- unique(c(drop_doi_ids, drop_key_ids))
if (length(drop_ids) > 0) {
  before <- nrow(M)
  M <- M %>% dplyr::filter(!(orig_id %in% drop_ids))
  message("Dedup removed: ", before - nrow(M))
} else {
  message("Dedup removed: 0")
}

M <- M %>% dplyr::arrange(PY, TI) %>% dplyr::mutate(pub_id = dplyr::row_number())
message("Final included records: ", nrow(M))

# 6) Build text corpora (metadata only)
M$TI2 <- tolower(get_text(M$TI))
M$AB2 <- tolower(get_text(M$AB))
M$DE2 <- tolower(get_text(M$DE))
M$ID2 <- tolower(get_text(M$ID))

M$ALL_FULL <- paste(M$TI2, M$AB2, M$DE2, M$ID2, sep = " ")
M$ALL_TIAB <- paste(M$TI2, M$AB2, sep = " ")
M$ALL2 <- M$ALL_FULL


# 7) Device extraction + generator type (metadata only)
device_dict <- tibble::tribble(
  ~pattern_regex, ~device_model, ~manufacturer, ~generator_type, ~device_class,
  
  "sd1\\s*t-?top|sd1\\s*ttop",                "DUOLITH SD1 T-TOP",         "STORZ Medical", "Unknown", "Focused",
  "duolith\\s*sd1(\\b|\\s)",                  "DUOLITH SD1",               "STORZ Medical", "Unknown", "Focused",
  "\\bmasterpuls\\b",                         "Masterpuls",                "STORZ Medical", "Pneumatic/ballistic", "Radial",
  "\\bduolith\\b",                            "DUOLITH (brand mentioned)", "STORZ Medical", "Unknown", "Unspecified",
  "\\bstorz\\s*-?\\s*medical\\b|\\bstorz\\b",  "STORZ Medical (brand mentioned)", "STORZ Medical", "Unknown", "Unspecified",
  
  "\\bed[-\\s]*1000\\b",                      "ED1000",                    "Medispec",      "Unknown", "Focused",
  "\\bomnispec\\b",                           "Omnispec",                  "Medispec",      "Unknown", "Unspecified",
  "\\bmedispec\\b",                           "Medispec (brand mentioned)","Medispec",      "Unknown", "Unspecified",
  
  "\\brenova\\b|\\bmorenova\\b",              "Renova/Morenova",           "Direx",         "Unknown", "Focused",
  "\\bdirex\\b",                              "Direx (brand mentioned)",   "Direx",         "Unknown", "Unspecified",
  
  "\\bpiezowave\\b|\\bpiezowave\\s*2\\b",     "PiezoWave",                 "Richard Wolf / ELvation", "Piezoelectric", "Focused",
  "\\brichard\\s+wolf\\b|\\belvation\\b",     "Richard Wolf/ELvation (brand mentioned)", "Richard Wolf / ELvation", "Unknown", "Unspecified",
  
  "dolorclast|\\bswiss\\s*dolorclast\\b",     "Swiss DolorClast",          "EMS",           "Pneumatic/ballistic", "Radial",
  "\\bems\\b",                                "EMS (brand mentioned)",     "EMS",           "Unknown", "Unspecified",
  
  "btl[-\\s]*6000|btl\\s*6000",               "BTL 6000",                  "BTL",           "Pneumatic/ballistic", "Radial",
  "\\bbtl\\b",                                "BTL (brand mentioned)",     "BTL",           "Unknown", "Unspecified",
  
  "\\bgainswave\\b",                          "GAINSWave (protocol/brand)","GAINSWave",     "Pneumatic/ballistic", "Radial",
  
  "\\baries\\s*2\\b",                         "ARIES 2",                   "Dornier MedTech", "Electromagnetic", "Focused",
  "\\baries\\b|\\baries\\s*ii\\b",            "ARIES",                     "Dornier MedTech", "Electromagnetic", "Focused",
  "\\bdornier\\b",                            "Dornier (brand mentioned)", "Dornier MedTech", "Unknown", "Unspecified"
)

gen_text_dict <- tibble::tribble(
  ~pattern_regex, ~generator_type, ~device_class,
  "electrohydraulic|spark\\s*gap",                   "Electrohydraulic", "Focused",
  "electromagnetic",                                 "Electromagnetic",  "Focused",
  "piezoelectric|piezo\\s*electric",                 "Piezoelectric",     "Focused",
  "pneumatic|ballistic|radial\\s+pressure\\s+wave|\\brpw\\b|\\brswt\\b", "Pneumatic/ballistic", "Radial"
)

extract_from_dict <- function(text_vec, dict_df) {
  out_model <- character(length(text_vec))
  out_manu  <- character(length(text_vec))
  out_gen   <- character(length(text_vec))
  out_class <- character(length(text_vec))
  out_ev    <- character(length(text_vec))
  
  for (i in seq_along(text_vec)) {
    txt <- text_vec[i]
    if (is.na(txt) || txt == "") {
      out_model[i] <- ""; out_manu[i] <- ""; out_gen[i] <- ""; out_class[i] <- ""; out_ev[i] <- ""
      next
    }
    
    hit <- stringr::str_detect(txt, dict_df$pattern_regex)
    if (!any(hit)) {
      out_model[i] <- ""; out_manu[i] <- ""; out_gen[i] <- ""; out_class[i] <- ""; out_ev[i] <- ""
      next
    }
    
    dfh <- dict_df[hit, , drop = FALSE]
    
    if (all(c("device_model","manufacturer") %in% names(dfh))) {
      is_generic <- grepl("brand mentioned", dfh$device_model, ignore.case = TRUE)
      if (any(!is_generic) && any(is_generic)) {
        manu_specific <- unique(dfh$manufacturer[!is_generic])
        dfh <- dfh[!(is_generic & dfh$manufacturer %in% manu_specific), , drop = FALSE]
      }
    }
    
    if ("device_model" %in% names(dfh)) out_model[i] <- paste(unique(dfh$device_model), collapse = "; ")
    if ("manufacturer" %in% names(dfh)) out_manu[i]  <- paste(unique(dfh$manufacturer), collapse = "; ")
    out_gen[i] <- paste(unique(dfh$generator_type), collapse = "; ")
    
    cls <- unique(dfh$device_class); cls <- cls[cls != ""]
    if (length(cls) == 0) out_class[i] <- ""
    else if (any(cls == "Mixed")) out_class[i] <- "Mixed"
    else if (any(cls == "Focused") && any(cls == "Radial")) out_class[i] <- "Mixed"
    else if (any(cls == "Focused")) out_class[i] <- "Focused"
    else if (any(cls == "Radial")) out_class[i] <- "Radial"
    else out_class[i] <- "Unspecified"
    
    out_ev[i] <- paste(unique(dfh$pattern_regex), collapse = " | ")
  }
  
  list(
    device_model = out_model,
    manufacturer = out_manu,
    generator_type = out_gen,
    device_class = out_class,
    evidence = out_ev
  )
}

dev_out <- extract_from_dict(M$ALL2, device_dict)
M$DEVICE_MODEL       <- dev_out$device_model
M$DEVICE_MFG         <- dev_out$manufacturer
M$GENERATOR_TYPE_DEV <- dev_out$generator_type
M$DEVICE_CLASS_DEV   <- dev_out$device_class
M$DEVICE_EVIDENCE    <- dev_out$evidence

gen_out <- extract_from_dict(M$ALL2, gen_text_dict)
M$GENERATOR_TYPE_TEXT <- gen_out$generator_type
M$DEVICE_CLASS_TEXT   <- gen_out$device_class

M$GENERATOR_TYPE_FINAL <- dplyr::case_when(
  nzchar(M$GENERATOR_TYPE_DEV)  ~ M$GENERATOR_TYPE_DEV,
  nzchar(M$GENERATOR_TYPE_TEXT) ~ M$GENERATOR_TYPE_TEXT,
  TRUE ~ ""
)

M$DEVICE_CLASS_FINAL <- dplyr::case_when(
  nzchar(M$DEVICE_CLASS_DEV)  ~ M$DEVICE_CLASS_DEV,
  nzchar(M$DEVICE_CLASS_TEXT) ~ M$DEVICE_CLASS_TEXT,
  TRUE ~ "Unspecified"
)


# 8) Modality classification 
shock_ctx <- paste0(
  "(",
  "shock[-\\s]*wave(s)?|shockwave(s)?|",
  "extracorporeal\\s+shock[-\\s]*wave(s)?|extracorporeal\\s+shockwave(s)?|",
  "eswt|swt|li[-\\s]*eswt|li[-\\s]*swt|lieswt|liswt",
  ")"
)

pat_radial_reported <- paste0(
  "(",
  "\\bradial\\b\\s*(pressure\\s*)?wave\\b|",
  "\\bradial\\b\\s*(shock[-\\s]*wave(s)?|shockwave(s)?)\\b|",
  "\\b(rpw|rpwt|rswt|rwt)\\b|",
  "\\bradial\\s*type\\b",
  ")"
)

pat_focused_reported <- paste0(
  "(",
  "\\bf[-\\s]*eswt\\b|\\bf[-\\s]*swt\\b|\\bfswt\\b|\\bl[-\\s]*fswt\\b|",
  "\\b(focused|focussed)\\b(?!\\s+on\\b)",
  "(?:\\s+(?:linear|focal|low[-\\s]*intensity|low[-\\s]*energy|extracorporeal|therapy|treatment|trial|generator|source)){0,4}",
  "\\s*", shock_ctx, "\\b|",
  shock_ctx, "\\s*\\(\\s*(focused|focussed)\\b|",
  shock_ctx, "[-\\s]*(focused|focussed)\\b|",
  "\\bfocused\\s*type\\b",
  ")"
)

pat_mixed_reported <- paste0(
  "(",
  "\\bmixed\\b\\s*(modality|type)?\\b|",
  "multi[-\\s]*type\\b|",
  "both\\s+focused\\s+and\\s+radial|focused\\s+and\\s+radial|radial\\s+and\\s+focused",
  ")"
)

pat_eswt_generic <- paste0(
  "(",
  "\\bli\\s*-?\\s*eswt\\b|\\bli\\s*-?\\s*swt\\b|\\blieswt\\b|\\bliswt\\b|",
  "\\beswt\\b|\\bswt\\b|",
  "extracorporeal\\s+shock[-\\s]*wave|shock[-\\s]*wave|shockwave|acoustic\\s+wave",
  ")"
)

flag_radial_reported  <- stringr::str_detect(M$ALL2, pat_radial_reported)
flag_focused_reported <- stringr::str_detect(M$ALL2, pat_focused_reported)
flag_mixed_reported   <- stringr::str_detect(M$ALL2, pat_mixed_reported)
flag_eswt_generic     <- stringr::str_detect(M$ALL2, pat_eswt_generic)

M$THERAPY_TEXT5 <- dplyr::case_when(
  flag_mixed_reported ~ "Mixed_or_Comparative",
  flag_radial_reported & flag_focused_reported ~ "Mixed_or_Comparative",
  flag_radial_reported  ~ "Radial",
  flag_focused_reported ~ "Focused",
  flag_eswt_generic     ~ "ESWT_unspecified",
  TRUE ~ "Unclassified"
)

M$THERAPY_CLASS5 <- dplyr::case_when(
  M$DEVICE_CLASS_FINAL == "Mixed"    ~ "Mixed_or_Comparative",
  M$DEVICE_CLASS_FINAL == "Focused"  ~ "Focused",
  M$DEVICE_CLASS_FINAL == "Radial"   ~ "Radial",
  TRUE ~ M$THERAPY_TEXT5
)

class5_levels_stack <- c("ESWT_unspecified","Mixed_or_Comparative","Radial","Focused","Unclassified")
M$THERAPY_CLASS5 <- factor(as.character(M$THERAPY_CLASS5), levels = class5_levels_stack)

class5_levels_legend <- c("Focused","Radial","Mixed_or_Comparative","ESWT_unspecified","Unclassified")
class5_labels <- c(
  "Focused"              = "Focused",
  "Radial"               = "Radial pressure wave",
  "Mixed_or_Comparative" = "Mixed",
  "ESWT_unspecified"     = "Modality not stated",
  "Unclassified"         = "Unclassified"
)
class5_colors <- c(
  "Focused"              = COL_FOCUSED,
  "Radial"               = COL_RADIAL,
  "Mixed_or_Comparative" = COL_MIXED,
  "ESWT_unspecified"     = COL_UNSPEC,
  "Unclassified"         = COL_GRAY
)

# Explicit modality 
M$MODALITY_EXPLICIT4 <- dplyr::case_when(
  M$THERAPY_TEXT5 == "Focused"              ~ "Focused",
  M$THERAPY_TEXT5 == "Radial"               ~ "Radial pressure wave",
  M$THERAPY_TEXT5 == "Mixed_or_Comparative" ~ "Mixed",
  TRUE ~ "Modality not stated"
)
mod_levels <- c("Focused","Radial pressure wave","Mixed","Modality not stated")
M$MODALITY_EXPLICIT4 <- factor(M$MODALITY_EXPLICIT4, levels = mod_levels)


# 9) Evidence type classification
detect_evidence_v2 <- function(dt, ti, ab, all2 = "") {
  dt2  <- tolower(ifelse(is.na(dt), "", dt))
  ti2  <- tolower(ifelse(is.na(ti), "", ti))
  ab2  <- tolower(ifelse(is.na(ab), "", ab))
  all2 <- tolower(ifelse(is.na(all2), "", all2))
  
  tx <- paste(ti2, ab2, sep = " ")
  if (nchar(gsub("\\s+", "", tx)) < 10) tx <- all2
  
  meta_term <- grepl(
    "meta\\s*-?\\s*analysis|metaanalysis|network\\s+meta\\s*-?\\s*analysis|meta\\s*-?\\s*regression",
    tx
  )
  meta_action <- grepl(
    "(we|this\\s+study)\\s+(conducted|performed|carried\\s+out)\\s+(a\\s+)?(systematic\\s+review\\s+and\\s+)?meta\\s*-?\\s*analysis",
    tx
  ) | grepl("\\bpooled\\b|random\\s*-?effects|fixed\\s*-?effects|heterogeneity\\s*(i\\^?2|i2)|forest\\s+plot", tx)
  meta_in_title <- grepl("meta\\s*-?\\s*analysis|metaanalysis", ti2)
  
  is_meta <- meta_term & (meta_action | meta_in_title)
  
  is_sr <- grepl("systematic\\s+(review|literature\\s+review)|scoping\\s+review|umbrella\\s+review|evidence\\s+synthesis", tx)
  
  is_rev <- grepl("\\breview\\b", dt2) | grepl("\\bnarrative\\s+review\\b|\\boverview\\b", tx)
  
  rct_core <- grepl("randomi[sz]ed|randomi[sz]ation|\\brandomly\\b", tx)
  rct_design <- grepl(
    "\\brandomi[sz]ed\\s+controlled\\s+trial\\b|\\bdouble\\s*-?\\s*blind\\b|\\bsingle\\s*-?\\s*blind\\b|\\bsham\\s*-?\\s*controlled\\b|\\bplacebo\\s*-?\\s*controlled\\b|\\bparallel\\s+group\\b",
    tx
  )
  rct_trial_word <- grepl("\\btrial\\b|\\bpatients?\\s+were\\s+randomi[sz]ed\\b|\\ballocated\\b", tx)
  rct_excl <- grepl("protocol|trial\\s+protocol|study\\s+protocol|design\\s+of\\s+a\\s+trial|retrospective|observational|cohort|case\\s+series|cross\\s*-?sectional", tx)
  
  is_rct <- (rct_design | (rct_core & rct_trial_word)) & !rct_excl
  
  if (is_meta) return("Meta-analysis")
  if (is_sr)   return("Systematic review")
  if (is_rev)  return("Review (non-systematic)")
  if (is_rct)  return("Randomized controlled trial")
  if (grepl("article", dt2)) return("Original article (non-RCT/unspecified)")
  if (dt2 == "" || dt2 == "unknown") return("Unknown")
  "Other"
}
ev_levels <- c(
  "Review (non-systematic)",
  "Meta-analysis",
  "Systematic review",
  "Original article (non-RCT/unspecified)",
  "Randomized controlled trial",
  "Other",
  "Unknown"
)

ev_colors <- c(
  "Randomized controlled trial"            = "#4E79A7",
  "Original article (non-RCT/unspecified)" = "#59A14F",
  "Systematic review"                      = "#F28E2B",
  "Meta-analysis"                          = "#E15759",
  "Review (non-systematic)"                = "#B07AA1",
  "Other"                                  = "grey70",
  "Unknown"                                = "grey85"
)

DT_vec   <- if ("DT"   %in% names(M)) M$DT   else rep(NA_character_, nrow(M))
TI_vec   <- if ("TI"   %in% names(M)) M$TI   else rep(NA_character_, nrow(M))
AB_vec   <- if ("AB"   %in% names(M)) M$AB   else rep(NA_character_, nrow(M))
ALL2_vec <- if ("ALL2" %in% names(M)) M$ALL2 else rep("", nrow(M))

M$EVIDENCE_TYPE <- mapply(
  detect_evidence_v2,
  DT_vec, TI_vec, AB_vec, ALL2_vec,
  USE.NAMES = FALSE
)
M$EVIDENCE_TYPE <- factor(M$EVIDENCE_TYPE, levels = ev_levels)

print(table(M$EVIDENCE_TYPE))

# 10) Reporting completeness
is_generator_informative <- function(x) {
  x2 <- stringr::str_squish(tolower(get_text(x)))
  if (!nzchar(x2)) return(FALSE)
  if (grepl("^unknown$", x2)) return(FALSE)
  grepl("electrohydraulic|electromagnetic|piezoelectric|pneumatic|ballistic|spark\\s*gap", x2)
}

M$FLAG_TYPE_REPORTED      <- M$THERAPY_TEXT5 %in% c("Focused","Radial","Mixed_or_Comparative")
M$FLAG_DEVICE_REPORTED    <- nzchar(stringr::str_squish(get_text(M$DEVICE_MODEL)))
M$FLAG_GENERATOR_REPORTED <- vapply(M$GENERATOR_TYPE_FINAL, is_generator_informative, logical(1))

M$REPORTING_SCORE_0_3 <- as.integer(M$FLAG_TYPE_REPORTED) +
  as.integer(M$FLAG_DEVICE_REPORTED) +
  as.integer(M$FLAG_GENERATOR_REPORTED)

# 11) TABLE 1: Dataset overview
df_author_long <- M %>%
  dplyr::mutate(pub_id = pub_id, AU_raw = clean_text(AU)) %>%
  dplyr::filter(!is.na(AU_raw) & AU_raw != "") %>%
  tidyr::separate_rows(AU_raw, sep = ";") %>%
  dplyr::mutate(Author = toupper(stringr::str_squish(AU_raw))) %>%
  dplyr::filter(!is.na(Author) & Author != "") %>%
  dplyr::distinct(pub_id, Author)
n_authors <- dplyr::n_distinct(df_author_long$Author)


n_institutions <- NA_integer_
if ("AU_UN" %in% names(M)) {
  df_inst_long <- M %>%
    dplyr::mutate(pub_id = pub_id, AU_UN_raw = clean_text(AU_UN)) %>%
    dplyr::filter(!is.na(AU_UN_raw) & AU_UN_raw != "") %>%
    tidyr::separate_rows(AU_UN_raw, sep = ";") %>%
    dplyr::mutate(Institution = toupper(stringr::str_squish(AU_UN_raw))) %>%
    dplyr::distinct(pub_id, Institution)
  n_institutions <- dplyr::n_distinct(df_inst_long$Institution)
} else if ("C1" %in% names(M)) {
  df_inst_long <- M %>%
    dplyr::mutate(pub_id = pub_id, C1_raw = clean_text(C1)) %>%
    dplyr::filter(!is.na(C1_raw) & C1_raw != "") %>%
    tidyr::separate_rows(C1_raw, sep = ";") %>%
    dplyr::mutate(Institution = toupper(stringr::str_squish(stringr::str_extract(C1_raw, "^[^,]+")))) %>%
    dplyr::filter(!is.na(Institution) & Institution != "") %>%
    dplyr::distinct(pub_id, Institution)
  n_institutions <- dplyr::n_distinct(df_inst_long$Institution)
}

n_countries <- NA_integer_
if (!"AU_CO" %in% names(M)) {
  if ("C1" %in% names(M)) {
    M <- bibliometrix::metaTagExtraction(M, Field = "AU_CO", sep = ";")
  }
}
if ("AU_CO" %in% names(M)) {
  df_country_long <- M %>%
    dplyr::mutate(pub_id = pub_id, AU_CO_raw = clean_text(AU_CO)) %>%
    dplyr::filter(!is.na(AU_CO_raw) & AU_CO_raw != "") %>%
    tidyr::separate_rows(AU_CO_raw, sep = ";") %>%
    dplyr::mutate(Country = stringr::str_to_title(stringr::str_squish(AU_CO_raw))) %>%
    dplyr::filter(!is.na(Country) & Country != "") %>%
    dplyr::distinct(pub_id, Country)
  n_countries <- dplyr::n_distinct(df_country_long$Country)
}

n_journals <- if ("SO" %in% names(M)) dplyr::n_distinct(toupper(clean_text(M$SO))) else NA_integer_

n_articles <- NA_integer_
n_reviews  <- NA_integer_
if ("DT" %in% names(M)) {
  dt2 <- stringr::str_to_lower(get_text(M$DT))
  n_reviews  <- sum(stringr::str_detect(dt2, "review"))
  n_articles <- sum(stringr::str_detect(dt2, "article") & !stringr::str_detect(dt2, "review"))
}

tc <- suppressWarnings(as.numeric(M$TC)); tc[is.na(tc)] <- 0
total_cit  <- sum(tc)
mean_cit   <- round(mean(tc), 1)
median_cit <- stats::median(tc)

Table1_DatasetOverview <- tibble::tibble(
  Item = c(
    "Search period",
    "Publication year range",
    "Publications, n",
    "Document types",
    "Journals, n",
    "Authors, n",
    "Institutions, n",
    "Countries/regions, n",
    "Total citations, n",
    "Mean citations per paper",
    "Median citations per paper"
  ),
  Value = c(
    paste0(YEAR_START, "-01-01 to ", YEAR_END, "-12-31"),
    paste0(YEAR_START, "–", YEAR_END),
    nrow(M),
    paste0("Article (", n_articles, "); Review (", n_reviews, ")"),
    n_journals,
    n_authors,
    n_institutions,
    n_countries,
    format(total_cit, big.mark = ","),
    mean_cit,
    median_cit
  )
)

write.csv(
  Table1_DatasetOverview,
  file.path(table_dir, "Table_1_DatasetOverview.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)


# 12) TABLE 2 : Evidence structure by explicit modality
M$MODALITY_EXPLICIT4 <- dplyr::case_when(
  M$THERAPY_TEXT5 == "Focused"              ~ "Focused",
  M$THERAPY_TEXT5 == "Radial"               ~ "Radial pressure wave",
  M$THERAPY_TEXT5 == "Mixed_or_Comparative" ~ "Mixed",
  TRUE                                     ~ "Modality not stated"
)
mod_levels_tbl2 <- c("Focused", "Radial pressure wave", "Mixed", "Modality not stated")
M$MODALITY_EXPLICIT4 <- factor(M$MODALITY_EXPLICIT4, levels = mod_levels_tbl2)
ev_levels_tbl2 <- c(
  "Randomized controlled trial",
  "Original article (non-RCT/unspecified)",
  "Systematic review",
  "Meta-analysis",
  "Review (non-systematic)"
)

df_tbl2 <- M %>%
  dplyr::mutate(
    EVIDENCE_TYPE      = as.character(EVIDENCE_TYPE),
    MODALITY_EXPLICIT4 = as.character(MODALITY_EXPLICIT4)
  ) %>%
  dplyr::filter(EVIDENCE_TYPE %in% ev_levels_tbl2) %>%
  dplyr::mutate(
    EVIDENCE_TYPE      = factor(EVIDENCE_TYPE, levels = ev_levels_tbl2),
    MODALITY_EXPLICIT4 = factor(MODALITY_EXPLICIT4, levels = mod_levels_tbl2)
  )
tab_counts <- df_tbl2 %>%
  dplyr::count(EVIDENCE_TYPE, MODALITY_EXPLICIT4, name = "n") %>%
  tidyr::complete(
    EVIDENCE_TYPE      = ev_levels_tbl2,
    MODALITY_EXPLICIT4 = mod_levels_tbl2,
    fill = list(n = 0)
  )

Table2_ColumnN <- tab_counts %>%
  dplyr::group_by(MODALITY_EXPLICIT4) %>%
  dplyr::summarise(N = sum(n), .groups = "drop") %>%
  tidyr::complete(MODALITY_EXPLICIT4 = mod_levels_tbl2, fill = list(N = 0)) %>%
  dplyr::mutate(MODALITY_EXPLICIT4 = factor(MODALITY_EXPLICIT4, levels = mod_levels_tbl2)) %>%
  dplyr::arrange(MODALITY_EXPLICIT4)
tab_long <- tab_counts %>%
  dplyr::left_join(Table2_ColumnN, by = "MODALITY_EXPLICIT4") %>%
  dplyr::mutate(
    pct  = dplyr::if_else(N > 0, 100 * n / N, 0),
    cell = paste0(n, " (", sprintf("%.0f", pct), "%)")
  )
col_label_map <- setNames(
  paste0(as.character(Table2_ColumnN$MODALITY_EXPLICIT4), " (n=", Table2_ColumnN$N, ")"),
  as.character(Table2_ColumnN$MODALITY_EXPLICIT4)
)

Table2_EvidenceStructure_Explicit <- tab_long %>%
  dplyr::select(EVIDENCE_TYPE, MODALITY_EXPLICIT4, cell) %>%
  tidyr::pivot_wider(
    names_from  = MODALITY_EXPLICIT4,
    values_from = cell,
    values_fill = list(cell = "0 (0%)")
  ) %>%
  dplyr::mutate(EVIDENCE_TYPE = factor(as.character(EVIDENCE_TYPE), levels = ev_levels_tbl2)) %>%
  dplyr::arrange(EVIDENCE_TYPE) %>%
  dplyr::select(EVIDENCE_TYPE, all_of(mod_levels_tbl2)) %>%
  dplyr::rename_with(~ col_label_map[.x], .cols = all_of(mod_levels_tbl2)) %>%
  dplyr::rename(`Evidence type` = EVIDENCE_TYPE)

write.csv(
  Table2_ColumnN,
  file.path(table_dir, "Table_2_ColumnN.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)
write.csv(
  Table2_EvidenceStructure_Explicit,
  file.path(table_dir, "Table_2_EvidenceStructure_Explicit.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)


# 13) Figure 2 
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(grid)
  library(patchwork)
})

stopifnot(exists("M"))
stopifnot(all(c("PY","THERAPY_CLASS5") %in% names(M)))
stopifnot(exists("theme_jsm"), exists("save_plot_dual"))
stopifnot(exists("COL_FOCUSED"))

M_fig2 <- M %>%
  mutate(
    PY = as.integer(PY),
    THERAPY_CLASS4 = dplyr::case_when(
      as.character(THERAPY_CLASS5) == "Focused"              ~ "Focused",
      as.character(THERAPY_CLASS5) == "Radial"               ~ "Radial pressure wave",
      as.character(THERAPY_CLASS5) == "Mixed_or_Comparative" ~ "Mixed",
      TRUE                                                   ~ "Type unspecified"
    )
  )

if (!exists("YEAR_START")) YEAR_START <- min(M_fig2$PY, na.rm = TRUE)
if (!exists("YEAR_END"))   YEAR_END   <- max(M_fig2$PY, na.rm = TRUE)


# Panel A
annual_prod_A <- M_fig2 %>%
  count(PY, name = "Publications") %>%
  complete(PY = YEAR_START:YEAR_END, fill = list(Publications = 0)) %>%
  arrange(PY)

p1 <- ggplot(annual_prod_A, aes(x = PY, y = Publications)) +
  geom_col(fill = COL_FOCUSED, width = 0.85) +
  geom_text(aes(label = Publications),
            vjust = -0.35, size = 3.4, family = FONT_FAMILY) +
  scale_x_continuous(breaks = YEAR_START:YEAR_END) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  labs(x = "Year", y = "Publications") +
  theme_jsm(base_size = 12, legend_pos = "none") +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Panel B
class4_levels_legend <- c("Focused","Radial pressure wave","Mixed","Type unspecified")

class4_levels_stack <- rev(class4_levels_legend)

class4_colors <- c(
  "Focused"              = "#2C7FB8",
  "Radial pressure wave" = "#41B6C4",
  "Mixed"                = "#E76F51",
  "Type unspecified"     = "#D9D9D9"
)

M_fig2 <- M_fig2 %>%
  mutate(THERAPY_CLASS4 = factor(THERAPY_CLASS4, levels = class4_levels_stack))

df_year_type_B <- M_fig2 %>%
  count(PY, THERAPY_CLASS4, name = "Publications") %>%
  complete(
    PY = YEAR_START:YEAR_END,
    THERAPY_CLASS4 = class4_levels_stack,
    fill = list(Publications = 0)
  ) %>%
  mutate(THERAPY_CLASS4 = factor(as.character(THERAPY_CLASS4), levels = class4_levels_stack)) %>%
  arrange(PY, THERAPY_CLASS4)

df_year_total_B <- df_year_type_B %>%
  group_by(PY) %>%
  summarise(Total = sum(Publications), .groups = "drop")

p2 <- ggplot(df_year_type_B, aes(x = PY, y = Publications, fill = THERAPY_CLASS4)) +
  geom_col(
    width = 0.85,
    color = "white",
    linewidth = 0.25
  ) +
  geom_text(
    data = df_year_total_B,
    aes(x = PY, y = Total, label = paste0("n = ", Total)),
    inherit.aes = FALSE,
    vjust = -0.55, size = 3.5, family = FONT_FAMILY
  ) +
  scale_fill_manual(
    values = class4_colors,
    breaks = class4_levels_legend,   
    limits = class4_levels_stack,    
    drop = FALSE
  ) +
  scale_x_continuous(breaks = YEAR_START:YEAR_END) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(x = "Year", y = "Publications", fill = NULL) +
  theme_jsm(base_size = 12, legend_pos = "top") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(
    legend.position      = "top",
    legend.direction     = "horizontal",
    legend.box           = "horizontal",
    legend.box.just      = "center",
    legend.justification = "center",
    legend.box.margin    = margin(t = 0, r = 0, b = -6, l = 0),
    legend.spacing.x     = unit(0.35, "cm"),
    legend.key.width     = unit(0.85, "cm"),
    legend.key.height    = unit(0.35, "cm"),
    legend.text          = element_text(size = 11, lineheight = 1.0)
  )

# Combine
p1_panel <- p1 +
  labs(x = NULL) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin  = margin(t = 10, r = 14, b = 2, l = 12)
  )

p2_panel <- p2 +
  theme(plot.margin = margin(t = 2, r = 14, b = 10, l = 12))

Figure_2 <- (p1_panel / patchwork::plot_spacer() / p2_panel) +
  plot_layout(heights = c(1.0, 0.10, 1.35)) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = "bold", size = 14),
    plot.tag.position = c(0.01, 0.99)
  )

save_plot_dual(Figure_2, "Figure_2", width = 7.2, height = 9.0)


# 14) FIGURE_3
COL_SCP <- "#9ECAE1"
COL_MCP <- "#2171B5"

build_country_scp_mcp <- function(df, top_n = 10) {
  
  id_col <- dplyr::case_when(
    "SR" %in% names(df) ~ "SR",
    "UT" %in% names(df) ~ "UT",
    TRUE ~ NA_character_
  )
  
  df0 <- df %>%
    dplyr::mutate(.paper_id = if (!is.na(id_col)) .data[[id_col]] else dplyr::row_number())
  
  if (!("AU_CO" %in% names(df0))) {
    stop
  }
  
  country_long <- df0 %>%
    dplyr::select(.paper_id, AU_CO) %>%
    dplyr::mutate(AU_CO = clean_text(AU_CO)) %>%
    tidyr::separate_rows(AU_CO, sep = ";") %>%
    dplyr::mutate(
      AU_CO = clean_text(AU_CO),
      Country = normalize_country(AU_CO)
    ) %>%
    dplyr::filter(!is.na(Country) & Country != "") %>%
    dplyr::distinct(.paper_id, Country)
  
  paper_n <- country_long %>%
    dplyr::group_by(.paper_id) %>%
    dplyr::summarise(n_country = dplyr::n_distinct(Country), .groups = "drop")
  
  country_long2 <- country_long %>%
    dplyr::left_join(paper_n, by = ".paper_id") %>%
    dplyr::mutate(PubType = dplyr::if_else(n_country == 1, "SCP", "MCP"))
  
  country_wide <- country_long2 %>%
    dplyr::count(Country, PubType, name = "n") %>%
    tidyr::complete(Country, PubType, fill = list(n = 0)) %>%
    tidyr::pivot_wider(names_from = PubType, values_from = n, values_fill = 0) %>%
    dplyr::mutate(
      Total = SCP + MCP,
      MCP_pct = dplyr::if_else(Total > 0, MCP / Total, 0)
    ) %>%
    dplyr::arrange(dplyr::desc(Total)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(Country = forcats::fct_reorder(Country, Total))
  
  country_long_plot <- country_wide %>%
    tidyr::pivot_longer(c(SCP, MCP), names_to = "PubType", values_to = "n") %>%
    dplyr::mutate(PubType = factor(PubType, levels = c("SCP", "MCP")))
  
  list(country_wide = country_wide, country_long = country_long_plot)
}

tmp_cty <- build_country_scp_mcp(M, top_n = 10)
country_wide <- tmp_cty$country_wide
country_long <- tmp_cty$country_long

mcp_label_df <- country_wide %>%
  dplyr::filter(MCP > 0) %>%
  dplyr::mutate(
    lab = scales::percent(MCP_pct, accuracy = 1),
    x   = SCP + MCP / 2
  ) %>%
  dplyr::select(Country, x, lab)

max_total_A <- max(country_wide$Total, na.rm = TRUE)
x_off_A <- max_total_A * 0.04

tot_label_df <- country_wide %>%
  dplyr::mutate(
    lab = paste0("n = ", Total),
    x   = Total + x_off_A
  ) %>%
  dplyr::select(Country, x, lab)

# Panel A
pA <- ggplot(country_long, aes(x = n, y = Country, fill = PubType)) +
  geom_col(
    width = 0.78,
    color = "white",
    linewidth = 0.4,
    position = position_stack(reverse = TRUE)
  ) +
  geom_text(
    data = mcp_label_df,
    aes(x = x, y = Country, label = lab),
    inherit.aes = FALSE,
    color = "white",
    size = 3.2, family = FONT_FAMILY
  ) +
  geom_text(
    data = tot_label_df,
    aes(x = x, y = Country, label = lab),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.2, family = FONT_FAMILY, color = "grey25"
  ) +
  scale_fill_manual(
    values = c("SCP" = COL_SCP, "MCP" = COL_MCP),
    breaks = c("SCP", "MCP"),
    labels = c(
      "SCP" = "Single-country publications (SCP)",
      "MCP" = "Multiple-country publications (MCP)"
    )
  ) +
  labs(x = "Number of publications", y = "Country", fill = NULL) +
  theme_jsm(base_size = 11, legend_pos = "top") +
  theme(
    panel.grid.major.y = element_blank(),
    legend.direction = "horizontal",
    legend.justification = "center",
    plot.margin = margin(5.5, 45, 5.5, 5.5)
  ) +
  coord_cartesian(xlim = c(0, max_total_A * 1.14), clip = "off") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.02)))

#Panel B
if (!("SO" %in% names(M))) stop
journal_df <- M %>%
  dplyr::mutate(Journal = normalize_journal(SO)) %>%
  dplyr::filter(!is.na(Journal) & Journal != "") %>%
  dplyr::count(Journal, name = "n") %>%
  dplyr::arrange(dplyr::desc(n)) %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::mutate(Journal = forcats::fct_reorder(Journal, n))

max_total_B <- max(journal_df$n, na.rm = TRUE)
x_off_B <- max_total_B * 0.04

pB <- ggplot(journal_df, aes(x = n, y = Journal)) +
  geom_col(width = 0.78, fill = COL_MCP) +
  geom_text(
    aes(x = n + x_off_B, label = paste0("n = ", n)),
    hjust = 0,
    size = 3.2, family = FONT_FAMILY, color = "grey25"
  ) +
  labs(x = "Number of publications", y = "Journal") +
  theme_jsm(base_size = 11, legend_pos = "none") +
  theme(
    panel.grid.major.y = element_blank(),
    plot.margin = margin(5.5, 20, 5.5, 5.5)
  ) +
  coord_cartesian(xlim = c(0, max_total_B * 1.20), clip = "off") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.02)))

# Combine 
Figure_3 <- (pA / pB) +
  patchwork::plot_layout(heights = c(1.05, 1.0)) +
  patchwork::plot_annotation(tag_levels = "A") & panel_tag_theme

save_plot_dual(Figure_3, "Figure_3", width = 10.5, height = 10.5)


# 15) FIGURE_4
M <- M %>% dplyr::mutate(PY = as.integer(PY))

# Panel A
df_rq_year <- M %>%
  dplyr::group_by(PY) %>%
  dplyr::summarise(
    N = dplyr::n(),
    k_type      = sum(FLAG_TYPE_REPORTED,      na.rm = TRUE),
    k_device    = sum(FLAG_DEVICE_REPORTED,    na.rm = TRUE),
    k_generator = sum(FLAG_GENERATOR_REPORTED, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(PY)

df_n_year_rq <- df_rq_year %>% dplyr::filter(N > 0) %>% dplyr::transmute(PY, N)

rq_long_ci <- df_rq_year %>%
  dplyr::filter(N > 0) %>%
  tidyr::pivot_longer(cols = c(k_type, k_device, k_generator), names_to = "Metric_raw", values_to = "k") %>%
  dplyr::mutate(
    Metric = dplyr::case_when(
      Metric_raw == "k_type"      ~ "Type specified (focused/radial/mixed)",
      Metric_raw == "k_device"    ~ "Device model/brand reported",
      Metric_raw == "k_generator" ~ "Generator type reported",
      TRUE ~ Metric_raw
    ),
    Proportion = k / N
  )

ci_mat <- t(mapply(function(k, n) {
  bt <- stats::binom.test(k, n, conf.level = 0.95)
  as.numeric(bt$conf.int)
}, rq_long_ci$k, rq_long_ci$N))

rq_long_ci$ci_low  <- pmax(0, ci_mat[, 1])
rq_long_ci$ci_high <- pmin(1, ci_mat[, 2])

rq_long_ci$Metric <- factor(
  rq_long_ci$Metric,
  levels = c(
    "Type specified (focused/radial/mixed)",
    "Device model/brand reported",
    "Generator type reported"
  )
)

pal_line <- RColorBrewer::brewer.pal(3, "Dark2")
names(pal_line) <- levels(rq_long_ci$Metric)

mix_with_white <- function(col, mix = 0.80) {
  rgb <- grDevices::col2rgb(col) / 255
  rgb2 <- rgb * (1 - mix) + 1 * mix
  grDevices::rgb(rgb2[1], rgb2[2], rgb2[3])
}
pal_fill <- vapply(pal_line, mix_with_white, character(1), mix = 0.80)
names(pal_fill) <- names(pal_line)

lt_map <- c(
  "Type specified (focused/radial/mixed)" = "solid",
  "Device model/brand reported"          = "dashed",
  "Generator type reported"              = "dotdash"
)

pA <- ggplot(rq_long_ci, aes(x = PY, group = Metric)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = Metric), alpha = 0.26, colour = NA, show.legend = FALSE) +
  geom_line(aes(y = Proportion, color = Metric, linetype = Metric), linewidth = 1.10) +
  geom_point(aes(y = Proportion, color = Metric), size = 2.35) +
  geom_text(
    data = df_n_year_rq,
    aes(x = PY, y = 0, label = paste0("n=", N)),
    inherit.aes = FALSE,
    vjust = 2.15,
    color = "grey60",
    size = 3.1,
    family = FONT_FAMILY
  ) +
  scale_x_continuous(breaks = YEAR_START:YEAR_END) +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 1, by = 0.25)) +
  scale_color_manual(values = pal_line) +
  scale_fill_manual(values = pal_fill) +
  scale_linetype_manual(values = lt_map) +
  labs(x = "Year", y = "Proportion", color = NULL, linetype = NULL) +
  theme_jsm(base_size = 12, legend_pos = "top") +
  theme(
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box.just = "center",
    plot.margin = margin(t = 8, r = 12, b = 10, l = 12),
    panel.grid.major.x = element_blank()
  ) +
  guides(linetype = "none", color = guide_legend(nrow = 1, byrow = TRUE)) +
  coord_cartesian(ylim = c(0, 1), clip = "off")

# Panel B 
pal_score <- c(
  "0 (none)" = "#D9D9D9",
  "1"        = "#C6DBEF",
  "2"        = "#6BAED6",
  "3 (all)"  = "#2171B5"
)

df_score_period <- M %>%
  dplyr::mutate(
    SCORE = as.integer(REPORTING_SCORE_0_3),
    Period = dplyr::case_when(
      PY >= 2015 & PY <= 2019 ~ "2015–2019",
      PY >= 2020 & PY <= 2025 ~ "2020–2025",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(Period), !is.na(SCORE), SCORE %in% 0:3) %>%
  dplyr::count(Period, SCORE, name = "n") %>%
  tidyr::complete(Period = c("2015–2019", "2020–2025"), SCORE = 0:3, fill = list(n = 0)) %>%
  dplyr::group_by(Period) %>%
  dplyr::mutate(N = sum(n), prop = ifelse(N > 0, n / N, 0)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Period = factor(Period, levels = c("2015–2019", "2020–2025")),
    SCORE_LAB = factor(SCORE, levels = 0:3, labels = c("0 (none)", "1", "2", "3 (all)"))
  )

df_n_period <- df_score_period %>% dplyr::distinct(Period, N)

pB <- ggplot(df_score_period, aes(x = Period, y = prop, fill = SCORE_LAB)) +
  geom_col(width = 0.80, color = "white", linewidth = 0.25) +
  geom_text(
    data = df_n_period,
    aes(x = Period, y = 1.03, label = paste0("n = ", N)),
    inherit.aes = FALSE,
    size = 3.2, family = FONT_FAMILY
  ) +
  scale_fill_manual(values = pal_score, drop = FALSE) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.06), breaks = seq(0, 1, by = 0.25),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = NULL, y = "Proportion", fill = "Reporting score (0–3)") +
  theme_jsm(base_size = 12, legend_pos = "top") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        plot.margin = margin(t = 6, r = 10, b = 8, l = 12)) +
  coord_cartesian(clip = "off")

# Panel C
binom_ci_low <- function(x, n, alpha = 0.05) ifelse(x <= 0, 0, qbeta(alpha/2, x, n - x + 1))
binom_ci_high <- function(x, n, alpha = 0.05) ifelse(x >= n, 1, qbeta(1 - alpha/2, x + 1, n - x))

ev_keep_main_ordered <- ev_levels

ge2_df <- M %>%
  dplyr::mutate(EVIDENCE_TYPE = as.character(EVIDENCE_TYPE), SCORE = as.integer(REPORTING_SCORE_0_3)) %>%
  dplyr::filter(!is.na(SCORE), SCORE %in% 0:3) %>%
  dplyr::filter(!is.na(EVIDENCE_TYPE), EVIDENCE_TYPE %in% ev_keep_main_ordered) %>%
  dplyr::mutate(EVIDENCE_TYPE = factor(EVIDENCE_TYPE, levels = ev_keep_main_ordered)) %>%
  dplyr::group_by(EVIDENCE_TYPE) %>%
  dplyr::summarise(
    N = n(),
    x = sum(SCORE >= 2, na.rm = TRUE),
    prop = ifelse(N > 0, x / N, NA_real_),
    ci_low = ifelse(N > 0, binom_ci_low(x, N), NA_real_),
    ci_high = ifelse(N > 0, binom_ci_high(x, N), NA_real_),
    .groups = "drop"
  ) %>%
  dplyr::filter(!is.na(prop)) %>%
  dplyr::mutate(
    lab_pct = scales::percent(prop, accuracy = 1),
    lab_n   = paste0("n = ", N),
    lab_y   = pmin(pmax(prop, ci_high) + 0.035, 0.98)
  )

ev_colors_7 <- ev_colors[names(ev_colors) %in% ev_keep_main_ordered]

pC <- ggplot(ge2_df, aes(x = EVIDENCE_TYPE, y = prop, fill = EVIDENCE_TYPE)) +
  geom_col(width = 0.78, color = "white", linewidth = 0.35) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.14, linewidth = 0.55) +
  geom_text(aes(y = lab_y, label = lab_pct), size = 3.2, family = FONT_FAMILY) +
  geom_text(aes(y = 1.03, label = lab_n), size = 3.1, family = FONT_FAMILY) +
  scale_fill_manual(values = ev_colors_7, breaks = ev_keep_main_ordered, drop = FALSE, guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1), breaks = seq(0, 1, by = 0.25),
                     expand = expansion(mult = c(0, 0.11))) +
  labs(x = NULL, y = "Proportion with reporting score \u2265 2") +
  theme_jsm(base_size = 11, legend_pos = "right") +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 18, hjust = 1),
        plot.margin = margin(t = 6, r = 12, b = 8, l = 10)) +
  coord_cartesian(clip = "off")

Figure_4 <- (pA / (pB | pC)) +
  patchwork::plot_layout(heights = c(1.20, 0.95), widths = c(1, 1)) +
  patchwork::plot_annotation(tag_levels = "A") & panel_tag_theme

save_plot_dual(Figure_4, "Figure_4", width = 11.6, height = 10.2)
