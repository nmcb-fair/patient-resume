# Resolve newest file by regex across one or more folders
resolve_latest_file <- function(dir_paths, pattern, fallback_paths = character(0), required = TRUE) {
  files <- character(0)
  for (dir_path in unique(dir_paths)) {
    if (dir.exists(dir_path)) {
      files <- c(files, list.files(dir_path, pattern = pattern, full.names = TRUE))
    }
  }
  if (length(files) > 0) {
    info <- file.info(files)
    return(rownames(info)[which.max(info$mtime)])
  }
  for (fallback in fallback_paths) {
    if (file.exists(fallback)) return(fallback)
  }
  if (!required) return(NA_character_)
  stop("No matching file found for pattern '", pattern, "' in: ", paste(dir_paths, collapse = ", "))
}

read_castor_export <- function(path, preferred_sheet = "Study results") {
  if (is.na(path) || is.null(path) || path == "") return(NULL)
  if (grepl("\\.xlsx$", path, ignore.case = TRUE)) {
    sheet_names <- getSheetNames(path)
    target_sheet <- if (preferred_sheet %in% sheet_names) preferred_sheet else sheet_names[1]
    return(read.xlsx(path, sheet = target_sheet, sep.names = " "))
  }
  read_delim(path, delim = ";", escape_double = FALSE, trim_ws = TRUE)
}

# BMI from any NMCB Screener export in screening/ (newest file wins per ID).
# Resolves column "bmi" if present, else "self_bmi", else "self_bmi_1" (Post-COVID / Lyme).
build_screening_bmi_from_dir <- function(dir_path) {
  if (!dir.exists(dir_path)) return(NULL)
  pat <- "^NMCB_.*Screener.*export_.*\\.(csv|xlsx)$"
  files <- list.files(dir_path, pattern = pat, full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) return(NULL)
  info <- file.info(files)
  files <- files[order(info$mtime, decreasing = TRUE, na.last = TRUE)]

  norm_names <- function(nm) gsub('^"|"$', '', trimws(nm))

  find_id_col <- function(nm) {
    for (cand in c("Castor Participant ID", "Participant Id", "Participant ID", "Castor.ID", "Castor ID")) {
      if (cand %in% nm) return(cand)
    }
    id_idx <- grep("Castor.*Participant|Participant.*ID|Castor[\\. ]?ID|Participant Id",
                   nm, ignore.case = TRUE)[1]
    if (!is.na(id_idx)) return(nm[id_idx])
    NA_character_
  }

  find_bmi_col <- function(nm) {
    for (cand in c("bmi", "self_bmi", "self_bmi_1")) {
      if (cand %in% nm) return(cand)
    }
    NA_character_
  }

  id_to_bmi <- list()
  for (f in files) {
    df <- read_castor_export(f)
    if (is.null(df) || nrow(df) == 0) next
    names(df) <- norm_names(names(df))
    id_col <- find_id_col(names(df))
    bmi_col <- find_bmi_col(names(df))
    if (is.na(id_col) || is.na(bmi_col)) next
    for (i in seq_len(nrow(df))) {
      raw_id <- df[[id_col]][i]
      id_ch <- as.character(trimws(raw_id))
      if (is.na(raw_id) || id_ch == "") next
      if (!is.null(id_to_bmi[[id_ch]])) next
      val <- df[[bmi_col]][i]
      if (is.na(val)) next
      if (is.character(val) && trimws(val) == "") next
      val_num <- suppressWarnings(as.numeric(val))
      if (is.na(val_num)) next
      id_to_bmi[[id_ch]] <- val_num
    }
  }
  if (length(id_to_bmi) == 0) return(NULL)
  ids <- names(id_to_bmi)
  out <- data.frame(
    "Castor Participant ID" = ids,
    self_bmi = unlist(id_to_bmi[ids], use.names = FALSE),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out
}

# Optional Castor multi-study Excel export: use tab ME_CFS_Screener (same study as df_me_cfs, often newer than CSV-only pull).
load_screening_excel_me_cfs <- function(dir_paths) {
  bundle_path <- resolve_latest_file(
    dir_paths,
    "^NMCB_Screening_excel_export_.*\\.xlsx$",
    required = FALSE
  )
  if (is.na(bundle_path) || !nzchar(bundle_path) || !file.exists(bundle_path)) return(NULL)
  sheets <- getSheetNames(bundle_path)
  sheet_me <- NA_character_
  if ("ME_CFS_Screener" %in% sheets) {
    sheet_me <- "ME_CFS_Screener"
  } else {
    hit <- grep("^ME_CFS_Screener", sheets)[1]
    if (!is.na(hit)) sheet_me <- sheets[hit]
  }
  if (is.na(sheet_me)) {
    warning("NMCB_Screening_excel_export workbook found but no ME_CFS_Screener sheet: ", bundle_path)
    return(NULL)
  }
  df <- read.xlsx(bundle_path, sheet = sheet_me, sep.names = " ")
  if (is.null(df) || nrow(df) == 0) return(NULL)
  names(df) <- gsub('^"|"$', '', trimws(names(df)))
  # Map expects self_bmi (D15); Excel may expose calculation as bmi
  if ("bmi" %in% names(df) && !"self_bmi" %in% names(df)) {
    df$self_bmi <- df$bmi
  }
  df
}

# New folder-based input locations
screening_dir <- "input/screening"
visit_dir <- "input/visit"
dsq2_dir <- "input/dsq_2"
vragenlijsten_dir <- "input/vragenlijsten"

# Main screener data (required)
screening_file <- resolve_latest_file(
  c(screening_dir, "input"),
  "^NMCB_Study_ME_CFS_Screener_(excel_)?export_.*\\.(csv|xlsx)$",
  fallback_paths = c("input/NMCB_Study_ME_CFS_Screener_export_20260210.csv"),
  required = TRUE
)
df_me_cfs <- read_castor_export(screening_file)

# ME/CFS screener rows from combined Excel export (optional; fills gaps vs. CSV df_me_cfs)
df_screening_excel_me_cfs <- load_screening_excel_me_cfs(c(screening_dir, "input"))

# Newest-per-ID BMI across all Screener exports in screening/ (fills gaps vs. df_me_cfs)
df_screening_bmi <- build_screening_bmi_from_dir(screening_dir)

# Visit study data (required)
visit_file <- resolve_latest_file(
  c(visit_dir, "input"),
  "^NMCB_Study_(excel_)?export_.*\\.(csv|xlsx)$",
  fallback_paths = c("input/NMCB_Study_export_20260210.csv"),
  required = TRUE
)
df_visit <- read_castor_export(visit_file)

# Measurements/Questionnaires legacy export (optional fallback source)
measurements_file <- resolve_latest_file(
  c(visit_dir, "input"),
  "^NMCB_Measurements_Questionnaires(_excel)?_export_.*\\.(csv|xlsx)$",
  fallback_paths = c("input/NMCB_Measurements_Questionnaires_export_20260212.csv"),
  required = FALSE
)
df_measurements <- read_castor_export(measurements_file)

# DSQ2 symptoms data (required)
dsq2_file <- resolve_latest_file(
  c(dsq2_dir, "input"),
  "^NMCB_Study_Symptomen_frequentie_en_ernst_hiervan_(excel_)?export_.*\\.(csv|xlsx)$",
  fallback_paths = c("input/NMCB_Study_Symptomen_frequentie_en_ernst_hiervan_export_20260210.csv"),
  required = TRUE
)
df_dsq2 <- read_castor_export(dsq2_file)

# Vragenlijsten data (optional; used as supplemental Castor source)
vragenlijsten_file <- resolve_latest_file(
  c(vragenlijsten_dir, "input"),
  "^NMCB_Study_Vragenlijsten_(excel_)?export_.*\\.(csv|xlsx)$",
  required = FALSE
)
df_vragenlijsten <- read_castor_export(vragenlijsten_file)

# Load CRL admin (for Patient type -> D32)
# Use sep.names = " " to preserve spaces in column names (e.g. "Patient type" instead of "Patient.type")
df_crl_admin <- read.xlsx("input/CRL admin/CRL_Admin.xlsx", sheet = "Blad1", sep.names = " ")

# Load NASA Lean test (for NASA test sheet)
df_nasa_lean <- read_csv("input/Omron/NASA_LEAN_TEST.csv", show_col_types = FALSE)
