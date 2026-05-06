# ============================================================================
# Run All Script - Generate Patient Resumes
# ============================================================================
# This script sources all required modules and runs the patient resume generation
#
# Usage:
#   source("run_all.R")
#   Or modify the participant IDs below and run
# ============================================================================

# Clear workspace (optional - comment out if you want to keep existing objects)
# rm(list = ls())

# Source all required modules in order
cat("Loading libraries...\n")
source("01_load_libraries.R")

cat("Loading data...\n")
source("02_load_data.R")

cat("Loading mapping configuration...\n")
source("03_mapping.R")

cat("Loading helper functions...\n")
source("04_helper_functions.R")

cat("Loading export function...\n")
source("05_export_function.R")

# ============================================================================
# Configuration - Modify these values as needed
# ============================================================================

# ID selection mode
use_random_ids <- FALSE

# Manual participant IDs (used when use_random_ids = FALSE)
participant_ids_manual <- c("1001816")

# Random selection settings (used when use_random_ids = TRUE)
n_random_participants <- 50
random_seed <- NULL  # set numeric value for reproducible sampling

if (use_random_ids) {
  all_participant_ids <- unique(as.character(trimws(df_me_cfs[["Castor Participant ID"]])))
  all_participant_ids <- all_participant_ids[!is.na(all_participant_ids) & all_participant_ids != ""]
  if (!is.null(random_seed)) set.seed(random_seed)
  sample_n <- min(n_random_participants, length(all_participant_ids))
  participant_ids <- sample(all_participant_ids, size = sample_n, replace = FALSE)
} else {
  participant_ids <- participant_ids_manual
}

# Template path
template_path <- "template/template.xlsx"

# Output directory
output_dir <- "exports_resume"

# Sheet name in template
sheet_name <- "Resumé"

# ============================================================================
# Run the export
# ============================================================================

cat("\nStarting patient resume generation...\n")
cat("ID mode:", if (use_random_ids) "random" else "manual", "\n")
if (use_random_ids) {
  cat("Random participants selected:", length(participant_ids), "\n")
}
cat("Participants to process:", paste(participant_ids, collapse = ", "), "\n")
cat("Template:", template_path, "\n")
cat("Output directory:", output_dir, "\n\n")

export_participants_resume(
  df = df_me_cfs,
  ids = participant_ids,
  map = map_resume,
  template_path = template_path,
  sheet = sheet_name,
  out_dir = output_dir,
  df_visit = df_visit,
  df_dsq2 = df_dsq2,
  df_crl_admin = df_crl_admin,
  df_nasa_lean = df_nasa_lean,
  df_measurements = df_measurements,
  df_vragenlijsten = df_vragenlijsten,
  df_screening_bmi = df_screening_bmi,
  df_screening_excel_me_cfs = df_screening_excel_me_cfs,
  cdl_alert_dir = "input/CDL_alert"
)

cat("\n✓ Patient resume generation completed!\n")
cat("Check the", output_dir, "directory for output files.\n")
