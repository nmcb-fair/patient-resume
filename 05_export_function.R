# Main export function for generating patient resumes

# Helper to normalize participant ID for matching across data sources
match_id <- function(id, id_vec) {
  id_char <- as.character(trimws(id))
  id_vec_char <- as.character(trimws(id_vec))
  # Exact match first
  idx <- which(id_vec_char == id_char)
  if (length(idx) > 0) return(idx[1])
  # Try without leading zeros
  id_trim <- sub("^0+", "", id_char)
  idx <- which(sub("^0+", "", id_vec_char) == id_trim)
  if (length(idx) > 0) return(idx[1])
  return(NA_integer_)
}

export_participants_resume <- function(df, ids, map,
                                       template_path,
                                       sheet = "Resumé",
                                       out_dir = "exports_resume",
                                       field_report_path = file.path(out_dir, "field_capture_report.csv"),
                                       id_col = "Castor Participant ID",
                                       on_duplicate = c("first", "all", "error"),
                                       df_visit = NULL,
                                       df_dsq2 = NULL,
                                       df_crl_admin = NULL,
                                       df_nasa_lean = NULL,
                                       df_measurements = NULL,
                                       df_vragenlijsten = NULL,
                                       df_screening_bmi = NULL,
                                       df_screening_excel_me_cfs = NULL,
                                       cdl_alert_dir = "input/CDL_alert") {

  on_duplicate <- match.arg(on_duplicate)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Normalize variable names: remove quotes, trim whitespace
  normalize_var_name <- function(x) {
    gsub('^"|"$', '', trimws(x))
  }
  
  df_vars_normalized <- normalize_var_name(names(df))
  names(df) <- df_vars_normalized  # Update df column names to normalized versions

  normalize_source_df <- function(source_df) {
    if (is.null(source_df)) return(NULL)
    names(source_df) <- normalize_var_name(names(source_df))
    source_df
  }
  df_visit <- normalize_source_df(df_visit)
  df_measurements <- normalize_source_df(df_measurements)
  df_dsq2 <- normalize_source_df(df_dsq2)
  df_vragenlijsten <- normalize_source_df(df_vragenlijsten)
  df_screening_bmi <- normalize_source_df(df_screening_bmi)
  df_screening_excel_me_cfs <- normalize_source_df(df_screening_excel_me_cfs)
  
  map_vars_normalized <- normalize_var_name(map$var)
  map$var <- map_vars_normalized  # Update mapping variable names
  
  all_source_vars <- unique(c(
    names(df),
    if (!is.null(df_visit)) names(df_visit) else character(0),
    if (!is.null(df_measurements)) names(df_measurements) else character(0),
    if (!is.null(df_dsq2)) names(df_dsq2) else character(0),
    if (!is.null(df_vragenlijsten)) names(df_vragenlijsten) else character(0),
    if (!is.null(df_screening_bmi)) names(df_screening_bmi) else character(0),
    if (!is.null(df_screening_excel_me_cfs)) names(df_screening_excel_me_cfs) else character(0)
  ))

  # Fuzzy matching for variables that don't have exact matches
  for (i in seq_along(map$var)) {
    map_var <- map$var[i]
    if (!map_var %in% all_source_vars) {
      # Try partial match - check if map_var is a prefix of any source var
      matches <- all_source_vars[startsWith(all_source_vars, map_var)]
      if (length(matches) == 0) {
        # Try reverse - check if any source var starts with map_var base (before #)
        base_name <- gsub("#.*", "", map_var)
        matches <- all_source_vars[startsWith(all_source_vars, base_name)]
      }
      if (length(matches) > 0) {
        map$var[i] <- matches[1]  # Update mapping to use matched variable
      }
    }
  }
  
  map2 <- map
  missing_map_vars <- setdiff(map$var, all_source_vars)
  
  if (length(missing_map_vars) > 0) {
    warning("These mapped variables are not in loaded data sources and will be skipped:\n- ",
            paste(missing_map_vars, collapse = "\n- "))
  }

  # Normalize id_col as well
  id_col_normalized <- normalize_var_name(id_col)
  if (!id_col_normalized %in% names(df)) {
    # Try original id_col if normalized doesn't work
    if (!id_col %in% names(df)) {
      stop("id_col not found in df: ", id_col)
    }
    id_col_normalized <- id_col
  }

  find_participant_id_col <- function(source_df) {
    if (is.null(source_df) || ncol(source_df) == 0) return(NA_character_)
    for (cand in c("Castor Participant ID", "Participant Id", "Participant ID", "Castor.ID", "Castor ID")) {
      if (cand %in% names(source_df)) return(cand)
    }
    id_idx <- grep("Castor.*Participant|Participant.*ID|Castor[\\. ]?ID|Participant Id",
                   names(source_df), ignore.case = TRUE)[1]
    if (!is.na(id_idx)) return(names(source_df)[id_idx])
    NA_character_
  }

  build_source_info <- function(source_df, source_name) {
    if (is.null(source_df)) return(NULL)
    id_col_source <- find_participant_id_col(source_df)
    if (is.na(id_col_source) || !id_col_source %in% names(source_df)) return(NULL)
    ids_source <- as.character(trimws(source_df[[id_col_source]]))
    list(df = source_df, id_col = id_col_source, ids = ids_source, name = source_name)
  }

  supplemental_sources <- Filter(Negate(is.null), list(
    build_source_info(df_screening_excel_me_cfs, "df_screening_excel_me_cfs"),
    build_source_info(df_screening_bmi, "df_screening_bmi"),
    build_source_info(df_visit, "df_visit"),
    build_source_info(df_measurements, "df_measurements"),
    build_source_info(df_dsq2, "df_dsq2"),
    build_source_info(df_vragenlijsten, "df_vragenlijsten")
  ))

  # Pre-compute cell coordinates for direct mappings (avoid repeated calculations)
  direct_mappings <- map2[is.na(map2$op) | !map2$op %in% c("add_to_list", "add_to_sheet", "add_text_to_sheet"), ]
  if (nrow(direct_mappings) > 0) {
    direct_mappings$row <- NA_integer_
    direct_mappings$col <- NA_integer_
    for (i in seq_len(nrow(direct_mappings))) {
      rc <- cell_to_rowcol(direct_mappings$cell[i])
      direct_mappings$row[i] <- rc$row
      direct_mappings$col[i] <- rc$col
    }
  }
  
  # Pre-compute C4 and D4 coordinates for diagnosis list (Post-COVID goes to D4)
  c4_rc <- cell_to_rowcol("C4")
  d4_rc <- cell_to_rowcol("D4")
  # Pre-compute D19:D20, E15:E20, E25, E26, F28 for inclusie/exclusie operations
  d19_rc <- cell_to_rowcol("D19")
  d20_rc <- cell_to_rowcol("D20")
  e_cells <- lapply(paste0("E", 15:20), cell_to_rowcol)
  e25_rc <- cell_to_rowcol("E25")
  e26_rc <- cell_to_rowcol("E26")
  f28_rc <- cell_to_rowcol("F28")

  # Use flexible ID matching for main df (handles leading zeros)
  ids_df <- as.character(trimws(df[[id_col_normalized]]))
  ids_df_trim <- sub("^0+", "", ids_df)

  processed <- character(0)
  skipped <- character(0)
  report_rows <- list()

  add_report_row <- function(participant_id, var_name, target, op_name, status, value_preview, source_name) {
    report_rows[[length(report_rows) + 1]] <<- data.frame(
      participant_id = as.character(participant_id),
      field = as.character(var_name),
      target = as.character(target),
      operation = as.character(op_name),
      status = as.character(status),
      value_preview = as.character(value_preview),
      source_dataset = as.character(source_name),
      stringsAsFactors = FALSE
    )
  }

  for (id in ids) {

    id_char <- as.character(trimws(id))
    id_trim <- sub("^0+", "", id_char)
    idx <- which(ids_df == id_char | ids_df_trim == id_trim)

    if (length(idx) == 0) {
      skipped <- c(skipped, id_char)
      warning("ID not found in main screener data (df_me_cfs), skipped: ", id_char,
              ". Participants must exist in NMCB_Study_ME_CFS_Screener export.")
      next
    }
    processed <- c(processed, id_char)

    if (length(idx) > 1) {
      if (on_duplicate == "error") stop("Duplicate rows found for ID: ", id)
      if (on_duplicate == "first") idx <- idx[1]
    }

    for (j in idx) {

      # Load workbook (necessary for each participant to get fresh copy)
      wb <- loadWorkbook(template_path)
      # Resolve sheet name (handle encoding e.g. Resumé)
      main_sheet <- sheet
      if (!main_sheet %in% names(wb)) {
        sheet_idx <- grep("^Resum", names(wb), ignore.case = TRUE)[1]
        if (!is.na(sheet_idx)) main_sheet <- names(wb)[sheet_idx]
      }
      
      # Initialize lists for collecting text to add (use list for efficiency)
      diagnosis_list <- list()  # For C4:G4 range
      illness_items <- list()   # For Illness sheet
      medicatie_items <- list() # For Medicatie sheet
      nasa_diff <- NA_real_
      nasa_h2 <- NA_real_
      lab_priority <- "Normal"
      
      # Prepare batch write data for direct mappings
      write_rows <- integer(0)
      write_cols <- integer(0)
      write_values <- list()

      is_missing_value <- function(x) {
        if (is.null(x) || length(x) == 0) return(TRUE)
        x1 <- x[1]
        if (is.na(x1)) return(TRUE)
        trimws(as.character(x1)) == ""
      }

      # Build a participant-level value cache by combining screener + supplemental sources
      participant_values <- as.list(df[j, , drop = FALSE])
      participant_sources <- setNames(as.list(rep("df_me_cfs", length(participant_values))), names(participant_values))
      for (src in supplemental_sources) {
        src_idx <- match_id(id_char, src$ids)
        if (is.na(src_idx)) next
        src_values <- as.list(src$df[src_idx, , drop = FALSE])
        for (nm in names(src_values)) {
          if (is.null(participant_values[[nm]]) || is_missing_value(participant_values[[nm]])) {
            participant_values[[nm]] <- src_values[[nm]]
            participant_sources[[nm]] <- src$name
          }
        }
      }

      # Process all mappings
      for (k in seq_len(nrow(map2))) {

        v <- participant_values[[map2$var[k]]]
        if (is.null(v) || length(v) == 0) v <- NA
        v <- v[1]
        var_name <- map2$var[k]
        cell_target <- map2$cell[k]
        op <- map2$op[k]

        source_name <- participant_sources[[var_name]]
        if (is.null(source_name) || length(source_name) == 0 || is.na(source_name)) {
          source_name <- "missing_source"
        }

        # Handle different operation types
        if (!is.na(op) && op == "add_to_list") {
          text_to_add <- get_text_to_add(var_name, v)
          if (!is.null(text_to_add)) {
            diagnosis_list[[length(diagnosis_list) + 1]] <- text_to_add
            add_report_row(id_char, var_name, cell_target, op, "captured", text_to_add, source_name)
          } else if (is.na(v) || v == "") {
            add_report_row(id_char, var_name, cell_target, op, "missing_data", "", source_name)
          } else {
            add_report_row(id_char, var_name, cell_target, op, "not_selected", as.character(v), source_name)
          }
          next
        }
        
        if (!is.na(op) && op == "add_to_sheet") {
          text_to_add <- get_text_to_add(var_name, v)
          if (!is.null(text_to_add)) {
            if (cell_target == "Illness") {
              illness_items[[length(illness_items) + 1]] <- text_to_add
            } else if (cell_target == "Medicatie") {
              medicatie_items[[length(medicatie_items) + 1]] <- text_to_add
            }
            add_report_row(id_char, var_name, cell_target, op, "captured", text_to_add, source_name)
          } else if (is.na(v) || v == "") {
            add_report_row(id_char, var_name, cell_target, op, "missing_data", "", source_name)
          } else {
            add_report_row(id_char, var_name, cell_target, op, "not_selected", as.character(v), source_name)
          }
          next
        }
        
        if (!is.na(op) && op == "add_text_to_sheet") {
          if (!is.na(v) && v != "") {
            if (cell_target == "Medicatie") {
              medicatie_items[[length(medicatie_items) + 1]] <- as.character(v)
            }
            add_report_row(id_char, var_name, cell_target, op, "captured", as.character(v), source_name)
          } else {
            add_report_row(id_char, var_name, cell_target, op, "missing_data", "", source_name)
          }
          next
        }

        # Skip if value is empty
        if (is.na(v) || v == "") {
          add_report_row(id_char, var_name, cell_target, ifelse(is.na(op), "direct", op), "missing_data", "", source_name)
          next
        }

        # Apply optional operation
        if (!is.na(op) && op == "ja_nee") {
          v <- to_ja_nee(v)
          if (is.na(v) || v == "") {
            add_report_row(id_char, var_name, cell_target, op, "missing_after_transform", "", source_name)
            next
          }
        } else if (!is.na(op) && op == "vrouw_man") {
          v <- to_vrouw_man(v)
          if (is.na(v) || v == "") {
            add_report_row(id_char, var_name, cell_target, op, "missing_after_transform", "", source_name)
            next
          }
        } else if (!is.na(op) && op == "no_decimal") {
          v <- to_no_decimal(v)
          if (is.na(v)) {
            add_report_row(id_char, var_name, cell_target, op, "missing_after_transform", "", source_name)
            next
          }
        }

        add_report_row(
          id_char, var_name, cell_target,
          ifelse(is.na(op), "direct", op),
          "captured", as.character(v), source_name
        )

        # Collect for batch write
        direct_idx <- match(var_name, direct_mappings$var)
        if (!is.na(direct_idx)) {
          write_rows <- c(write_rows, direct_mappings$row[direct_idx])
          write_cols <- c(write_cols, direct_mappings$col[direct_idx])
          write_values[[length(write_values) + 1]] <- v
        }
      }
      
      # Batch write all direct mappings at once
      if (length(write_values) > 0) {
        for (i in seq_along(write_values)) {
          writeData(
            wb, sheet = main_sheet, x = write_values[[i]],
            startRow = write_rows[i], startCol = write_cols[i],
            colNames = FALSE
          )
        }
      }
      
      # Write diagnosis list: other diagnoses to C4, Post-COVID to D4
      if (length(diagnosis_list) > 0) {
        diag_items <- unlist(diagnosis_list)
        post_covid <- grepl("Post-COVID|Post COVID", diag_items, ignore.case = TRUE)
        other_diag <- diag_items[!post_covid]
        if (length(other_diag) > 0) {
          writeData(wb, sheet = main_sheet,
                    x = paste(other_diag, collapse = ", "),
                    startRow = c4_rc$row, startCol = c4_rc$col, colNames = FALSE)
        }
        if (any(post_covid)) {
          writeData(wb, sheet = main_sheet,
                    x = diag_items[post_covid][1],
                    startRow = d4_rc$row, startCol = d4_rc$col, colNames = FALSE)
        }
      }
      
      # Batch write to Illness sheet
      if (length(illness_items) > 0) {
        if (!"Illness" %in% names(wb)) {
          addWorksheet(wb, "Illness")
        }
        # Write all items at once using a data frame
        writeData(
          wb, sheet = "Illness", 
          x = data.frame(Item = unlist(illness_items), stringsAsFactors = FALSE),
          startRow = 2, startCol = 1,
          colNames = FALSE
        )
      }
      
      # Batch write to Medicatie sheet
      if (length(medicatie_items) > 0) {
        if (!"Medicatie" %in% names(wb)) {
          addWorksheet(wb, "Medicatie")
        }
        # Write all items at once using a data frame
        writeData(
          wb, sheet = "Medicatie",
          x = data.frame(Item = unlist(medicatie_items), stringsAsFactors = FALSE),
          startRow = 2, startCol = 1,
          colNames = FALSE
        )
      }

      # --- Additional mappings from other data sources ---
      d26_rc <- cell_to_rowcol("D26")
      d30_rc <- cell_to_rowcol("D30")
      d32_rc <- cell_to_rowcol("D32")

      # (1) D26: average of hgs_01, hgs_02, hgs_03 from df_visit; if not found, try df_measurements
      hgs_avg <- NA_real_
      for (hgs_df in list(df_visit, df_measurements)) {
        if (is.null(hgs_df) || !is.na(hgs_avg)) next
        id_col_hgs <- names(hgs_df)[grep("Participant", names(hgs_df), ignore.case = TRUE)[1]]
        if (is.na(id_col_hgs)) id_col_hgs <- "Participant Id"
        ids_hgs <- as.character(trimws(hgs_df[[id_col_hgs]]))
        id_char <- as.character(trimws(id))
        hgs_matches <- (ids_hgs == id_char) | (sub("^0+", "", ids_hgs) == sub("^0+", "", id_char))
        hgs_rows <- hgs_df[hgs_matches, , drop = FALSE]
        if (nrow(hgs_rows) > 0) {
          hgs_primary <- c("hgs_01", "hgs_02", "hgs_03")
          hgs_fallback <- c("hgs__1", "hgs__2", "hgs__3")
          hgs_cols <- hgs_primary[hgs_primary %in% names(hgs_df)]
          if (length(hgs_cols) == 0) hgs_cols <- hgs_fallback[hgs_fallback %in% names(hgs_df)]
          if (length(hgs_cols) > 0) {
            vals <- as.numeric(unlist(hgs_rows[, hgs_cols]))
            vals <- vals[!is.na(vals) & !vals %in% c(-99, -999)]
            if (length(vals) == 0 && length(hgs_fallback[hgs_fallback %in% names(hgs_df)]) > 0) {
              hgs_cols <- hgs_fallback[hgs_fallback %in% names(hgs_df)]
              vals <- as.numeric(unlist(hgs_rows[, hgs_cols]))
              vals <- vals[!is.na(vals) & !vals %in% c(-99, -999)]
            }
            if (length(vals) > 0) hgs_avg <- mean(vals)
          }
        }
      }
      if (!is.na(hgs_avg)) {
        writeData(wb, sheet = main_sheet, x = as.integer(round(hgs_avg)),
                  startRow = d26_rc$row, startCol = d26_rc$col, colNames = FALSE)
      }

      # (2) df_dsq2: Survey Progress -> D30
      if (!is.null(df_dsq2)) {
        id_col_dsq <- names(df_dsq2)[grep("Castor.*Participant|Participant.*ID", names(df_dsq2), ignore.case = TRUE)[1]]
        if (is.na(id_col_dsq)) id_col_dsq <- "Castor Participant ID"
        didx <- match_id(id, df_dsq2[[id_col_dsq]])
        if (!is.na(didx) && "Survey Progress" %in% names(df_dsq2)) {
          val <- df_dsq2[["Survey Progress"]][didx]
          if (!is.na(val) && val != "") {
            writeData(wb, sheet = main_sheet, x = val,
                      startRow = d30_rc$row, startCol = d30_rc$col, colNames = FALSE)
          }
        }
      }

      # (3) df_crl_admin: Patient type -> D32
      if (!is.null(df_crl_admin)) {
        id_col_crl <- names(df_crl_admin)[grep("Castor[\\. ]?ID|Castor\\.ID", names(df_crl_admin), ignore.case = TRUE)[1]]
        if (is.na(id_col_crl)) id_col_crl <- "Castor.ID"
        cidx <- match_id(id, df_crl_admin[[id_col_crl]])
        if (!is.na(cidx)) {
          # Prefer exact "Patient type" or "Patient.type"; avoid "EV protocol + Patient type"
          pt_col <- NA_character_
          for (cand in c("Patient type", "Patient.type")) {
            if (cand %in% names(df_crl_admin)) {
              pt_col <- cand
              break
            }
          }
          if (is.na(pt_col)) {
            pt_col <- names(df_crl_admin)[grep("^Patient[\\. ]type$", names(df_crl_admin), ignore.case = TRUE)[1]]
          }
          if (!is.na(pt_col) && pt_col %in% names(df_crl_admin)) {
            val <- df_crl_admin[[pt_col]][cidx]
            if (!is.na(val) && trimws(as.character(val)) != "") {
              writeData(wb, sheet = main_sheet, x = as.character(val),
                        startRow = d32_rc$row, startCol = d32_rc$col, colNames = FALSE)
            }
          }
        }
      }

      # (4) df_nasa_lean: subset by PARTICIPANT_ID, sort by DATETIME (early to late), copy to 'NASA test' sheet
      if (!is.null(df_nasa_lean)) {
        pid_col <- names(df_nasa_lean)[grep("PARTICIPANT_ID|Participant.*ID", names(df_nasa_lean), ignore.case = TRUE)[1]]
        if (!is.na(pid_col) && pid_col %in% names(df_nasa_lean)) {
          ids_nasa <- as.character(trimws(df_nasa_lean[[pid_col]]))
          id_char <- as.character(trimws(id))
          id_matches <- (ids_nasa == id_char) |
            (sub("^0+", "", ids_nasa) == sub("^0+", "", id_char))
          nasa_sub <- df_nasa_lean[id_matches, , drop = FALSE]
          if (nrow(nasa_sub) > 0) {
            dt_col <- names(nasa_sub)[grep("^DATETIME$", names(nasa_sub), ignore.case = TRUE)[1]]
            if (!is.na(dt_col)) {
              dt_vec <- nasa_sub[[dt_col]]
              dt_parsed <- tryCatch({
                as.POSIXct(dt_vec, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
              }, error = function(e) NULL)
              if (is.null(dt_parsed)) {
                dt_parsed <- tryCatch(as.POSIXct(dt_vec, tz = "UTC"), error = function(e) NULL)
              }
              if (!is.null(dt_parsed) && !all(is.na(dt_parsed))) {
                nasa_sub <- nasa_sub[order(dt_parsed), , drop = FALSE]
              }
            }
            if (!"NASA test" %in% names(wb)) {
              addWorksheet(wb, "NASA test")
            }
            nasa_sub <- cbind(Index = seq_len(nrow(nasa_sub)), nasa_sub)
            writeData(wb, sheet = "NASA test", x = nasa_sub,
                      startRow = 1, startCol = 1, colNames = TRUE)
            # LIGGEN AVG (H1), STAAN AVG (I1), DIFF (J1); H2 = avg of E2&E3, I2 = avg of last 3 E, J2 = H2 - I2
            pulse_col <- names(nasa_sub)[grep("^PULSE$", names(nasa_sub), ignore.case = TRUE)[1]]
            if (!is.na(pulse_col)) {
              pulse_vals <- suppressWarnings(as.numeric(nasa_sub[[pulse_col]]))
              pulse_vals <- pulse_vals[!is.na(pulse_vals)]
              n <- length(pulse_vals)
              writeData(wb, sheet = "NASA test", x = "LIGGEN AVG", startRow = 1, startCol = 8, colNames = FALSE)
              writeData(wb, sheet = "NASA test", x = "STAAN AVG", startRow = 1, startCol = 9, colNames = FALSE)
              writeData(wb, sheet = "NASA test", x = "DIFF", startRow = 1, startCol = 10, colNames = FALSE)
              liggen_avg <- if (n >= 2) mean(pulse_vals[1:2]) else NA_real_
              staan_avg <- if (n >= 3) mean(pulse_vals[(n - 2):n]) else NA_real_
              if (n >= 2) {
                nasa_h2 <- liggen_avg
                writeData(wb, sheet = "NASA test", x = round(liggen_avg, 2), startRow = 2, startCol = 8, colNames = FALSE)
              }
              if (n >= 3) writeData(wb, sheet = "NASA test", x = round(staan_avg, 2), startRow = 2, startCol = 9, colNames = FALSE)
              if (n >= 3 && !is.na(liggen_avg)) {
                nasa_diff <- liggen_avg - staan_avg
                writeData(wb, sheet = "NASA test", x = round(nasa_diff, 2), startRow = 2, startCol = 10, colNames = FALSE)
              }
            }
          }
        }
      }

      # (5) CDL_alert: load CSV from cdl_alert_dir (and subfolders) if filename contains ID, copy to Laboratorium sheet
      if (!is.null(cdl_alert_dir) && dir.exists(cdl_alert_dir)) {
        id_char <- as.character(trimws(id))
        id_trim <- sub("^0+", "", id_char)
        csv_files <- list.files(cdl_alert_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
        # Match ID as whole token (e.g. analyzed_1001121_CDL or analyzed_001121_CDL)
        id_pattern <- paste0("(^|_)", id_char, "(_|\\.)")
        matches <- csv_files[grepl(id_pattern, basename(csv_files))]
        if (length(matches) == 0 && id_trim != id_char) {
          id_pattern_trim <- paste0("(^|_)", id_trim, "(_|\\.)")
          matches <- csv_files[grepl(id_pattern_trim, basename(csv_files))]
        }
        if (length(matches) > 0) {
          # Prefer most recent by modification time
          info <- file.info(matches)
          info <- info[order(info$mtime, decreasing = TRUE), ]
          cdl_file <- rownames(info)[1]
          cdl_data <- tryCatch({
            read.csv(cdl_file, header = FALSE, stringsAsFactors = FALSE)
          }, error = function(e) {
            read.csv(cdl_file, header = FALSE, fileEncoding = "latin1", stringsAsFactors = FALSE)
          })
          if (nrow(cdl_data) > 0 && "Laboratorium" %in% names(wb)) {
            writeData(wb, sheet = "Laboratorium", x = cdl_data,
                      startRow = 1, startCol = 1, colNames = FALSE)
            if (ncol(cdl_data) >= 4) {
              col4 <- as.character(cdl_data[, 4])
              if (any(grepl("ALARM", col4, ignore.case = TRUE))) lab_priority <- "ALARM"
              else if (any(grepl("Afwijkend", col4, ignore.case = TRUE))) lab_priority <- "Afwijkend"
              else lab_priority <- "Normal"
            }
          }
        }
      }

      # --- Inclusie/Exclusie rules for E15:E20 ---
      get_d_val <- function(cell) {
        m <- map2[map2$cell == cell, , drop = FALSE]
        if (nrow(m) == 0) return(NA)
        var_name <- m$var[1]
        if (!var_name %in% names(participant_values)) return(NA)
        v <- participant_values[[var_name]][1]
        if (grepl("self_bmi", var_name, fixed = TRUE)) return(to_no_decimal(v))
        return(v)
      }
      d15 <- suppressWarnings(as.numeric(get_d_val("D15")))
      d16 <- suppressWarnings(as.numeric(get_d_val("D16")))
      d17 <- suppressWarnings(as.numeric(get_d_val("D17")))
      d18 <- suppressWarnings(as.numeric(get_d_val("D18")))
      diag_items <- unlist(diagnosis_list)
      has_gezonde <- any(grepl("Gezonde deelnemer", diag_items, ignore.case = TRUE))
      med_items <- unlist(medicatie_items)
      illness_items_vec <- unlist(illness_items)
      has_geen_meds <- any(grepl("geen van (de )?bovenstaande medicijnen gebruikt", med_items, ignore.case = TRUE))
      has_geen_illness <- any(grepl("Bij mij is geen van de bovenstaande aandoeningen vastgesteld", illness_items_vec, fixed = TRUE))

      e_vals <- c(
        if (!is.na(d15) && d15 <= 40) "inclusie" else "exclusie",
        if (has_gezonde && !is.na(d16) && d16 <= 70) "exclusie" else "inclusie",
        if (!is.na(d17) && d17 <= 4) "inclusie" else "exclusie",
        if (!is.na(d18) && d18 <= 2) "inclusie" else "exclusie",
        if (has_geen_meds) "inclusie" else "gb",
        if (has_geen_illness) "gb" else "exclusie"
      )
      d19_val <- if (has_geen_meds) "geen" else "ja"
      d20_val <- if (has_geen_illness) "geen" else "ja"
      for (ii in 1:4) {
        writeData(wb, sheet = main_sheet, x = e_vals[ii],
                  startRow = e_cells[[ii]]$row, startCol = e_cells[[ii]]$col, colNames = FALSE)
      }
      writeData(wb, sheet = main_sheet, x = d19_val,
                startRow = d19_rc$row, startCol = d19_rc$col, colNames = FALSE)
      writeData(wb, sheet = main_sheet, x = e_vals[5],
                startRow = e_cells[[5]]$row, startCol = e_cells[[5]]$col, colNames = FALSE)
      writeData(wb, sheet = main_sheet, x = d20_val,
                startRow = d20_rc$row, startCol = d20_rc$col, colNames = FALSE)
      writeData(wb, sheet = main_sheet, x = e_vals[6],
                startRow = e_cells[[6]]$row, startCol = e_cells[[6]]$col, colNames = FALSE)
      # E25: if |DIFF| > 30 (orthostatic intolerance) OR H2 > 120 -> POTS, else gb (only when NASA data exists)
      if (!is.na(nasa_diff) || !is.na(nasa_h2)) {
        e25_val <- if ((!is.na(nasa_diff) && abs(nasa_diff) > 30) || (!is.na(nasa_h2) && nasa_h2 > 120)) "POTS" else "gb"
        writeData(wb, sheet = main_sheet, x = e25_val, startRow = e25_rc$row, startCol = e25_rc$col, colNames = FALSE)
      }

      # E26: man < 44 -> velagt; vrouw < 23 -> velagt; otherwise normaal
      m_g3 <- map2[map2$cell == "F3", , drop = FALSE]
      g3_var <- if (nrow(m_g3) > 0) m_g3$var[1] else "sex"
      g3_val <- if (g3_var %in% names(participant_values)) to_vrouw_man(participant_values[[g3_var]][1]) else NA_character_
      g3 <- trimws(tolower(g3_val))
      e26_val <- if (!is.na(hgs_avg) && ((g3 == "man" && hgs_avg < 44) || (g3 == "vrouw" && hgs_avg < 23))) "velagt" else "normaal"
      writeData(wb, sheet = main_sheet, x = e26_val, startRow = e26_rc$row, startCol = e26_rc$col, colNames = FALSE)

      # F28: priority ALARM > Afwijkend > Normal (from Laboratorium column D = Opmerking)
      writeData(wb, sheet = main_sheet, x = lab_priority, startRow = f28_rc$row, startCol = f28_rc$col, colNames = FALSE)

      safe_id <- gsub("[^A-Za-z0-9_\\-]+", "_", as.character(id))
      suffix  <- if (length(idx) > 1) paste0("_row", j) else ""
      out_file <- file.path(out_dir, paste0("Participant_", safe_id, suffix, ".xlsx"))

      saveWorkbook(wb, out_file, overwrite = TRUE)
    }
  }

  # Summary message
  if (length(processed) > 0) {
    message(sprintf("Created %d resume(s) for: %s", length(processed), paste(processed, collapse = ", ")))
  }
  if (length(skipped) > 0) {
    message(sprintf("Skipped %d ID(s) (not in screener): %s", length(skipped), paste(skipped, collapse = ", ")))
  }

  if (!is.null(field_report_path) && length(report_rows) > 0) {
    report_df <- do.call(rbind, report_rows)
    report_df$value_preview <- substr(report_df$value_preview, 1, 120)
    write.csv(report_df, field_report_path, row.names = FALSE, na = "")
    message("Field capture report written to: ", field_report_path)
    status_summary <- sort(table(report_df$status), decreasing = TRUE)
    message("Field capture summary: ",
            paste(paste0(names(status_summary), "=", as.integer(status_summary)), collapse = ", "))
  }

  invisible(NULL)
}
