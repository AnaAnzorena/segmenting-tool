# app.R  ─────────────────────────────────────────────────────────────────
# Route Segment Editor ➜ Ridership Allocation ➜ OD/IPF Expansion (3-step)
# -----------------------------------------------------------------------

# Core packages
library(shiny)
library(readxl)
library(leaflet)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(DT)
library(writexl)

# Extra pkgs used by Step 3
library(tidyverse)
library(stringi)
library(ipfr)
library(questionr)
library(shinythemes)
library(shinyfullscreen)
library(shinyjs)


#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # in RStudio

# ── helpers from your allocation app ────────────────────────────────────
# ─────────────────────────────────────────────────────────────────────────────
# Helper: route rename mapping
# ─────────────────────────────────────────────────────────────────────────────
apply_route_map <- function(df, map_path) {
  if (!"route_short_name" %in% names(df)) {
    route_cols <- names(df)[tolower(names(df)) == "route"]
    if (length(route_cols) > 0) {
      df <- df %>% mutate(route_short_name = as.character(.data[[route_cols[1]]]))
    } else {
      stop("Need a `route_short_name` or a `Route`/`ROUTE` column in SEGMENTOS.")
    }
  }
  if (is.null(map_path) || !file.exists(map_path)) return(df)
  
  mapping <- read.csv(map_path, stringsAsFactors = FALSE, strip.white = TRUE)
  if (!all(c("old", "new") %in% names(mapping))) return(df)
  
  mapping <- mapping %>%
    mutate(across(c(old, new), toupper))
  
  df %>%
    mutate(route_short_name = toupper(route_short_name)) %>%
    left_join(mapping, by = c("route_short_name" = "old")) %>%
    mutate(route_short_name = coalesce(new, route_short_name)) %>%
    select(-new)
}

# ─────────────────────────────────────────────────────────────────────────────
# Main: allocate ridership
# ─────────────────────────────────────────────────────────────────────────────
allocate_ridership <- function(seg_df        = NULL,
                               seg_path      = NULL,
                               times_path,
                               route_map_path = NULL,
                               inbound_vals   = c("East","North"),
                               outbound_vals  = c("South","West"),
                               loop_vals      = c("Circular","Loop"),
                               seg2_path      = NULL,
                               seg_sheet      = 1,
                               times_sheet    = 6) {
  library(dplyr)
  library(readxl)
  library(stringr)
  library(purrr)
  
  # ── 1) Ingest ──────────────────────────────────────────────────────
  SEGMENTOS <- if (!is.null(seg_df)) {
    seg_df
  } else if (!is.null(seg2_path) && file.exists(seg2_path)) {
    read_xlsx(seg2_path, sheet = seg_sheet, .name_repair = "unique") %>%
      rename(SEGMENT2 = `APPROVED SEGMENTS`)
  } else if (!is.null(seg_path) && file.exists(seg_path)) {
    read_xlsx(seg_path, sheet = seg_sheet, .name_repair = "unique")
  } else {
    stop("No segment data provided.")
  }
  
  need_cols <- c("ETC_ROUTE_ID", "ETC_STOP_ID", "SEGMENT",
                 "Boarding", "TOTAL_OFF_adjusted")
  if (!all(need_cols %in% names(SEGMENTOS))) {
    if ("TOTAL_ON" %in% names(SEGMENTOS)) {
      SEGMENTOS <- SEGMENTOS %>% rename(Boarding = TOTAL_ON)
    } else {
      missing <- paste(setdiff(need_cols, names(SEGMENTOS)), collapse = ", ")
      stop("SEGMENTOS is missing required columns: ", missing)
    }
  }
  
  SEGMENTOS <- SEGMENTOS %>% apply_route_map(route_map_path)
  
  TIMES <- read_xlsx(times_path, sheet = times_sheet, .name_repair = "unique")
  
  # ── 2) TOD percentages ─────────────────────────────────────────────
  time_period_cols <- TIMES %>%
    dplyr::select(-1, -2, -dplyr::last_col()) %>% names()
  
  if (length(time_period_cols) == 0) {
    stop("No time period columns detected in TIMES. Check sheet index or headers.")
  }
  stopifnot("Total" %in% names(TIMES))
  
  TIMES <- TIMES %>%
    mutate(
      Total = as.numeric(Total),
      across(all_of(time_period_cols), as.numeric),
      across(all_of(time_period_cols), ~ .x / Total, .names = "{.col}_pct")
    )
  
  # ── 3) Parse LINE & DIRECTION ──────────────────────────────────────
  data1 <- SEGMENTOS %>%
    mutate(
      temp      = str_split(ETC_ROUTE_ID, "_"),
      LINE_DIR  = purrr::map_chr(temp, ~ .x[3]),
      DIRECTION = purrr::map_chr(temp, ~ .x[4]),
      LINE      = toupper(dplyr::coalesce(LINE_DIR, route_short_name))
    ) %>%
    dplyr::select(-temp, -LINE_DIR) %>%
    mutate(across(
      -matches("^(Boarding|Alighting|TOTAL_OFF_adjusted|TOTAL_ON|num_of_segments|TOTAL_OFF|TOTAL_RIDERSHIP|CUMM_TOTAL|TOTAL_DIRECTOPM|%_OF_RIDERS)$"),
      as.character
    ))
  
  # ── 4) TIMES + direction mapping ───────────────────────────────────
  line_col <- names(TIMES)[1]
  dir_col  <- names(TIMES)[2]
  
  data2 <- TIMES %>%
    mutate(
      LINE      = toupper(as.character(.data[[line_col]])),
      DIRECTION = as.character(.data[[dir_col]])
    ) %>%
    dplyr::select(-all_of(c(line_col, dir_col)))
  
  # ── 5) Merge & allocate ────────────────────────────────────────────
  merged <- data1 %>% left_join(data2, by = c("LINE", "DIRECTION"))
  pct_cols <- names(merged)[grep("_pct$", names(merged))]
  
  bad <- merged %>%
    group_by(LINE, DIRECTION) %>%
    summarise(has_pct = any(!is.na(across(all_of(pct_cols)))), .groups = "drop") %>%
    filter(!has_pct)
  if (nrow(bad)) {
    message("⚠️ No TIMES match for these LINE/DIRECTION pairs:\n",
            paste(bad$LINE, bad$DIRECTION, sep = " • ", collapse = "\n"))
  }
  
  for (i in seq_along(pct_cols)) {
    pct <- pct_cols[i]
    merged <- merged %>%
      mutate(
        !!paste0("X", i, "_ON")  := Boarding            * .data[[pct]],
        !!paste0("X", i, "_OFF") := TOTAL_OFF_adjusted * .data[[pct]]
      )
  }
  
  # ── 6) Totals ──────────────────────────────────────────────────────
  add_totals <- function(df, by_cols, suffix) {
    for (i in seq_along(pct_cols)) {
      df <- df %>%
        group_by(across(all_of(by_cols))) %>%
        mutate(
          !!paste0("TOTAL_ON",  suffix, i) := sum(.data[[paste0("X", i, "_ON")]],  na.rm = TRUE),
          !!paste0("TOTAL_OFF", suffix, i) := sum(.data[[paste0("X", i, "_OFF")]], na.rm = TRUE)
        ) %>%
        ungroup()
    }
    df
  }
  
  merged <- merged %>%
    add_totals("ETC_ROUTE_ID", "_") %>%
    mutate(ETC_ROUTE = sub("_\\d+$", "", ETC_ROUTE_ID)) %>%
    add_totals("ETC_ROUTE", "_ROUTE") %>%
    add_totals(character(0), "_SYS_LEV")
  
  # ── 7) Output ──────────────────────────────────────────────────────
  final <- merged %>%
    dplyr::select(ETC_ROUTE_ID, ETC_STOP_ID, SEGMENT,
                  matches("_ON$|_OFF$|^TOTAL_"))
  
  summary_tbl <- final %>%
    group_by(ETC_ROUTE_ID, SEGMENT) %>%
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
  
  list(full = final, summary = summary_tbl)
}



# ── Step 3 processor (drop-in) ──────────────────────────────────────────
process_data <- function(APC_DATA, OD, O2O, sampling, max_weight_factor = 60) {
  # ---- B') Harmonize OD/O2O FINAL_ROUTE_DIRECTION to match APC (e.g., "73_01") ----
  normalize_frd <- function(x) {
    x0 <- as.character(x)
    x0 <- stringr::str_squish(x0)
    passthru <- grepl("^\\d+_(00|01|02)$", x0)
    if (all(passthru)) return(x0)
    num <- stringr::str_extract(x0, "^\\s*\\d+")
    bracket_txt <- stringr::str_extract(x0, "\\[[^\\]]+\\]")
    base_txt    <- ifelse(is.na(bracket_txt), x0, bracket_txt)
    base_txt    <- stringr::str_to_lower(gsub("\\[|\\]", "", base_txt))
    code <- dplyr::case_when(
      grepl("\\bin\\b|inbound|ib", base_txt) ~ "00",
      grepl("\\bout\\b|outbound|ob", base_txt) ~ "01",
      grepl("loop|circular|circ", base_txt) ~ "02",
      TRUE ~ NA_character_
    )
    out <- ifelse(!is.na(num) & !is.na(code), paste0(num, "_", code), NA_character_)
    ifelse(is.na(out), x0, out)
  }
  
  OD$FINAL_ROUTE_DIRECTION  <- normalize_frd(as.character(OD$FINAL_ROUTE_DIRECTION))
  O2O$FINAL_ROUTE_DIRECTION <- normalize_frd(as.character(O2O$FINAL_ROUTE_DIRECTION))
  
  # --- quick diag: how many route-directions match after normalization?
  apc_keys <- APC_DATA %>%
    dplyr::mutate(
      tmp = strsplit(as.character(ETC_ROUTE_ID), "_", TRUE),
      line_dir = vapply(tmp, function(x) if (length(x) >= 3) x[3] else NA_character_, character(1)),
      dir_code = vapply(tmp, function(x) if (length(x) >= 4) x[4] else NA_character_, character(1)),
      line_id  = stringr::str_extract(line_dir, "^[0-9]+"),
      FRD_APC  = dplyr::if_else(!is.na(line_id), paste0(line_id, "_", dir_code), paste0(line_dir, "_", dir_code))
    ) %>%
    dplyr::distinct(FRD_APC) %>% dplyr::filter(!is.na(FRD_APC)) %>% dplyr::pull()
  
  OD  <- OD  %>% dplyr::filter(FINAL_ROUTE_DIRECTION %in% apc_keys)
  O2O <- O2O %>% dplyr::filter(FINAL_ROUTE_DIRECTION %in% apc_keys)
  
  od_keys <- OD %>% dplyr::distinct(FINAL_ROUTE_DIRECTION) %>% dplyr::pull()
  message(sprintf("Route-direction keys → APC:%d • OD:%d • overlap:%d",
                  length(unique(apc_keys)), length(unique(od_keys)),
                  length(intersect(apc_keys, od_keys))))
  
  apc_frds <- unique(apc_keys)
  od_frds  <- unique(OD$FINAL_ROUTE_DIRECTION)
  missing_in_apc <- setdiff(od_frds, apc_frds)
  if (length(missing_in_apc)) {
    message("Filtering out OD rows for FRDs not in APC (sample): ",
            paste(head(missing_in_apc, 10), collapse=", "),
            if (length(missing_in_apc) > 10) " ..." else "")
    OD  <- OD  %>% dplyr::filter(FINAL_ROUTE_DIRECTION %in% apc_frds)
    O2O <- O2O %>% dplyr::filter(FINAL_ROUTE_DIRECTION %in% apc_frds)
  }
  
  # Build per-line direction availability from APC
  apc_dir_map <- APC_DATA %>%
    dplyr::mutate(
      tmp = strsplit(as.character(ETC_ROUTE_ID), "_", fixed = TRUE),
      line_dir = vapply(tmp, function(x) if (length(x) >= 3) x[3] else NA_character_, character(1)),
      dir_code = vapply(tmp, function(x) if (length(x) >= 4) x[4] else NA_character_, character(1)),
      line_id  = stringr::str_extract(line_dir, "^[0-9]+"),
      FRD_APC  = dplyr::if_else(!is.na(line_id),
                                paste0(line_id, "_", dir_code),
                                paste0(line_dir, "_", dir_code))
    ) %>%
    dplyr::filter(!is.na(FRD_APC)) %>%
    dplyr::mutate(line = sub("_.*$", "", FRD_APC),
                  dir  = sub("^.*_", "", FRD_APC)) %>%
    dplyr::distinct(line, dir) %>%
    dplyr::group_by(line) %>%
    dplyr::summarise(dirs = list(sort(unique(dir))), .groups="drop")
  
  # remap OD/O2O xx_02 to a single available direction when possible
  remap_loop <- function(frd_vec) {
    frd <- as.character(frd_vec)
    is_loop <- grepl("^\\d+_02$", frd)
    if (!any(is_loop)) return(frd)
    line <- sub("_.*$", "", frd[is_loop])
    m <- match(line, apc_dir_map$line)
    chosen <- mapply(function(i, ln) {
      if (is.na(i)) return(NA_character_)
      d <- apc_dir_map$dirs[[i]]
      if (length(d) == 1) paste0(ln, "_", d) else NA_character_
    }, m, line, SIMPLIFY = TRUE)
    frd[is_loop] <- ifelse(is.na(chosen), frd[is_loop], chosen)
    frd
  }
  
  OD$FINAL_ROUTE_DIRECTION  <- remap_loop(OD$FINAL_ROUTE_DIRECTION)
  O2O$FINAL_ROUTE_DIRECTION <- remap_loop(O2O$FINAL_ROUTE_DIRECTION)
  
  # helper: latinify a column iff it exists
  latinify <- function(df, col) {
    if (col %in% names(df)) {
      df[[col]] <- stringi::stri_trans_general(as.character(df[[col]]), "latin-ascii")
    }
    df
  }
  
  # === A) Ensure/derive FINAL_ROUTE_DIRECTION ===========================
  if (!"FINAL_ROUTE_DIRECTION" %in% names(APC_DATA)) {
    if (!"ETC_ROUTE_ID" %in% names(APC_DATA)) {
      stop("APC_DATA must have either FINAL_ROUTE_DIRECTION or ETC_ROUTE_ID.")
    }
    tmp <- strsplit(as.character(APC_DATA$ETC_ROUTE_ID), "_", fixed = TRUE)
    line_dir <- vapply(tmp, function(x) if (length(x) >= 3) x[3] else NA_character_, character(1))
    dir_code <- vapply(tmp, function(x) if (length(x) >= 4) x[4] else NA_character_, character(1))
    line_id  <- stringr::str_extract(line_dir, "^[0-9]+")
    APC_DATA$FINAL_ROUTE_DIRECTION <- dplyr::case_when(
      !is.na(line_id)  & !is.na(dir_code) ~ paste0(line_id,  "_", dir_code),
      !is.na(line_dir) & !is.na(dir_code) ~ paste0(line_dir, "_", dir_code),
      TRUE                                ~ NA_character_
    )
  }
  
  # Require FINAL_ROUTE_DIRECTION in OD & O2O
  for (nm in c("OD","O2O")) {
    dat <- get(nm, inherits = FALSE)
    if (!"FINAL_ROUTE_DIRECTION" %in% names(dat)) {
      stop(nm, " is missing FINAL_ROUTE_DIRECTION. Add values like '14_00'.")
    }
  }
  
  # Normalize encoding + TIME_PERIOD types
  APC_DATA <- latinify(APC_DATA, "FINAL_ROUTE_DIRECTION")
  OD       <- latinify(OD,       "FINAL_ROUTE_DIRECTION") %>% dplyr::mutate(TIME_PERIOD = as.character(TIME_PERIOD))
  O2O      <- latinify(O2O,      "FINAL_ROUTE_DIRECTION") %>% dplyr::mutate(TIME_PERIOD = as.character(TIME_PERIOD))
  
  # === C) ROUTE_SURVEY safety shim =====================================
  derive_survey <- function(df){
    if ("ROUTE_SURVEY" %in% names(df)) {
      df$ROUTE_SURVEY <- as.character(df$ROUTE_SURVEY); return(df)
    }
    if ("ROUTE_SURVEYED" %in% names(df)) {
      df <- dplyr::rename(df, ROUTE_SURVEY = ROUTE_SURVEYED)
      df$ROUTE_SURVEY <- as.character(df$ROUTE_SURVEY); return(df)
    }
    if ("ETC_ROUTE_ID" %in% names(df)) {
      rid  <- as.character(df$ETC_ROUTE_ID)
      base <- sub("_[^_]+$", "", rid)
      base[is.na(base) | base == ""] <- rid[is.na(base) | base == ""]
      df$ROUTE_SURVEY <- as.character(base);  return(df)
    }
    df$ROUTE_SURVEY <- "UNKNOWN"; df
  }
  APC_DATA <- derive_survey(APC_DATA)
  OD       <- derive_survey(OD)
  
  # === D) APC “X#_ON/OFF” normalization =================================
  apc_time_cols <- names(APC_DATA)[grepl("^X\\d+_(ON|OFF)$", names(APC_DATA))]
  APC_DATA_norm <- APC_DATA %>% dplyr::rename_with(~ sub("^X", "", .x), .cols = dplyr::all_of(apc_time_cols))
  
  on_cols   <- names(APC_DATA_norm)[grepl("^\\d+_ON$",  names(APC_DATA_norm))]
  off_cols  <- names(APC_DATA_norm)[grepl("^\\d+_OFF$", names(APC_DATA_norm))]
  tp_nums   <- sort(unique(as.integer(gsub("_.*$", "", c(on_cols, off_cols)))))
  if (length(tp_nums) == 0) stop("No APC time period columns found (expected X1_ON/X1_OFF etc).")
  
  # Clean OD / O2O
  OD <- OD %>%
    dplyr::filter(!is.na(BOARDING_SEG), !is.na(ALIGHTING_SEG)) %>%
    dplyr::mutate(
      BOARDING_SEG = as.character(BOARDING_SEG),
      ALIGHTING_SEG = as.character(ALIGHTING_SEG),
      TIME_PERIOD = as.character(TIME_PERIOD),
      FINAL_ROUTE_DIRECTION = as.character(FINAL_ROUTE_DIRECTION)
    )
  
  O2O <- O2O %>%
    dplyr::mutate(
      BOARDING_SEG = as.character(BOARDING_SEG),
      ALIGHTING_SEG = as.character(ALIGHTING_SEG),
      TIME_PERIOD = as.character(TIME_PERIOD),
      FINAL_ROUTE_DIRECTION = as.character(FINAL_ROUTE_DIRECTION)
    )
  
  # Make APC consistent
  APC_DATA_norm <- APC_DATA_norm %>%
    dplyr::mutate(
      FINAL_ROUTE_DIRECTION = as.character(FINAL_ROUTE_DIRECTION),
      ROUTE_SURVEY = as.character(ROUTE_SURVEY),
      Segment = as.character(if ("Segment" %in% names(.)) Segment else SEGMENT)
    )
  
  # Sorted ON/OFF columns
  on_cols  <- names(APC_DATA_norm)[grepl("^\\d+_ON$",  names(APC_DATA_norm))]
  off_cols <- names(APC_DATA_norm)[grepl("^\\d+_OFF$", names(APC_DATA_norm))]
  on_ord   <- on_cols[order(as.integer(sub("_ON$", "", on_cols)))]
  off_ord  <- off_cols[order(as.integer(sub("_OFF$", "", off_cols)))]
  
  APC_DATA2 <- APC_DATA_norm %>%
    dplyr::select(ROUTE_SURVEY, FINAL_ROUTE_DIRECTION, Segment, dplyr::all_of(on_ord), dplyr::all_of(off_ord)) %>%
    dplyr::group_by(FINAL_ROUTE_DIRECTION, Segment) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::matches("^\\d+_(ON|OFF)$"),
        ~ sum(as.numeric(.x), na.rm = TRUE)
      ),
      .groups = "drop"
    )
  
  # --- PERIOD HARMONIZATION + DIAGNOSTICS ---------------------------------
  apc_periods <- sort(unique(as.character(gsub("_.*$", "", names(APC_DATA2)[grepl("^\\d+_ON$", names(APC_DATA2))]))))
  
  canonize_tp <- function(tp, target = apc_periods) {
    tp_chr <- as.character(tp)
    if (all(tp_chr %in% target)) return(tp_chr)
    labs <- unique(tp_chr)
    map_tbl <- tibble::tibble(label = labs,
                              tp = target[pmin(seq_along(labs), length(target))])
    tibble::tibble(label = tp_chr) %>% dplyr::left_join(map_tbl, by = "label") %>% dplyr::pull(tp)
  }
  
  OD$TIME_PERIOD  <- canonize_tp(OD$TIME_PERIOD)
  O2O$TIME_PERIOD <- canonize_tp(O2O$TIME_PERIOD)
  
  OD  <- OD  %>% dplyr::filter(TIME_PERIOD %in% apc_periods)
  O2O <- O2O %>% dplyr::filter(TIME_PERIOD %in% apc_periods)
  
  OD_bad_tp  <- OD  %>% dplyr::filter(is.na(TIME_PERIOD)) %>% dplyr::distinct(FINAL_ROUTE_DIRECTION)
  O2O_bad_tp <- O2O %>% dplyr::filter(is.na(TIME_PERIOD)) %>% dplyr::distinct(FINAL_ROUTE_DIRECTION)
  if (nrow(OD_bad_tp))  message("OD rows with unmapped TIME_PERIOD dropped for FRDs: ",
                                paste(OD_bad_tp$FINAL_ROUTE_DIRECTION, collapse=", "))
  if (nrow(O2O_bad_tp)) message("O2O rows with unmapped TIME_PERIOD dropped for FRDs: ",
                                paste(O2O_bad_tp$FINAL_ROUTE_DIRECTION, collapse=", "))
  
  OD  <- OD  %>% dplyr::filter(!is.na(TIME_PERIOD))
  O2O <- O2O %>% dplyr::filter(!is.na(TIME_PERIOD))
  
  bad_tp <- setdiff(unique(OD$TIME_PERIOD), apc_periods)
  if (length(bad_tp)) message("OD TIME_PERIOD values not in APC periods (will be dropped): ", paste(bad_tp, collapse=", "))
  
  # 3) Segment universe check (per route)
  apc_seg_dom <- APC_DATA2 %>%
    dplyr::distinct(FINAL_ROUTE_DIRECTION, Segment) %>%
    dplyr::group_by(FINAL_ROUTE_DIRECTION) %>%
    dplyr::summarise(apc_segments = list(sort(unique(as.character(Segment)))), .groups="drop")
  
  od_seg_dom <- OD %>%
    dplyr::distinct(FINAL_ROUTE_DIRECTION, BOARDING_SEG) %>%
    dplyr::group_by(FINAL_ROUTE_DIRECTION) %>%
    dplyr::summarise(od_board = list(sort(unique(as.character(BOARDING_SEG)))), .groups="drop") %>%
    dplyr::full_join(
      OD %>% dplyr::distinct(FINAL_ROUTE_DIRECTION, ALIGHTING_SEG) %>%
        dplyr::group_by(FINAL_ROUTE_DIRECTION) %>%
        dplyr::summarise(od_alight = list(sort(unique(as.character(ALIGHTING_SEG)))), .groups="drop"),
      by="FINAL_ROUTE_DIRECTION"
    )
  
  seg_diag <- apc_seg_dom %>%
    dplyr::full_join(od_seg_dom, by="FINAL_ROUTE_DIRECTION") %>%
    dplyr::mutate(
      missing_in_apc_board  = purrr::map2_chr(od_board, apc_segments, ~paste(setdiff(.x %||% character(), .y %||% character()), collapse=", ")),
      missing_in_apc_alight = purrr::map2_chr(od_alight, apc_segments, ~paste(setdiff(.x %||% character(), .y %||% character()), collapse=", "))
    )
  
  quiet <- function(x) if (!is.null(x) && nzchar(x)) message(x)
  quiet(sprintf("Routes with OD BOARDING segments not in APC: %s",
                paste(seg_diag$FINAL_ROUTE_DIRECTION[nchar(seg_diag$missing_in_apc_board)>0], collapse=", ")))
  quiet(sprintf("Routes with OD ALIGHTING segments not in APC: %s",
                paste(seg_diag$FINAL_ROUTE_DIRECTION[nchar(seg_diag$missing_in_apc_alight)>0], collapse=", ")))
  
  seg_map_todo <- seg_diag %>%
    dplyr::select(FINAL_ROUTE_DIRECTION, missing_in_apc_board, missing_in_apc_alight) %>%
    dplyr::filter(nchar(missing_in_apc_board) > 0 | nchar(missing_in_apc_alight) > 0)
  
  if (nrow(seg_map_todo)) {
    fn <- file.path(tempdir(), paste0("segments_to_map_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
    readr::write_csv(seg_map_todo, fn)
    message("Wrote a segment mapping to-do CSV: ", fn)
  }
  
  # Helper to sum APC for a period
  safe_apc_sum <- function(df, period_prefix) {
    on_col  <- paste0(period_prefix, "ON")
    off_col <- paste0(period_prefix, "OFF")
    df %>%
      dplyr::select(3, dplyr::all_of(on_col), dplyr::all_of(off_col)) %>%
      dplyr::rename(Boarding = dplyr::all_of(on_col), Alighting = dplyr::all_of(off_col)) %>%
      dplyr::summarise(B = sum(Boarding,  na.rm = TRUE),
                       A = sum(Alighting, na.rm = TRUE)) %>%
      dplyr::mutate(AB = A + B)
  }
  
  # Compute APC by FRD × period once (Boardings only)
  APC_BY_PERIOD <- APC_DATA2 %>%
    tidyr::pivot_longer(
      cols = tidyselect::matches("^\\d+_ON$"),
      names_to = "TIME_PERIOD",
      values_to = "Boarding"
    ) %>%
    dplyr::mutate(TIME_PERIOD = as.character(sub("_ON$", "", TIME_PERIOD))) %>%
    dplyr::group_by(FINAL_ROUTE_DIRECTION, TIME_PERIOD) %>%
    dplyr::summarise(APC = sum(Boarding, na.rm = TRUE), .groups = "drop")
  
  combos <- APC_BY_PERIOD %>%
    dplyr::filter(APC > 0) %>%
    dplyr::select(FINAL_ROUTE_DIRECTION, TIME_PERIOD) %>%
    dplyr::arrange(FINAL_ROUTE_DIRECTION, TIME_PERIOD) %>%
    dplyr::mutate(period_prefix = paste0(TIME_PERIOD, "_"))
  
  message(sprintf("Combos with APC>0: %d", nrow(combos)))
  if (nrow(combos) > 0) print(head(combos, 10))
  
  run_ipf_once <- function(od_like, apc_like, time_period_chr) {
    tp <- as.integer(time_period_chr)
    on_cols_r  <- names(apc_like)[grepl("^\\d+_ON$",  names(apc_like))]
    off_cols_r <- names(apc_like)[grepl("^\\d+_OFF$", names(apc_like))]
    on_idx  <- which(as.integer(sub("_ON$",  "", on_cols_r))  == tp)
    off_idx <- which(as.integer(sub("_OFF$", "", off_cols_r)) == tp)
    if (!length(on_idx) || !length(off_idx)) stop("Time period ", tp, " not found in APC columns.")
    
    on_col  <- on_cols_r[on_idx]
    off_col <- off_cols_r[off_idx]
    
    Boarding  <- apc_like %>%
      dplyr::select(Segment, !!on_col) %>%
      dplyr::rename(val = !!on_col) %>%
      dplyr::group_by(Segment) %>%
      dplyr::summarise(Boarding = sum(as.numeric(val), na.rm = TRUE), .groups = "drop")
    
    Alighting <- apc_like %>%
      dplyr::select(Segment, !!off_col) %>%
      dplyr::rename(val = !!off_col) %>%
      dplyr::group_by(Segment) %>%
      dplyr::summarise(Alighting = sum(as.numeric(val), na.rm = TRUE), .groups = "drop")
    
    A1 <- Boarding %>% tidyr::pivot_wider(names_from = Segment, values_from = Boarding, values_fn = sum, values_fill = 0)
    A2 <- Alighting %>% tidyr::pivot_wider(names_from = Segment, values_from = Alighting, values_fn = sum, values_fill = 0)
    
    A1 <- A1 %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.numeric(.x)))
    A2 <- A2 %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.numeric(.x)))
    
    targets <- list(Boarding = A1, Alighting = A2)
    
    err <- NULL; out <- NULL; counter <- 0
    while (is.null(out) && counter < 100) {
      out <- tryCatch(ipfr::ipu(od_like, targets), error = function(e){ err <<- e$message; NULL })
      if (is.null(out) && !is.null(err)) {
        if (grepl("Alighting", err)) {
          catnum <- stringr::str_extract(err, "Alighting category (\\d+)") %>% stringr::str_remove_all("[^[:digit:]]")
          targets$Alighting <- targets$Alighting %>% dplyr::select(-tidyselect::contains(catnum))
        } else if (grepl("Boarding", err)) {
          catnum <- stringr::str_extract(err, "Boarding category (\\d+)") %>% stringr::str_remove_all("[^[:digit:]]")
          targets$Boarding <- targets$Boarding %>% dplyr::select(-tidyselect::contains(catnum))
        } else stop("IPF failed: ", err)
      }
      counter <- counter + 1
    }
    if (is.null(out)) stop("IPF failed to converge for TIME_PERIOD = ", tp)
    
    wt  <- out$weight_tbl
    tab <- questionr::wtd.table(wt$Boarding, wt$Alighting, weights = wt$weight) %>% as.data.frame.table()
    
    tab %>%
      dplyr::rename(Boarding = Var1, Alighting = Var2, Ridership = Freq) %>%
      dplyr::mutate(Boarding = as.character(Boarding), Alighting = as.character(Alighting))
  }
  
  # Run IPF across combos
  IPF_accum <- list()
  skip_log  <- tibble::tibble(FINAL_ROUTE_DIRECTION=character(),
                              TIME_PERIOD=character(),
                              reason=character())
  
  for (k in seq_len(nrow(combos))) {
    rd <- combos$FINAL_ROUTE_DIRECTION[k]
    tp <- combos$TIME_PERIOD[k]
    apc_r <- APC_DATA2 %>% dplyr::filter(FINAL_ROUTE_DIRECTION == rd)
    
    if (nrow(apc_r) == 0) {
      skip_log <- tibble::add_row(skip_log, FINAL_ROUTE_DIRECTION=rd, TIME_PERIOD=tp, reason="no APC rows for route")
      next
    }
    
    cats <- sort(unique(as.character(apc_r$Segment)))
    if (length(cats) < 2) {
      skip_log <- tibble::add_row(skip_log, FINAL_ROUTE_DIRECTION=rd, TIME_PERIOD=tp, reason="only 1 APC segment")
      next
    }
    
    OD_k  <- OD  %>% dplyr::filter(FINAL_ROUTE_DIRECTION == rd, TIME_PERIOD == tp) %>%
      dplyr::transmute(Boarding = as.character(BOARDING_SEG), Alighting = as.character(ALIGHTING_SEG))
    O2O_k <- O2O %>% dplyr::filter(FINAL_ROUTE_DIRECTION == rd, TIME_PERIOD == tp) %>%
      dplyr::transmute(Boarding = as.character(BOARDING_SEG), Alighting = as.character(ALIGHTING_SEG))
    
    seed <- if (nrow(O2O_k) > 0) O2O_k else OD_k
    if (nrow(seed) == 0) {
      skip_log <- tibble::add_row(skip_log, FINAL_ROUTE_DIRECTION=rd, TIME_PERIOD=tp, reason="no OD/O2O for period")
      next
    }
    
    seed_in <- seed %>% dplyr::filter(Boarding %in% cats, Alighting %in% cats)
    if (nrow(seed_in) == 0) {
      skip_log <- tibble::add_row(skip_log, FINAL_ROUTE_DIRECTION=rd, TIME_PERIOD=tp, reason="OD pairs not in APC segments")
      next
    }
    
    apc_sum <- safe_apc_sum(apc_r, paste0(tp, "_"))
    if (apc_sum$AB == 0) {
      skip_log <- tibble::add_row(skip_log, FINAL_ROUTE_DIRECTION=rd, TIME_PERIOD=tp, reason="APC=0 for period")
      next
    }
    
    res <- tryCatch(run_ipf_once(seed_in, apc_r, tp),
                    error = function(e){
                      skip_log <<- tibble::add_row(skip_log, FINAL_ROUTE_DIRECTION=rd,
                                                   TIME_PERIOD=tp, reason=paste0("IPF error: ", e$message))
                      NULL
                    })
    if (is.null(res)) next
    
    count_tbl <- table(seed_in$Boarding, seed_in$Alighting) %>%
      as.data.frame.table(stringsAsFactors = FALSE) %>%
      dplyr::rename(Boarding = Var1, Alighting = Var2, Count = Freq) %>%
      dplyr::mutate(Boarding = as.character(Boarding), Alighting = as.character(Alighting))
    
    full_grid <- expand.grid(Boarding = cats, Alighting = cats, stringsAsFactors = FALSE)
    
    merged <- full_grid %>%
      dplyr::left_join(res,       by = c("Boarding","Alighting")) %>%
      dplyr::left_join(count_tbl, by = c("Boarding","Alighting")) %>%
      dplyr::mutate(
        Count = tidyr::replace_na(Count, 0),
        WEIGHT = ifelse(Count > 0, Ridership / Count, NA_real_),
        FINAL_ROUTE_DIRECTION = rd,
        TIME_PERIOD = tp
      )
    
    IPF_accum[[length(IPF_accum)+1]] <- merged
  }
  
  if (nrow(skip_log)) {
    message("IPF skip summary:")
    print(skip_log %>% dplyr::count(reason, sort = TRUE))
    print(skip_log %>% dplyr::arrange(reason, FINAL_ROUTE_DIRECTION, TIME_PERIOD), n = 30)
  }
  
  IPF_final <- if (length(IPF_accum)) dplyr::bind_rows(IPF_accum) else tibble::tibble(
    FINAL_ROUTE_DIRECTION = character(), TIME_PERIOD = character(),
    Boarding = integer(), Alighting = integer(),
    Ridership = numeric(), Count = integer(), WEIGHT = numeric()
  )
  
  # Build survey map
  rs_map <- OD %>%
    dplyr::mutate(ROUTE_SURVEY = if ("ROUTE_SURVEY" %in% names(.)) as.character(ROUTE_SURVEY) else NA_character_) %>%
    dplyr::select(FINAL_ROUTE_DIRECTION, ROUTE_SURVEY) %>%
    dplyr::filter(!is.na(ROUTE_SURVEY) & nzchar(ROUTE_SURVEY)) %>%
    dplyr::distinct()
  
  if (nrow(rs_map) == 0) {
    rs_map <- APC_DATA %>%
      dplyr::mutate(
        FINAL_ROUTE_DIRECTION = as.character(FINAL_ROUTE_DIRECTION),
        ROUTE_SURVEY = if ("ROUTE_SURVEY" %in% names(.)) as.character(ROUTE_SURVEY) else NA_character_,
        ETC_ROUTE_ID = as.character(ETC_ROUTE_ID)
      ) %>%
      dplyr::transmute(
        FINAL_ROUTE_DIRECTION,
        ROUTE_SURVEY = dplyr::coalesce(ROUTE_SURVEY, sub("_[^_]+$", "", ETC_ROUTE_ID))
      ) %>%
      dplyr::distinct()
  }
  
  # Attach ROUTE_SURVEY
  merg2 <- IPF_final %>%
    dplyr::rename(BOARDING_SEG = Boarding, ALIGHTING_SEG = Alighting) %>%
    dplyr::mutate(
      FINAL_ROUTE_DIRECTION = as.character(FINAL_ROUTE_DIRECTION),
      TIME_PERIOD = as.character(TIME_PERIOD)
    ) %>%
    dplyr::left_join(rs_map, by = "FINAL_ROUTE_DIRECTION") %>%
    dplyr::mutate(ROUTE_SURVEY = dplyr::coalesce(ROUTE_SURVEY, "UNKNOWN")) %>%
    dplyr::select(
      ROUTE_SURVEY, FINAL_ROUTE_DIRECTION, TIME_PERIOD,
      BOARDING_SEG, ALIGHTING_SEG, Ridership, Count, WEIGHT
    )
  
  # ── APC totals (route level, boardings only) ──────────────────────────
  APC_COMP_ALL <- APC_DATA_norm %>%
    dplyr::mutate(APC_row = rowSums(dplyr::select(., tidyselect::ends_with("_ON")), na.rm = TRUE)) %>%
    dplyr::group_by(FINAL_ROUTE_DIRECTION) %>%
    dplyr::summarise(APC = sum(APC_row, na.rm = TRUE), .groups = "drop")
  
  # ── Build APC_BY_PERIOD (Boardings only) ──────────────────────────────
  APC_BY_PERIOD <- APC_DATA2 %>%
    tidyr::pivot_longer(
      cols      = tidyselect::matches("^\\d+_ON$"),
      names_to  = "TIME_PERIOD",
      values_to = "Boarding"
    ) %>%
    dplyr::mutate(TIME_PERIOD = as.character(sub("_ON$", "", TIME_PERIOD))) %>%
    dplyr::group_by(FINAL_ROUTE_DIRECTION, TIME_PERIOD) %>%
    dplyr::summarise(APC = sum(Boarding, na.rm = TRUE), .groups = "drop")
  
  # ── Balance per FRD×Period to APC_BY_PERIOD ───────────────────────────
  if (nrow(merg2)) {
    diff_tp <- merg2 %>%
      dplyr::group_by(FINAL_ROUTE_DIRECTION, TIME_PERIOD) %>%
      dplyr::summarise(OD1 = sum(Ridership, na.rm = TRUE), .groups = "drop") %>%
      dplyr::full_join(APC_BY_PERIOD, by = c("FINAL_ROUTE_DIRECTION","TIME_PERIOD")) %>%
      dplyr::mutate(APC = tidyr::replace_na(APC, 0),
                    OD1 = tidyr::replace_na(OD1, 0),
                    gap = APC - OD1)
    
    merg3 <- merg2 %>%
      dplyr::left_join(diff_tp, by = c("FINAL_ROUTE_DIRECTION","TIME_PERIOD")) %>%
      dplyr::group_by(FINAL_ROUTE_DIRECTION, TIME_PERIOD) %>%
      dplyr::mutate(
        npos = sum(!is.na(Ridership) & Ridership != 0),
        adj  = ifelse(npos > 0, gap / npos, 0),
        Ridership = ifelse(!is.na(Ridership) & Ridership != 0, Ridership + adj, Ridership)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-gap, -npos, -adj)
  } else {
    merg3 <- merg2
  }
  
  # ── Guard empty merg3 ─────────────────────────────────────────────────
  empty_x3 <- tibble::tibble(
    ROUTE_SURVEY = character(),
    FINAL_ROUTE_DIRECTION = character(),
    TIME_PERIOD = character(),
    BOARDING_SEG = character(),
    ALIGHTING_SEG = character(),
    Ridership = numeric(),
    Count = integer(),
    WEIGHT = numeric()
  )
  merg3 <- if (exists("merg3") && nrow(merg3)) merg3 else empty_x3
  
  # ── Build WEIGHT table (keep FRD!) ────────────────────────────────────
  WEIGHT <- merg3 %>%
    dplyr::select(ROUTE_SURVEY, FINAL_ROUTE_DIRECTION, TIME_PERIOD, BOARDING_SEG, ALIGHTING_SEG, Ridership) %>%
    dplyr::rename(WEIGHT = Ridership)
  
  # ── Limit APC to FRD×TP actually present in output ────────────────────
  APC_MATCHED <- WEIGHT %>%
    dplyr::filter(!is.na(WEIGHT) & WEIGHT != 0) %>%
    dplyr::distinct(FINAL_ROUTE_DIRECTION, TIME_PERIOD) %>%
    dplyr::inner_join(APC_BY_PERIOD, by = c("FINAL_ROUTE_DIRECTION","TIME_PERIOD"))
  
  APC_COMP_MATCHED <- APC_MATCHED %>%
    dplyr::group_by(FINAL_ROUTE_DIRECTION) %>%
    dplyr::summarise(APC = sum(APC, na.rm = TRUE), .groups = "drop")
  
  # ── Route-level closing pass (ensure exact route totals) ──────────────
  route_gap <- WEIGHT %>%
    dplyr::group_by(FINAL_ROUTE_DIRECTION) %>%
    dplyr::summarise(OD_WEIGHT = sum(WEIGHT, na.rm = TRUE), .groups = "drop") %>%
    dplyr::full_join(APC_COMP_MATCHED, by = "FINAL_ROUTE_DIRECTION") %>%
    dplyr::mutate(APC = tidyr::replace_na(APC, 0),
                  OD_WEIGHT = tidyr::replace_na(OD_WEIGHT, 0),
                  gap = APC - OD_WEIGHT) %>%
    dplyr::select(FINAL_ROUTE_DIRECTION, gap)
  
  merg4 <- merg3 %>%
    dplyr::left_join(route_gap, by = "FINAL_ROUTE_DIRECTION") %>%
    dplyr::group_by(FINAL_ROUTE_DIRECTION) %>%
    dplyr::mutate(
      npos = sum(!is.na(Ridership) & Ridership != 0),
      adj  = ifelse(npos > 0, gap / npos, 0),
      Ridership = ifelse(!is.na(Ridership) & Ridership != 0, Ridership + adj, Ridership)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-gap, -npos, -adj)
  
  # ── Final WEIGHT (post reconciliation) ─────────────────────────────────
  WEIGHT_final <- merg4 %>%
    dplyr::select(ROUTE_SURVEY, FINAL_ROUTE_DIRECTION, TIME_PERIOD, BOARDING_SEG, ALIGHTING_SEG, Ridership) %>%
    dplyr::rename(WEIGHT = Ridership)
  
  # ── Period-level comparison (xxxx) using WEIGHT_final ─────────────────
  xxxx <- APC_BY_PERIOD %>%
    dplyr::rename(APC_Boarding = APC) %>%
    dplyr::full_join(
      WEIGHT_final %>%
        dplyr::group_by(FINAL_ROUTE_DIRECTION, TIME_PERIOD) %>%
        dplyr::summarise(IPF_Ridership = sum(WEIGHT, na.rm = TRUE), .groups = "drop"),
      by = c("FINAL_ROUTE_DIRECTION","TIME_PERIOD")
    ) %>%
    dplyr::mutate(APC_Boarding  = tidyr::replace_na(APC_Boarding, 0),
                  IPF_Ridership = tidyr::replace_na(IPF_Ridership, 0))
  
  balance2 <- xxxx %>%
    dplyr::group_by(FINAL_ROUTE_DIRECTION, TIME_PERIOD) %>%
    dplyr::summarise(
      APC      = max(APC_Boarding,   na.rm = TRUE),
      APC2     = max(IPF_Ridership,  na.rm = TRUE),
      APCDIFF2 = ifelse(is.finite(APC - APC2), (APC - APC2), 0),
      .groups  = "drop"
    )
  
  # ── Route-level DIFF2 against matched APC only ────────────────────────
  DIFF2 <- WEIGHT_final %>%
    dplyr::group_by(FINAL_ROUTE_DIRECTION) %>%
    dplyr::summarise(OD_WEIGHT = sum(WEIGHT, na.rm = TRUE), .groups = "drop") %>%
    dplyr::full_join(APC_COMP_MATCHED, by = "FINAL_ROUTE_DIRECTION") %>%
    dplyr::mutate(
      APC       = tidyr::replace_na(APC, 0),
      OD_WEIGHT = tidyr::replace_na(OD_WEIGHT, 0),
      diff      = APC - OD_WEIGHT
    )
  
  # ──────────────────────────────────────────────────────────────────────
  # >>> Legacy-style 'collapse' weights (x3 parity with older pipeline) <<<
  # Build collapse = Ridership/Count with smart fallbacks (col → row → all)
  calc_collapse <- merg4 %>%
    dplyr::mutate(
      collapse_raw = ifelse(Count > 0, Ridership / Count, NA_real_)
    ) %>%
    dplyr::group_by(FINAL_ROUTE_DIRECTION, TIME_PERIOD, ALIGHTING_SEG) %>%
    dplyr::mutate(
      col_ratio = ifelse(sum(Count, na.rm = TRUE) > 0,
                         sum(Ridership, na.rm = TRUE) / sum(Count, na.rm = TRUE),
                         NA_real_)
    ) %>% dplyr::ungroup() %>%
    dplyr::group_by(FINAL_ROUTE_DIRECTION, TIME_PERIOD, BOARDING_SEG) %>%
    dplyr::mutate(
      row_ratio = ifelse(sum(Count, na.rm = TRUE) > 0,
                         sum(Ridership, na.rm = TRUE) / sum(Count, na.rm = TRUE),
                         NA_real_)
    ) %>% dplyr::ungroup() %>%
    dplyr::group_by(FINAL_ROUTE_DIRECTION, TIME_PERIOD) %>%
    dplyr::mutate(
      all_ratio = ifelse(sum(Count, na.rm = TRUE) > 0,
                         sum(Ridership, na.rm = TRUE) / sum(Count, na.rm = TRUE),
                         NA_real_)
    ) %>% dplyr::ungroup() %>%
    dplyr::mutate(
      collapse = collapse_raw,
      collapse = dplyr::if_else(is.na(collapse) | !is.finite(collapse) | collapse == 0, col_ratio, collapse),
      collapse = dplyr::if_else(is.na(collapse) | !is.finite(collapse) | collapse == 0, row_ratio, collapse),
      collapse = dplyr::if_else(is.na(collapse) | !is.finite(collapse) | collapse == 0, all_ratio, collapse)
    )
  
  # Final x3 (legacy-compatible) + QA flag
  x3 <- calc_collapse %>%
    dplyr::select(
      ROUTE_SURVEY, FINAL_ROUTE_DIRECTION, TIME_PERIOD,
      BOARDING_SEG, ALIGHTING_SEG, Count, Ridership, collapse
    ) %>%
    dplyr::mutate(
      `needs human fixing` = dplyr::if_else(is.na(collapse) | !is.finite(collapse) | collapse <= 0, 1L, 0L)
    ) %>%
    dplyr::arrange(FINAL_ROUTE_DIRECTION, TIME_PERIOD, BOARDING_SEG, ALIGHTING_SEG)
  
  # Weight table using collapse (legacy app style)
  WEIGHT_collapse <- x3 %>%
    dplyr::select(ROUTE_SURVEY, TIME_PERIOD, BOARDING_SEG, ALIGHTING_SEG, collapse) %>%
    dplyr::rename(WEIGHT = collapse)
  
  # Optional: DIFF2 using collapse (route-level check vs APC)
  APC_COMP_route <- APC_DATA_norm %>%
    dplyr::mutate(APC_row = rowSums(dplyr::select(., tidyselect::ends_with("_ON")), na.rm = TRUE)) %>%
    dplyr::group_by(FINAL_ROUTE_DIRECTION) %>%
    dplyr::summarise(APC = sum(APC_row, na.rm = TRUE), .groups = "drop")
  
  DIFF2_collapse <- x3 %>%
    dplyr::group_by(FINAL_ROUTE_DIRECTION) %>%
    dplyr::summarise(OD_WEIGHT = sum(collapse, na.rm = TRUE), .groups = "drop") %>%
    dplyr::full_join(APC_COMP_route, by = "FINAL_ROUTE_DIRECTION") %>%
    dplyr::mutate(
      APC       = tidyr::replace_na(APC, 0),
      OD_WEIGHT = tidyr::replace_na(OD_WEIGHT, 0),
      diff = APC - OD_WEIGHT
    )
  # ──────────────────────────────────────────────────────────────────────
  
  # Return
  list(
    x3                = x3,                              # now includes 'collapse' + QA flag
    WEIGHT            = WEIGHT_final,                    # reconciled ridership weights (as before)
    WEIGHT_collapse   = WEIGHT_collapse,                 # legacy-style weights via 'collapse'
    DIFF2             = DIFF2,                           # route diff vs APC (Ridership-based)
    DIFF2_collapse    = DIFF2_collapse,                  # route diff vs APC (collapse-based)
    xxxx              = xxxx,
    balance2          = balance2,
    skip_log          = skip_log %>% dplyr::arrange(reason, FINAL_ROUTE_DIRECTION, TIME_PERIOD)
  )
}



# ── UI ─────────────────────────────────────────────────────────────────
ui <- fluidPage(useShinyjs(),
  theme = shinytheme("darkly"),
  tags$head(tags$style(HTML("body { padding: 0 !important; }"))),
  titlePanel("Route Segment Editor ➜ Ridership Allocation ➜ OD/IPF Expansion"),
  
  tabsetPanel(id = "wizard", type = "pills",
              
              # STEP 1 ----------------------------------------------------
              tabPanel("1) Edit Segments",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("dataset_upload", "Upload Default Segments", accept = c(".xlsx", ".xls")),
                           uiOutput("route_picker"),
                           br(),
                           downloadButton("download_csv", "Download edited CSV"),
                           br(), br(),
                           actionButton("confirm_next", "Confirm & Continue → Allocation", class = "btn-success")
                         ),
                         mainPanel(
                           fluidRow(
                             column(6,
                                    h4("Viewer"),
                                    leafletOutput("viewer_map", height = 600)
                             ),
                             column(
                               6,
                               h4("Editor"),
                               leafletOutput("editor_map", height = 600),
                               br(),
                               wellPanel(
                                 htmlOutput("sel_info"),
                                 uiOutput("segment_picker"),
                                 actionButton("apply_change", "Move selected → segment", class = "btn-primary"),
                                 br(), br(), hr(),
                                 
                                 fluidRow(
                                   column(
                                     6,
                                     checkboxInput("keep_sel", "Multi-select mode (toggle by click)", TRUE)
                                   ),
                                   column(
                                     6,
                                     actionButton("clear_sel", "Clear selection")
                                   )
                                 ),
                                 actionButton("select_view", "Select all in current view")
                               )
                             )
                           )
                         )
                       )
              ),
              
              
              # STEP 2 ----------------------------------------------------
              tabPanel("2) Allocate Ridership",
                       sidebarLayout(
                         sidebarPanel(
                           helpText("Uses the edited segments from Step 1."),
                           fileInput("times", "Upload Ridership by Time Period", accept = c(".xlsx", ".xls")),
                           fileInput("map",   "Route Rename CSV (optional: cols old,new)", accept = ".csv"),
                           actionButton("run_alloc", "Process & Display", class = "btn-primary"),
                           downloadButton("download_xlsx", "Download Excel")
                         ),
                         mainPanel(
                           uiOutput("alloc_guard"),
                           tabsetPanel(type = "pills",
                                       tabPanel("Final Data",    br(), DTOutput("tbl_final")),
                                       tabPanel("Route Summary", br(), DTOutput("tbl_summary"))
                           )
                         )
                       )
              ),
              
              # STEP 3 ----------------------------------------------------
              tabPanel("3) Expand (OD/IPF)",
                       sidebarLayout(
                         sidebarPanel(
                           helpText("Uses the APC produced in Step 2 → upload OD & O2O."),
                           fileInput("od_file",  "Upload OD (.csv/.xlsx)",  accept = c(".csv",".xls",".xlsx")),
                           fileInput("o2o_file", "Upload O2O (.csv/.xlsx)", accept = c(".csv",".xls",".xlsx")),
                           
                           fluidRow(
                             column(
                               6,
                               numericInput(
                                 "sampling",
                                 "Sampling value:",
                                 value = 5,
                                 min = 1
                               )
                             ),
                             column(
                               6,
                               shinyjs::disabled(
                                 numericInput(
                                   "max_weight_factor",
                                   "Maximum Weight Factor:",
                                   value = 60,
                                   min = 1
                                 )
                               )
                             )
                           ),
                           
                           
                           actionButton("run_expand", "Run Expansion", class = "btn-primary"),
                           br(), br(),
                           downloadButton("download_expand_xlsx", "Download Expansion Excel")
                         ),
                         mainPanel(
                           uiOutput("expand_guard"),
                           tabsetPanel(type = "pills",
                                       tabPanel("Collapse (x3)",            br(), DTOutput("x3_table")),
                                       tabPanel("OD Weights",               br(), DTOutput("weight_table")),
                                       tabPanel("OD vs APC (DIFF2)",        br(), DTOutput("diff2_table")),
                                       tabPanel("APC Adjusted (xxxx)",      br(), DTOutput("xxxx_table")),
                                       tabPanel("Balance by Period",        br(), DTOutput("balance2_table"))
                           )
                         )
                       )
              )
              
  ) # <-- CLOSE tabsetPanel
)   # <-- CLOSE fluidPage



# ── SERVER ─────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || identical(x, "")) y else x
  
  safemsg <- function(e) {
    cls <- paste(class(e), collapse = ",")
    paste0(conditionMessage(e) %||% "<no message>", " [", cls, "]")
  }
  
  # ===== STEP 1: EDITOR ===== -----------------------------------------------
  
  output$viewer_map <- renderLeaflet({ leaflet() |> addTiles() })
  output$editor_map <- renderLeaflet({ leaflet() |> addTiles() })
  
  raw_segments <- reactive({
    req(input$dataset_upload)                      # force upload
    path <- input$dataset_upload$datapath
    df <- read_excel(path)
    req(all(c("ETC_ROUTE_ID","SEGMENT","stop_lat","stop_lon") %in% names(df)))
    df %>%
      mutate(
        stop_lat  = as.numeric(stop_lat),
        stop_lon  = as.numeric(stop_lon),
        stop_id   = ifelse(!"stop_id" %in% names(.), paste0("stop_", dplyr::row_number()), as.character(stop_id)),
        stop_name = ifelse(!"stop_name" %in% names(.), paste0("Stop ", dplyr::row_number()), as.character(stop_name))
      )
  })
  
  zoom_to_df <- function(mapId, df, pad = 0.001) {
    if (is.null(df) || !nrow(df)) return(invisible(NULL))
    lon_rng <- range(df$stop_lon, na.rm = TRUE)
    lat_rng <- range(df$stop_lat, na.rm = TRUE)
    if (any(is.infinite(c(lon_rng, lat_rng)))) return(invisible(NULL))
    
    # If all points are the same, fitBounds won't zoom; use setView
    if (diff(lon_rng) == 0 && diff(lat_rng) == 0) {
      leafletProxy(mapId) |>
        setView(lng = lon_rng[1], lat = lat_rng[1], zoom = 15)
    } else {
      leafletProxy(mapId) |>
        fitBounds(lng1 = lon_rng[1] - pad, lat1 = lat_rng[1] - pad,
                  lng2 = lon_rng[2] + pad, lat2 = lat_rng[2] + pad)
    }
  }
  
  edited_all <- reactiveVal(NULL)
  observeEvent(raw_segments(), {
    edited_all(raw_segments() %>% mutate(row_uid = dplyr::row_number()))
  })  # <- no ignoreInit
  
  output$route_picker <- renderUI({
    df <- edited_all(); req(df)
    
    # Remember current selection (if any) without making it reactive
    current <- isolate(input$route_id)
    
    if ("ETC_ROUTE_NAME" %in% names(df)) {
      # One name per route ID (take the first non-NA name)
      route_df <- df %>%
        dplyr::filter(!is.na(ETC_ROUTE_ID)) %>%
        dplyr::group_by(ETC_ROUTE_ID) %>%
        dplyr::summarise(
          ETC_ROUTE_NAME = dplyr::first(na.omit(ETC_ROUTE_NAME)),
          .groups = "drop"
        ) %>%
        dplyr::arrange(ETC_ROUTE_ID)
      
      # Build labels like "1234_00 — Main St – Downtown"
      choice_values <- route_df$ETC_ROUTE_ID
      choice_labels <- ifelse(
        is.na(route_df$ETC_ROUTE_NAME) | route_df$ETC_ROUTE_NAME == "",
        route_df$ETC_ROUTE_ID,
        paste0(route_df$ETC_ROUTE_ID, " — ", route_df$ETC_ROUTE_NAME)
      )
      
    } else {
      # Fallback: only IDs available
      choice_values <- sort(unique(df$ETC_ROUTE_ID))
      choice_labels <- choice_values
    }
    
    # Keep previous selection if still valid; otherwise default to first
    selected_value <- if (!is.null(current) && current %in% choice_values) {
      current
    } else {
      choice_values[1]
    }
    
    selectInput(
      "route_id",
      if ("ETC_ROUTE_NAME" %in% names(df)) "Select route:" else "Select route (ID only):",
      choices  = stats::setNames(choice_values, choice_labels),
      selected = selected_value
    )
  })
  
  
  filtered_data <- reactive({
    df <- edited_all(); req(df, input$route_id)
    df %>% filter(ETC_ROUTE_ID == input$route_id)
  })
  
  selected_uids <- reactiveVal(integer(0))
  
  seg_palette <- reactive({
    df <- filtered_data()
    uniq <- sort(unique(df$SEGMENT))
    if (length(uniq) == 0) {
      # harmless placeholder palette
      colorFactor(c("#999999"), levels = "NA")
    } else {
      colorFactor(rainbow(length(uniq)), uniq)
    }
  })
  
  observeEvent(edited_all(), {
    df_all <- edited_all(); req(nrow(df_all))
    zoom_to_df("viewer_map", df_all)
    zoom_to_df("editor_map", df_all)
  })
  
  observeEvent(filtered_data(), {
    df <- filtered_data(); pal <- seg_palette()
    lp <- leafletProxy("viewer_map") |> clearMarkers() |> clearControls()
    if (nrow(df)) {
      lp |> addCircleMarkers(
        data = df, lng = ~stop_lon, lat = ~stop_lat,
        radius = 4, color = ~pal(SEGMENT), stroke = FALSE, fillOpacity = 0.8,
        popup = ~paste0("Stop: ", stop_name, "<br>Segment: ", SEGMENT)
      ) |> addLegend("bottomright", pal = pal, values = df$SEGMENT,
                     title = "Segment", opacity = 1)
      zoom_to_df("viewer_map", df)
    }
  })
  
  draw_editor_markers <- function() {
    df <- tryCatch(filtered_data(), error = function(e) NULL)
    if (is.null(df) || !nrow(df)) return(invisible(NULL))
    pal <- seg_palette(); sel <- selected_uids()
    leafletProxy("editor_map") |>
      clearMarkers() |> clearControls() |>
      addCircleMarkers(
        data    = df,
        lng     = ~stop_lon, lat = ~stop_lat,
        radius  = ifelse(df$row_uid %in% sel, 8, 5),
        
        # --- NEW BORDER COLOR LOGIC ---
        stroke  = TRUE,
        color   = ifelse(df$row_uid %in% sel, "black", "gray40"),   # border color
        weight  = ifelse(df$row_uid %in% sel, 3, 1),                # border thickness
        
        # --- KEEP YOUR ORIGINAL FILL COLORS ---
        fillColor = ~pal(SEGMENT),
        fillOpacity = 0.9,
        
        layerId = ~row_uid,
        popup   = ~paste0("Stop: ", stop_name, "<br>Segment: ", SEGMENT)
    ) |>
      addLegend("bottomright", pal = pal, values = df$SEGMENT, title = "Segment", opacity = 1)
  }
  
  observeEvent(filtered_data(), {
    draw_editor_markers()
    df <- filtered_data()
    zoom_to_df("editor_map", df)
    selected_uids(integer(0))
  })
  
  observeEvent(selected_uids(), {
    draw_editor_markers()
    n <- length(selected_uids())
    txt <- if (!n) "No stops selected." else sprintf("<b>%d</b> stop%s selected.", n, ifelse(n==1,"","s"))
    output$sel_info <- renderUI(HTML(txt))
  })
  
  observeEvent(input$editor_map_marker_click, {
    click <- input$editor_map_marker_click
    uid   <- suppressWarnings(as.integer(click$id))
    req(!is.na(uid))
    if (!is.null(click$lng) && !is.null(click$lat)) {
      leafletProxy("editor_map") |> setView(click$lng, click$lat, zoom = 15)
    }
    cur <- selected_uids()
    if (isTRUE(input$keep_sel)) {
      if (uid %in% cur) cur <- setdiff(cur, uid) else cur <- c(cur, uid)
    } else cur <- uid
    selected_uids(unique(cur))
  })
  
  observeEvent(input$select_view, {
    b  <- input$editor_map_bounds; req(b)
    df <- filtered_data(); req(nrow(df))
    inside <- df$stop_lon >= b$west & df$stop_lon <= b$east &
      df$stop_lat >= b$south & df$stop_lat <= b$north
    selected_uids(unique(c(selected_uids(), df$row_uid[inside])))
  })
  
  observeEvent(input$clear_sel, { selected_uids(integer(0)) })
  
  output$segment_picker <- renderUI({
    df <- filtered_data(); req(nrow(df))
    segs <- sort(unique(df$SEGMENT))
    selectInput("move_to_segment", "Move selected to segment:", choices = segs)
  })
  
  observeEvent(input$apply_change, {
    sel <- selected_uids(); req(length(sel))
    tgt <- input$move_to_segment; req(tgt)
    df_all <- edited_all(); req(nrow(df_all))
    edited_all(df_all %>% mutate(SEGMENT = ifelse(row_uid %in% sel, tgt, SEGMENT)))
    selected_uids(integer(0))
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("segments_edited_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) { write.csv(edited_all(), file, row.names = FALSE, na = "") }
  )
  
  confirmed_segments <- reactiveVal(NULL)
  observeEvent(input$confirm_next, {
    df <- edited_all(); req(df)
    confirmed_segments(df %>% dplyr::select(-row_uid))
    updateTabsetPanel(session, "wizard", selected = "2) Allocate Ridership")
  })
  
  # ===== STEP 2: ALLOCATION ===== -------------------------------------------
  output$alloc_guard <- renderUI({
    if (is.null(confirmed_segments()))
      HTML("<div style='color:#b00; font-weight:600;'>Confirm your edits in Step 1 first.</div>")
    else HTML("<div style='color:#2b7; font-weight:600;'>Using edited segments from Step 1.</div>")
  })
  
  processed <- eventReactive(input$run_alloc, {
    req(confirmed_segments(), input$times)
    tryCatch({
      allocate_ridership(
        seg_df         = confirmed_segments(),
        times_path     = input$times$datapath,
        route_map_path = if (is.null(input$map)) NULL else input$map$datapath,
        seg_sheet      = 1,
        times_sheet    = 6
      )
    }, error = function(e) {
      showNotification(paste("Allocation error:", e$message), type = "error", duration = NULL)
      validate(need(FALSE, e$message))
    })
  })
  
  # Filter columns for display in Step 2 (hide *_TOTAL_ON, *_TOTAL_OFF, *SYS_LEV*)
  final_display <- reactive({
    res <- processed(); req(res)
    res$full %>%
      dplyr::select(-dplyr::matches("TOTAL_ON|TOTAL_OFF|SYS_LEV"))
  })
  
  summary_display <- reactive({
    res <- processed(); req(res)
    res$summary %>%
      dplyr::select(-dplyr::matches("TOTAL_ON|TOTAL_OFF|SYS_LEV"))
  })
  
  output$tbl_final <- renderDT(
    final_display(),
    options = list(pageLength = 25, scrollX = TRUE)
  )
  
  output$tbl_summary <- renderDT(
    summary_display(),
    options = list(pageLength = 25, scrollX = TRUE)
  )
  
  
  output$download_xlsx <- downloadHandler(
    filename = function() paste0("ridership_allocation_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      res <- processed(); req(res)
      write_xlsx(list(Full_Data = res$full, Route_Summary = res$summary), path = file)
    }
  )
  
  # ===== STEP 3: EXPANSION (IPF) ===== --------------------------------------
  output$expand_guard <- renderUI({
    if (is.null(processed()))
      HTML("<div style='color:#b00; font-weight:600;'>Run Step 2 first to generate APC.</div>")
    else HTML("<div style='color:#2b7; font-weight:600;'>Using APC from Step 2 (Final Data).</div>")
  })
  
  read_any <- function(infile) {
    ext <- tools::file_ext(infile$name)
    if (ext == "csv") readr::read_csv(infile$datapath)
    else if (ext %in% c("xls","xlsx")) readxl::read_excel(infile$datapath)
    else stop("Unsupported file type: ", ext)
  }
  
  
  # Auto-update max_weight_factor based on sampling:
  # max_weight_factor = (1 / (sampling/100)) * 3 = (100 / sampling) * 3
  observeEvent(input$sampling, {
    s <- suppressWarnings(as.numeric(input$sampling))
    if (is.na(s) || s <= 0) return()
    
    new_max <- (100 / s) * 3
    updateNumericInput(
      session,
      "max_weight_factor",
      value = round(new_max, 2)
    )
  })
  
  
  expand_results <- eventReactive(input$run_expand, {
    if (is.null(processed())) {
      showNotification("Please run Step 2 first (Process & Display).", type = "error")
      validate(need(FALSE, "Run Step 2 first"))
    }
    req(input$od_file, input$o2o_file, input$sampling, input$max_weight_factor)
    
    APC <- processed()$full
    OD  <- read_any(input$od_file)
    O2O <- read_any(input$o2o_file)
    
    # ===== B) Preflight checks =====
    check_cols <- function(df, need, name){
      miss <- setdiff(need, names(df))
      if (length(miss)) stop(name, " is missing required columns: ", paste(miss, collapse = ", "))
    }
    
    need_apc <- c("ETC_ROUTE_ID","ETC_STOP_ID","SEGMENT")
    need_od  <- c("FINAL_ROUTE_DIRECTION","TIME_PERIOD","BOARDING_SEG","ALIGHTING_SEG")
    need_o2o <- c("FINAL_ROUTE_DIRECTION","TIME_PERIOD","BOARDING_SEG","ALIGHTING_SEG")
    
    check_cols(APC, need_apc, "APC_DATA (Step 2 output)")
    check_cols(OD,  need_od,  "OD")
    check_cols(O2O, need_o2o, "O2O")
    
    if (!any(grepl("^X\\d+_(ON|OFF)$", names(APC)))) {
      stop("APC_DATA must contain time-bucket columns like X1_ON/X1_OFF from Step 2.")
    }
    
    # Normalize types early (avoids cryptic errors later)
    OD  <- OD  %>% dplyr::mutate(TIME_PERIOD = as.character(TIME_PERIOD))
    O2O <- O2O %>% dplyr::mutate(TIME_PERIOD = as.character(TIME_PERIOD))
    
    # Optional: quick console breadcrumbs
    message(sprintf("APC cols: %d • OD rows: %d • O2O rows: %d",
                    ncol(APC), nrow(OD), nrow(O2O)))
    # ===== end preflight =====
    
    # ===== Big visible modal while expansion runs ======================
    showModal(
      modalDialog(
        title = NULL,
        easyClose = FALSE,
        footer = NULL,
        size = "m",
        tags$div(
          style = "text-align:center; padding: 25px 20px; background-color:#222; border-radius:8px;",
          tags$h2(
            "⚠️ IPF expansion is running...",
            style = "font-size: 28px; font-weight: 900; color: #ffeb3b; margin-bottom: 18px;"
          ),
          tags$div(
            "Please DO NOT close this tab or change steps until this message disappears.",
            style = "font-size: 18px; font-weight: 700; color: #ffffff; margin-bottom: 10px;"
          ),
          tags$div(
            "You can work in other windows while this finishes.",
            style = "font-size: 14px; color: #cccccc;"
          )
        )
      )
    )
    on.exit(removeModal(), add = TRUE)
    # ==================================================================
    
    withProgress(
      message = "Running IPF expansion...",
      detail  = "This may take a while depending on the size of APC, OD and O2O...",
      value   = 0,
      {
        tryCatch({
          process_data(
            APC_DATA          = APC,
            OD                = OD,
            O2O               = O2O,
            sampling          = input$sampling,
            max_weight_factor = input$max_weight_factor
          )
        }, error = function(e) {
          # Show full error — uses safemsg() helper defined earlier
          calls <- utils::tail(as.character(sys.calls()), 6)
          stack <- paste0("\nCall stack (last frames):\n- ", paste(calls, collapse = "\n- "))
          msg <- paste0("Expansion error: ", safemsg(e), stack)
          message(msg)
          showNotification(msg, type = "error", duration = NULL)
          validate(need(FALSE, msg))
        })
      }
    )
  })
  
  
  output$x3_table       <- renderDT({ req(expand_results()); expand_results()$x3 },
                                    options = list(pageLength = 25, scrollX = TRUE))
  output$weight_table   <- renderDT({ req(expand_results()); expand_results()$WEIGHT },
                                    options = list(pageLength = 25, scrollX = TRUE))
  output$diff2_table    <- renderDT({ req(expand_results()); expand_results()$DIFF2 },
                                    options = list(pageLength = 25, scrollX = TRUE))
  output$xxxx_table     <- renderDT({ req(expand_results()); expand_results()$xxxx },
                                    options = list(pageLength = 25, scrollX = TRUE))
  output$balance2_table <- renderDT({ req(expand_results()); expand_results()$balance2 },
                                    options = list(pageLength = 25, scrollX = TRUE))
  
  output$download_expand_xlsx <- downloadHandler(
    filename = function() paste0("ipf_expansion_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(expand_results())
      out <- expand_results()
      writexl::write_xlsx(
        list(
          X3               = out$x3,
          WEIGHT           = out$WEIGHT,
          DIFF2            = out$DIFF2,
          APC_ADJUSTED_xxxx= out$xxxx,
          BALANCE2         = out$balance2,
          APC_from_Step2   = processed()$full
        ),
        path = file
      )
    }
  )
}

# ── run ──────────────────────────────────────────────────────────────────────
shinyApp(ui, server)






