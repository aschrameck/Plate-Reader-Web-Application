path  <- "C:\\Users\\aubre\\OneDrive\\Documents\\Capstone Project\\HaileyAFPrestoblue.xlsx"
fname <- "HaileyAFPrestoblue.xlsx"
ext   <- tools::file_ext(fname)
name  <- tools::file_path_sans_ext(fname)
has_headers <- TRUE
plate_list <- list()

# --- Read File ---
raw <- tryCatch({
  switch(
    ext,
    csv  = read.csv(path, header = FALSE, stringsAsFactors = FALSE),
    txt  = read.delim(path, header = FALSE, stringsAsFactors = FALSE),
    xlsx = readxl::read_xlsx(path, col_names = FALSE)
  )
}, error = function(e) {
  showNotification(
    paste("Failed to read", fname, ":", e$message),
    type = "error",
    duration = 8
  )
  return(NULL)
})

# Failed to read error
if (is.null(raw)) next

# Save as data frame
values <- as.data.frame(raw)

# -- Handle headers ---
if(isTRUE(has_headers)) {
  # Extract labels
  new_colnames <- as.character(values[1, -1])
  new_rownames <- as.character(values[-1, 1])

  # Remove header row + column
  values <- values[-1, -1, drop = FALSE]

  # Assign labels (remove empty columns)
  new_colnames[new_colnames == "" | is.na(new_colnames)] <- NA
  keep_cols <- !is.na(new_colnames)

  values       <- values[, keep_cols, drop = FALSE]
  new_colnames <- new_colnames[keep_cols]

  colnames(values) <- new_colnames
  rownames(values) <- new_rownames

} else {
  rownames(values) <- LETTERS[seq_len(nrow(values))]
  colnames(values) <- seq_len(ncol(values))
}

# --- Convert to long format ---
plate <- values %>%
  tibble::rownames_to_column("row") %>%
  tidyr::pivot_longer(
    -row,
    names_to = "col",
    values_to = "value"
  )

plate$row   <- as.character(plate$row)
plate$col   <- as.integer(plate$col)

plate$label          <- NA_character_
plate$is_control     <- FALSE
plate$is_blank       <- FALSE
plate$control_groups <- replicate(nrow(plate), character(0), simplify = FALSE)
plate$blanks         <- replicate(nrow(plate), character(0), simplify = FALSE)
plate$plate          <- name

plate_list[[name]] <- plate
