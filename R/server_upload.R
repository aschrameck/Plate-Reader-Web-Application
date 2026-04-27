#' Upload Screen Server Logic
#'
#' Manages file ingestion, validation, parsing, and initialization of plate data
#' for the Plate Reader Analysis workflow. This module is responsible for
#' transforming raw uploaded files into structured, analysis-ready plate objects.
#'
#' @details
#' This server component performs five primary responsibilities:
#'
#' **1. Upload Feedback & UX**
#' - Displays a loading spinner immediately when files are selected
#' - Replaces spinner with a success indicator upon completion
#' - Provides visual feedback using `shinyjs`
#'
#' **2. File Validation**
#' - Restricts uploads to supported formats: `.csv`, `.txt`, `.xlsx`
#' - Ensures files are non-empty
#' - Dynamically enables/disables progression based on validity
#'
#' **3. File Reading & Error Handling**
#' - Uses format-specific readers:
#'   - CSV → `read.csv()`
#'   - TXT → `read.table()`
#'   - XLSX → `readxl::read_xlsx()`
#' - Wraps reads in `tryCatch()` to prevent app crashes
#' - Displays user-facing error notifications on failure
#'
#' **4. Plate Detection & Extraction**
#' - Automatically detects one or more plates per file by:
#'   - Identifying contiguous non-empty row blocks
#'   - Identifying contiguous non-empty column blocks
#'   - Extracting all valid row/column block combinations
#' - Supports multiple plates embedded in a single file
#' - Preserves internal missing values (NAs) within detected plate regions
#'
#' **Header Handling**
#' - Optional (`input$has_header`)
#' - Detects and removes:
#'   - Column headers (numeric indices, e.g., 1–12)
#'   - Row headers (alphabetical labels, e.g., A–H)
#' - Uses strict cropping to isolate only the data matrix
#' - Falls back safely if header detection fails
#'
#' **5. Data Transformation & Initialization**
#' - Converts all values to numeric (non-numeric → NA)
#' - Reshapes wide plate format → long format:
#'   - Columns: `row`, `col`, `value`
#' - Assigns standardized row/column identifiers
#'
#' **Metadata Initialization**
#' Each well is initialized with default annotation fields:
#' - `is_control`, `is_blank`, `is_label`, `is_standard`
#' - `labels`, `control_groups`, `blanks` (list columns)
#' - `standards`, `standard_units`
#'
#' **Plate Naming**
#' - Plates are uniquely named using:
#'   `Plate_<index>_<filename>`
#'
#' **Final Output**
#' - All parsed plates are stored in a named list
#' - The reactive `plates()` object is updated
#' - Validation ensures at least one plate is successfully loaded
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param state ReactiveValues object storing global application state
#' @param plates ReactiveVal storing parsed plate data (named list)
#'
#' @return
#' NULL. This function operates via side effects:
#' - Updates `plates()` with parsed data
#' - Updates UI state via `shinyjs` and notifications
#'
#' @seealso server_inspect, main_server
#'
#' @family Upload Module

server_upload <- function(input, output, session, state, plates) {

  # ---- Allowed file extensions ----
  allowed_ext <- c("csv", "txt", "xlsx")

  # ---- Upload Spinner (Client-Side) ----
  # Shows a loading spinner immediately when files are selected
  shinyjs::runjs("
    $(document).on('change', '#data_files', function() {
      $('#upload_status').html(
        '<span class=\"spinner-border spinner-border-sm\"></span> Uploading'
      );
    });
  ")

  # Replace spinner with success indicator after upload completes
  observe({
    if (is.null(input$data_files)) {
      shinyjs::html("upload_status", "")
      return()
    }

    shinyjs::html(
      "upload_status",
      "<span class='upload-status success'>✓</span>"
    )
  })

  # ---- File Validation ----
  files_valid <- reactive({
    req(input$data_files)

    # Extract and normalize extensions
    ext <- tolower(tools::file_ext(input$data_files$name))

    # Check allowed extensions
    if (any(!ext %in% allowed_ext)) return(FALSE)

    # Ensure files are not empty
    if (any(input$data_files$size == 0)) return(FALSE)

    TRUE
  })

  # Enable/disable progression button based on validation
  observe({
    valid <- !is.null(input$data_files) && files_valid()
    shinyjs::toggleState("to_inspect", condition = valid)
  })

  # ---- File Parsing & Plate Extraction ----
  observeEvent(list(input$data_files, input$has_header), {

    req(files_valid())

    # Container for all parsed plates
    plate_list <- list()

    # ---- Helper: Find contiguous index runs ----
    # Converts logical vector into groups of consecutive TRUE indices
    get_runs <- function(x) {
      idx <- which(x)
      if (length(idx) == 0) return(list())
      splits <- cumsum(c(1, diff(idx) > 1))
      split(idx, splits)
    }

    # Iterate over uploaded files
    for (i in seq_len(nrow(input$data_files))) {

      path  <- input$data_files$datapath[i]
      fname <- input$data_files$name[i]
      name  <- tools::file_path_sans_ext(fname)
      ext   <- tolower(tools::file_ext(fname))

      # Read file safely
      raw <- tryCatch({
        switch(
          ext,
          csv  = read.csv(path, header = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA")),
          txt  = read.table(path, header = FALSE, fill = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA")),
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

      if (is.null(raw)) next

      # Normalize structure
      raw <- as.data.frame(raw, stringsAsFactors = FALSE)
      raw[raw == ""] <- NA

      # Detect occupied regions
      nonempty_rows <- apply(raw, 1, function(r) any(!is.na(r)))
      nonempty_cols <- apply(raw, 2, function(c) any(!is.na(c)))

      row_blocks <- get_runs(nonempty_rows)
      col_blocks <- get_runs(nonempty_cols)

      file_plate_count <- 1

      # Extract all plate candidates
      for (rb in row_blocks) {
        for (cb in col_blocks) {

          values <- raw[rb, cb, drop = FALSE]

          # Skip empty regions
          if (!any(!is.na(values))) next

          # Preserve full bounding rectangle
          values <- raw[min(rb):max(rb), min(cb):max(cb), drop = FALSE]

          if (nrow(values) == 0 || ncol(values) == 0) next

          # Header Handling (Optional)
          if (isTRUE(input$has_header)) {

            # First row/column as candidate headers
            header_row <- values[1, , drop = TRUE]
            header_col <- values[, 1, drop = TRUE]

            # Detect numeric column indices
            col_start <- which(!is.na(suppressWarnings(as.numeric(header_row))))[1]
            col_end   <- tail(which(!is.na(suppressWarnings(as.numeric(header_row)))), 1)

            # Detect row labels
            row_start <- which(!is.na(header_col) & header_col != "")[1]
            row_end   <- tail(which(!is.na(header_col) & header_col != ""), 1)

            # Abort if detection fails
            if (is.na(col_start) || is.na(col_end) ||
                is.na(row_start) || is.na(row_end)) next

            # Strict cropping to data-only region
            values <- values[
              row_start:row_end,
              col_start:col_end,
              drop = FALSE
            ]

            # Assign clean dimension names
            rownames(values) <- LETTERS[seq_len(nrow(values))]
            colnames(values) <- seq_len(ncol(values))

          } else {

            # Assign default indices if no headers
            rownames(values) <- LETTERS[seq_len(nrow(values))]
            colnames(values) <- seq_len(ncol(values))
          }

          # Type Conversion
          # Convert all entries to numeric; invalid values → NA
          values[] <- lapply(values, function(x) suppressWarnings(as.numeric(x)))

          # Reshape to Long Format
          plate <- values %>%
            tibble::rownames_to_column("row") %>%
            tidyr::pivot_longer(
              -row,
              names_to = "col",
              values_to = "value"
            )

          plate$row <- as.character(plate$row)
          plate$col <- as.integer(plate$col)

          # ---- Initialize Metadata Fields ----
          plate$is_control     <- FALSE
          plate$is_blank       <- FALSE
          plate$is_label       <- FALSE
          plate$is_standard    <- FALSE

          plate$labels         <- replicate(nrow(plate), character(0), simplify = FALSE)
          plate$control_groups <- replicate(nrow(plate), character(0), simplify = FALSE)
          plate$blanks         <- replicate(nrow(plate), character(0), simplify = FALSE)

          plate$standards      <- NA_real_
          plate$standard_units <- NA_character_

          # Assign Plate Name
          plate_name <- paste0("Plate_", file_plate_count, "_", name)

          plate$plate <- plate_name
          plate_list[[plate_name]] <- plate

          file_plate_count <- file_plate_count + 1
        }
      }
    }

    # ---- Final Validation ----
    validate(
      need(length(plate_list) > 0, "No valid plates could be loaded.")
    )

    # Update reactive plate storage
    plates(plate_list)

  },
  ignoreInit = FALSE)
}
