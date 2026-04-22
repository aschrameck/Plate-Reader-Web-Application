#' Upload Screen Server Logic
#'
#' Handles file input, parsing, and initialization of plate data for the app.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param state ReactiveValues object storing application state
#' @param plates Reactive object for storing loaded plate data
#'
#' @return NULL; updates `state` and `plates` reactively

server_upload <- function(input, output, session, state, plates) {

  allowed_ext <- c("csv", "txt", "xlsx")

  # --- Upload spinner ---
  shinyjs::runjs("
    $(document).on('change', '#data_files', function() {
      $('#upload_status').html(
        '<span class=\"spinner-border spinner-border-sm\"></span> Uploading'
      );
    });
  ")

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

  # ---- File validation ----
  files_valid <- reactive({
    req(input$data_files)
    ext <- tolower(tools::file_ext(input$data_files$name))
    if (any(!ext %in% allowed_ext)) return(FALSE)
    if (any(input$data_files$size == 0)) return(FALSE)
    TRUE
  })

  observe({
    valid <- !is.null(input$data_files) && files_valid()
    shinyjs::toggleState("to_inspect", condition = valid)
  })

  # ---- Read Files + Parse Plates ----
  observeEvent(list(input$data_files, input$has_header), {

    req(files_valid())

    plate_list <- list()

    # -----------------------------
    # Helper: contiguous runs
    # -----------------------------
    get_runs <- function(x) {
      idx <- which(x)
      if (length(idx) == 0) return(list())
      splits <- cumsum(c(1, diff(idx) > 1))
      split(idx, splits)
    }

    for (i in seq_len(nrow(input$data_files))) {

      path  <- input$data_files$datapath[i]
      fname <- input$data_files$name[i]
      name  <- tools::file_path_sans_ext(fname)
      ext   <- tolower(tools::file_ext(fname))

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

      raw <- as.data.frame(raw, stringsAsFactors = FALSE)
      raw[raw == ""] <- NA

      # -----------------------------
      # Detect occupied rows/cols
      # -----------------------------
      nonempty_rows <- apply(raw, 1, function(r) any(!is.na(r)))
      nonempty_cols <- apply(raw, 2, function(c) any(!is.na(c)))

      row_blocks <- get_runs(nonempty_rows)
      col_blocks <- get_runs(nonempty_cols)

      file_plate_count <- 1

      # -----------------------------
      # Extract all separated tables
      # -----------------------------
      for (rb in row_blocks) {
        for (cb in col_blocks) {

          values <- raw[rb, cb, drop = FALSE]

          if (!any(!is.na(values))) next

          # Keep full detected rectangle so internal NAs remain visible
          values <- raw[min(rb):max(rb), min(cb):max(cb), drop = FALSE]

          if (nrow(values) == 0 || ncol(values) == 0) next

          # -----------------------------
          # Handle headers (STRICT CROPPING)
          # -----------------------------
          if (isTRUE(input$has_header)) {

            # Detect numeric column headers (e.g., 1–12)
            header_row <- values[1, , drop = TRUE]

            # Detect row headers (e.g., A–H)
            header_col <- values[, 1, drop = TRUE]

            # Identify where real column numbers start (ignore NA/text)
            col_start <- which(!is.na(suppressWarnings(as.numeric(header_row))))[1]
            col_end   <- tail(which(!is.na(suppressWarnings(as.numeric(header_row)))), 1)

            # Identify where real row letters start (ignore NA)
            row_start <- which(!is.na(header_col) & header_col != "")[1]
            row_end   <- tail(which(!is.na(header_col) & header_col != ""), 1)

            # Safety fallback if detection fails
            if (is.na(col_start) || is.na(col_end) || is.na(row_start) || is.na(row_end)) next

            # Crop EXACT plate region (removes headers completely)
            values <- values[
              row_start:row_end,
              col_start:col_end,
              drop = FALSE
            ]

            # Now assign clean dim names
            rownames(values) <- LETTERS[seq_len(nrow(values))]
            colnames(values) <- seq_len(ncol(values))

          } else {

            rownames(values) <- LETTERS[seq_len(nrow(values))]
            colnames(values) <- seq_len(ncol(values))
          }

          # -----------------------------
          # Convert all non-numeric to NA
          # -----------------------------
          values[] <- lapply(values, function(x) suppressWarnings(as.numeric(x)))

          # -----------------------------
          # Convert to long format
          # -----------------------------
          plate <- values %>%
            tibble::rownames_to_column("row") %>%
            tidyr::pivot_longer(
              -row,
              names_to = "col",
              values_to = "value"
            )

          plate$row <- as.character(plate$row)
          plate$col <- as.integer(plate$col)

          # -----------------------------
          # Initialize metadata
          # -----------------------------
          plate$is_control     <- FALSE
          plate$is_blank       <- FALSE
          plate$is_label       <- FALSE
          plate$is_standard    <- FALSE
          plate$labels         <- replicate(nrow(plate), character(0), simplify = FALSE)
          plate$control_groups <- replicate(nrow(plate), character(0), simplify = FALSE)
          plate$blanks         <- replicate(nrow(plate), character(0), simplify = FALSE)
          plate$standards      <- NA_real_
          plate$standard_units <- NA_character_

          plate_name <- paste0("Plate_", file_plate_count, "_", name)

          plate$plate <- plate_name
          plate_list[[plate_name]] <- plate

          file_plate_count <- file_plate_count + 1
        }
      }
    }

    validate(
      need(length(plate_list) > 0, "No valid plates could be loaded.")
    )

    plates(plate_list)
  },
  ignoreInit = FALSE)
}
