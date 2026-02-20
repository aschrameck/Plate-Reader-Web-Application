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
    ext <- tools::file_ext(input$data_files$name)
    if (any(!ext %in% allowed_ext)) return(FALSE)
    if (any(input$data_files$size == 0)) return(FALSE)
    TRUE
  })

  observe({
    valid <- !is.null(input$data_files) && files_valid()
    shinyjs::toggleState("to_inspect", condition = valid)
  })

  # ---- Read Files + Parse Plates ----
  observeEvent(input$data_files, {

    req(files_valid())

    plate_list <- list()

    for (i in seq_len(nrow(input$data_files))) {

      path  <- input$data_files$datapath[i]
      fname <- input$data_files$name[i]
      name  <- tools::file_path_sans_ext(fname)
      ext   <- tools::file_ext(fname)

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

      # ---- Crop at first completely blank row ----
      blank_row <- apply(values, 1, function(r) {
        all(is.na(r) | trimws(as.character(r)) == "")
      })

      if (any(blank_row)) {
        first_blank_row <- which(blank_row)[1]
        values <- values[seq_len(first_blank_row - 1), , drop = FALSE]
      }

      # ---- Crop at first completely blank column ----
      blank_col <- apply(values, 2, function(c) {
        all(is.na(c) | trimws(as.character(c)) == "")
      })

      if (any(blank_col)) {
        first_blank_col <- which(blank_col)[1]
        values <- values[, seq_len(first_blank_col - 1), drop = FALSE]
      }

      # -- Handle headers ---
      if(isTRUE(input$has_headers)) {
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

      # Ensure all columns are numeric
      values[] <- lapply(values, function(x) suppressWarnings(as.numeric(x)))

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

      plate$is_control     <- FALSE
      plate$is_blank       <- FALSE
      plate$is_label       <- FALSE
      plate$labels         <- replicate(nrow(plate), character(0), simplify = FALSE)
      plate$control_groups <- replicate(nrow(plate), character(0), simplify = FALSE)
      plate$blanks         <- replicate(nrow(plate), character(0), simplify = FALSE)
      plate$plate          <- name

      plate_list[[name]] <- plate
    }

    validate(
      need(length(plate_list) > 0, "No valid plates could be loaded.")
    )

    plates(plate_list)
  })
}
