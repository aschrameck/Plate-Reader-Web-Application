# --- Server ---
app_server <- function(input, output, session) {

  # --- First screen: Upload ---
  state <- reactiveValues(screen = "upload")

  allowed_ext <- c("csv", "txt", "xlsx")

  # Upload spinner
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
      "<span class='upload-status success' aria-label='Upload complete'>✓</span>"
    )
  })

  # File validation
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

  # ---- Plate storage ---
  plates <- reactiveVal(list())

  observeEvent(input$data_files, {
    req(files_valid())

    plate_list <- list()

    for (i in seq_len(nrow(input$data_files))) {

      path  <- input$data_files$datapath[i]
      fname <- input$data_files$name[i]
      name  <- tools::file_path_sans_ext(fname)
      ext   <- tools::file_ext(fname)

      # Read file
      raw <- tryCatch({
        switch(
          ext,
          csv  = read.csv(path, header = FALSE, stringsAsFactors = FALSE),
          txt  = read.delim(path, header = FALSE, stringsAsFactors = FALSE),
          xlsx = as.data.frame(readxl::read_xlsx(path, col_names = FALSE)),
          stop("Unsupported file type")
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

      raw <- as.data.frame(raw)

      # STEP 1: find blank row + blank column
      is_blank_row <- apply(raw, 1, function(x) all(is.na(x) | x == ""))
      is_blank_col <- apply(raw, 2, function(x) all(is.na(x) | x == ""))

      first_blank_row <- which(is_blank_row)[1]
      first_blank_col <- which(is_blank_col)[1]

      if (is.na(first_blank_row) || is.na(first_blank_col)) {
        showNotification(
          paste("Could not detect plate boundary in", fname),
          type = "error"
        )
        next
      }

      # STEP 2: crop to plate area
      plate_raw <- raw[
        seq_len(first_blank_row - 1),
        seq_len(first_blank_col - 1),
        drop = FALSE
      ]

      # STEP 3: handle headers
      if (isTRUE(input$has_headers)) {

        col_names <- as.character(plate_raw[1, -1])
        row_names <- as.character(plate_raw[-1, 1])

        values <- plate_raw[-1, -1, drop = FALSE]

        colnames(values) <- col_names
        rownames(values) <- row_names

      } else {

        values <- plate_raw
        rownames(values) <- LETTERS[seq_len(nrow(values))]
        colnames(values) <- seq_len(ncol(values))
      }

      # STEP 4: convert to long format
      plate <- as.data.frame(as.table(as.matrix(values)))
      colnames(plate) <- c("row", "col", "value")

      plate$row        <- as.character(plate$row)
      plate$col        <- as.integer(plate$col)
      plate$value      <- as.numeric(plate$value)
      plate$label          <- NA_character_
      plate$is_control     <- FALSE
      plate$is_blank       <- FALSE
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

  # --- Buttons ---
  # Analysis page select-all
  observeEvent(input$analysis_select_all, {
    updateCheckboxGroupInput(
      session,
      "analysis_types",
      selected = if (input$analysis_select_all)
        c("t-test", "ANOVA", "Nonparametric test") else character(0)
    )

    updateCheckboxGroupInput(
      session,
      "viz_types",
      selected = if (input$analysis_select_all)
        c("Boxplot", "Bar chart", "Heatmap") else character(0)
    )
  })

  # Navigation buttons
  observeEvent(input$to_inspect,        { state$screen <- "inspect" })
  observeEvent(input$inspect_ok,        { state$screen <- "normalize" })
  observeEvent(input$back_to_upload,    { state$screen <- "upload" })
  observeEvent(input$to_analysis,       { state$screen <- "analysis" })
  observeEvent(input$run_analysis,      { state$screen <- "results" })
  observeEvent(input$start_over,        { state$screen <- "upload" })

  # --- Header ---
  output$step_label <- renderText({
    switch(
      state$screen,
      upload    = "Step 1/5 - Upload",
      inspect   = "Step 2/5 - Select",
      normalize = "Step 3/5 - Normalization",
      analysis  = "Step 4/5 - Analysis",
      results   = "Step 5/5 - Results"
    )
  })

  # Progress bar
  output$progress_bar <- renderUI({
    value <- switch(
      state$screen,
      upload    = 20,
      inspect   = 40,
      normalize = 60,
      analysis  = 80,
      results   = 100
    )

    tags$div(
      class = "progress w-100",
      style = "height: 1.2rem;",
      tags$div(
        class = "progress-bar",
        role = "progressbar",
        style = sprintf("width: %d%%;", value),
        `aria-valuenow` = value,
        `aria-valuemin` = 0,
        `aria-valuemax` = 100
      )
    )
  })

  # --- Screen rendering ---
  output$current_screen <- renderUI({
    switch(
      state$screen,
      upload    = upload_ui(),
      inspect   = inspect_ui(),
      normalize = normalize_ui(),
      analysis  = analysis_ui(),
      results   = results_ui()
    )
  })


  # --- Second screen: Select ---
  observeEvent(
    list(state$screen, plates()),
    {
      req(state$screen == "inspect")
      req(length(plates()) > 0)

      updateSelectInput(
        session,
        "active_plate",
        choices  = names(plates()),
        selected = names(plates())[1]
      )
    },
    ignoreInit = TRUE
  )

  # Plate plot
  output$plate_preview <- renderPlot({
    req(input$active_plate)

    plate <- plates()[[input$active_plate]] %>%
      filter(!is.na(value))

    # ---- STEP 1: expand wells into groups ----
    expanded_plate <- plate %>%
      mutate(
        group = purrr::pmap(
          list(is_control, is_blank, control_groups, blanks, label),
          function(is_control, is_blank, control_groups, blanks, label) {
            if (is_control && length(control_groups) > 0) {
              control_groups
            } else if (is_blank && length(blanks) > 0) {
              blanks
            } else if (!is.na(label)) {
              label
            } else {
              "NA"
            }
          }
        )
      ) %>%
      tidyr::unnest(group)

    # ---- STEP 2: factor rows ONCE (fix upside-down issue) ----
    expanded_plate <- expanded_plate %>%
      mutate(row = factor(row, levels = rev(sort(unique(row)))))

    # ---- STEP 3: compute sub-tiles for multi-group controls ----
    expanded_plate <- expanded_plate %>%
      group_by(row, col) %>%
      mutate(
        n_groups = n(),
        xmin = col - 0.5 + (row_number() - 1) / n_groups,
        xmax = col - 0.5 + row_number() / n_groups,
        ymin = as.numeric(row) - 0.5,
        ymax = as.numeric(row) + 0.5
      ) %>%
      ungroup()

    # --- Dynamic label colors ---
    # Collect all label names (from label column + control_groups)
    all_labels <- unique(c(
      na.omit(plate$label),
      unlist(plate$control_groups),
      unlist(plate$blanks)
    ))

    if (length(all_labels) == 0) all_labels <- "dummy"

    palette <- scales::hue_pal()(length(all_labels))
    names(palette) <- all_labels

    # ---- STEP 4: plot ----
    ggplot(expanded_plate) +
      # Blanks
      geom_rect_pattern(
        data = dplyr::filter(expanded_plate, is_blank),
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = group),
        color = "white",
        pattern = "circle",
        pattern_color = "grey",
        pattern_angle = 45,
        pattern_density = 0.15,
        pattern_spacing = 0.02
      ) +

      # Controls
      geom_rect_pattern(
        data = dplyr::filter(expanded_plate, is_control),
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = group),
        color = "white",
        linewidth = 0.6,
        pattern_color = "grey",
        pattern_angle = 45,
        pattern_density = 0.1,
        pattern_spacing = 0.02,
        pattern_alpha = 0.8,
        show.legend = c(fill=TRUE, pattern=FALSE)
      ) +

      # Non-control wells (single tile)
      geom_tile(
        data = dplyr::filter(expanded_plate, !is_control & !is_blank),
        aes(x = col, y = row, fill = group),
        color = "white",
        linewidth = 0.6
      ) +

      # Values
      geom_text(
        aes(x = col, y = row, label = value),
        size = 3
      ) +

      # Dynamic coloring
      scale_fill_manual(
        values = palette,
        na.value = "#d3d3d3"
      ) +
      coord_fixed() +

      # Formatting
      theme_void() +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text  = element_text(size = 12),
        plot.margin  = margin(5, 5, 5, 5),
        plot.caption = element_text(
          hjust = 0.5,
          size = 11,
          color = "grey40",
          margin = margin(t = 8)
        )) +
      labs(
        fill = "Label",
        caption = "Stripes indicate controls, dots indicate blanks \nGrey cells will be discarded"
      )
  })

  # Control / Blank mutually exclusive
  observeEvent(input$mark_control, {
    if (isTRUE(input$mark_control)) {
      updateCheckboxInput(session, "mark_blank", value = FALSE)
    }
  })

  observeEvent(input$mark_blank, {
    if (isTRUE(input$mark_blank)) {
      updateCheckboxInput(session, "mark_control", value = FALSE)
    }
  })

  # Disable next button until label applied
  observe({
    if (state$screen != "inspect") {
      shinyjs::disable("inspect_ok")
      return()
    }

    req(input$active_plate)

    plate <- plates()[[input$active_plate]]

    has_group <- any(!is.na(plate$label))

    shinyjs::toggleState(
      "inspect_ok",
      condition = has_group
    )
  })

  # Apply label to brushed wells
  observeEvent(input$apply_label, {
    req(input$plate_brush, input$active_plate)

    plate_list <- plates()
    plate <- plate_list[[input$active_plate]]

    brushed <- brushedPoints(
      plate,
      input$plate_brush,
      xvar = "col",
      yvar = "row"
    )

    if (nrow(brushed) == 0) return()

    idx <- paste(plate$row, plate$col) %in%
      paste(brushed$row, brushed$col)

    if (isTRUE(input$mark_blank)) {

      # ---- BLANK ----
      plate$is_blank[idx]   <- TRUE
      plate$is_control[idx] <- FALSE
      plate$control_groups[idx] <- list(character(0))

      plate$blanks[idx] <- lapply(
        plate$blanks[idx],
        function(x) unique(c(x, input$new_label)))

    } else if (isTRUE(input$mark_control)) {
      # ---- CONTROL ----
      plate$is_control[idx] <- TRUE
      plate$is_blank[idx]   <- FALSE
      plate$blanks[idx] <- list(character(0))

      plate$control_groups[idx] <- lapply(
        plate$control_groups[idx],
        function(x) unique(c(x, input$new_label))
      )

    } else {

      # ---- NORMAL LABEL ----
      plate$label[idx]      <- input$new_label
      plate$is_control[idx] <- FALSE
      plate$is_blank[idx]   <- FALSE
      plate$control_groups[idx] <- list(character(0))
      plate$blanks[idx] <- list(character(0))
    }

    plate_list[[input$active_plate]] <- plate
    plates(plate_list)
  })

  # Clear Labels
  observe({
    # Only relevant on inspect screen
    if (state$screen != "inspect") {
      shinyjs::disable("clear_label")
      return()
    }

    has_brush <- !is.null(input$plate_brush)

    shinyjs::toggleState(
      "clear_label",
      condition = has_brush
    )
  })

  observeEvent(input$clear_label, {
    req(input$plate_brush, input$active_plate)

    plate_list <- plates()
    plate <- plate_list[[input$active_plate]]

    brushed <- brushedPoints(
      plate,
      input$plate_brush,
      xvar = "col",
      yvar = "row"
    )

    if (nrow(brushed) == 0) return()

    idx <- paste(plate$row, plate$col) %in%
      paste(brushed$row, brushed$col)

    # Clear label + control flag
    plate$label[idx] <- NA_character_
    plate$is_control[idx] <- FALSE
    plate$is_blank[idx]   <- FALSE
    plate$control_groups[idx] <- list(NULL)
    plate$blanks[idx] <- list(NULL)

    plate_list[[input$active_plate]] <- plate
    plates(plate_list)
  })

  # --- Placeholders (later steps) ---
  output$normalized_preview <- renderTable({
    data.frame(
      Well = c("A1", "A2", "B1"),
      Value = c(1.2, 0.9, 1.5)
    )
  })

  output$final_plots <- renderPlot({
    barplot(
      c(2, 4, 3),
      names.arg = c("Group 1", "Group 2", "Group 3"),
      main = "Results Plot (Placeholder)"
    )
  })
}

