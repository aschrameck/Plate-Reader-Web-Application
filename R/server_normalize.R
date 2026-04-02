server_normalize <- function(input, output, session, state, plates, group_map) {

  # --- Plate selector ---
  observe({
    req(state$screen == "normalize")
    req(length(plates()) > 0)

    updateSelectInput(
      session,
      "normalize_plate",
      choices = names(plates()),
      selected = names(plates())[1]
    )
  })

  # --- Reactive: Selected plate ---
  selected_plate <- reactive({
    req(state$screen == "normalize")
    req(input$normalize_plate)
    req(input$normalize_plate %in% names(plates()))
    plates()[[input$normalize_plate]]
  })

  # --- Reactive: Normalized data table ---
  normalized_data <- reactive({
    req(state$screen == "normalize")
    req(input$normalize_plate)

    gm <- group_map()
    req(nrow(gm) > 0)

    plate_name <- input$normalize_plate

    plate_groups <- gm %>%
      dplyr::filter(plate == plate_name)

    req(nrow(plate_groups) > 0)

    # ---- Split by role ----
    blanks   <- plate_groups %>% dplyr::filter(role == "blank")
    controls <- plate_groups %>% dplyr::filter(role == "control")
    normals  <- plate_groups %>% dplyr::filter(role == "normal")
    standards <- plate_groups %>% dplyr::filter(role == "standard")

    # --- Compute blank mean ---
    blank_mean <- if (nrow(blanks) > 0) mean(blanks$value, na.rm = TRUE) else 0

    # --- Correct controls and normals ---
    controls <- controls %>%
      dplyr::mutate(
        blank_corrected = value - blank_mean
      )
    normals <- normals %>%
      dplyr::mutate(
        blank_corrected = value - blank_mean
      )

    # --- Control normalization factor ---
    control_mean <- if (nrow(controls) > 0) mean(controls$blank_corrected, na.rm = TRUE) else NA_real_

    # --- Normalize controls and normals ---
    controls <- controls %>%
      dplyr::mutate(
        normalized_value = ifelse(is.na(control_mean) | control_mean == 0, NA_real_,
                                  blank_corrected / control_mean)
      )

    normals <- normals %>%
      dplyr::mutate(
        normalized_value = ifelse(is.na(control_mean) | control_mean == 0, NA_real_,
                                  blank_corrected / control_mean)
      )

    # --- Standards: blank-corrected only, no control normalization ---
    standards <- standards %>%
      dplyr::mutate(
        blank_corrected = value - blank_mean,
        normalized_value = blank_corrected
      )

    list(
      normals = normals,
      standards = standards,
      blanks = blanks,
      controls = controls,
      blank_mean = blank_mean,
      control_mean = control_mean
    )
  })

  # --- Download: Normal wells ---
  output$download_normal <- downloadHandler(
    filename = function() {
      paste0(input$normalize_plate, "_normalized_normals.csv")
    },
    content = function(file) {
      df <- normalized_data()$normals
      write.csv(
        df %>%
          dplyr::select(row, col, group, value, blank_corrected, normalized_value),
        file,
        row.names = FALSE
      )
    }
  )

  # --- Download: Standards ---
  output$download_standards <- downloadHandler(
    filename = function() {
      paste0(input$normalize_plate, "_standards.csv")
    },
    content = function(file) {
      df <- normalized_data()$standards
      write.csv(
        df %>%
          dplyr::select(row, col, group, value, blank_corrected),
        file,
        row.names = FALSE
      )
    }
  )

  # --- Optional Prism-style pivot for normals only ---
  output$download_prism <- downloadHandler(
    filename = function() {
      paste0(input$normalize_plate, "_prism_normals.csv")
    },
    content = function(file) {
      df <- normalized_data()$normals
      prism <- df %>%
        dplyr::mutate(sample = paste(row, col, sep = "_")) %>%
        dplyr::select(sample, normalized_value) %>%
        tidyr::pivot_wider(
          names_from = sample,
          values_from = normalized_value
        )
      write.csv(prism, file, row.names = FALSE)
    }
  )
}
