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

    # ---- Normalize per group, keep full detail ----
    detailed <- plate_groups %>%
      dplyr::group_by(group) %>%
      dplyr::group_modify(function(df, key) {

        blanks   <- df %>% dplyr::filter(role == "blank")
        controls <- df %>% dplyr::filter(role == "control")
        normals  <- df %>% dplyr::filter(role == "normal")

        # Compute blank mean
        blank_mean <- if (nrow(blanks) > 0) mean(blanks$value, na.rm = TRUE) else 0

        # Subtract blank from controls and normals
        controls <- controls %>%
          dplyr::mutate(blank_corrected = value - blank_mean)
        normals <- normals %>%
          dplyr::mutate(blank_corrected = value - blank_mean)

        # Compute control mean from blank-corrected controls
        control_mean <- if (nrow(controls) > 0) mean(controls$blank_corrected, na.rm = TRUE) else NA_real_

        # Divide both controls and normals by control mean
        controls <- controls %>%
          dplyr::mutate(normalized_value = ifelse(is.na(control_mean) | control_mean == 0, NA_real_,
                                                  blank_corrected / control_mean))
        normals <- normals %>%
          dplyr::mutate(normalized_value = ifelse(is.na(control_mean) | control_mean == 0, NA_real_,
                                                  blank_corrected / control_mean))

        # Combine controls and normals for export
        dplyr::bind_rows(controls, normals) %>%
          dplyr::mutate(blank_mean = blank_mean,
                        control_mean = control_mean)
      }) %>%
      dplyr::ungroup()

    req(nrow(detailed) > 0)
    detailed
  })

  # --- Download detailed CSV (processed values for selected plate) ---
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0(input$normalize_plate, "_processed_values.csv")
    },
    content = function(file) {
      write.csv(
        normalized_data() %>%
          dplyr::select(group, role, value, blank_mean, control_mean, normalized_value),
        file,
        row.names = FALSE
      )
    }
  )

  # --- Prism-style download (normalized only) ---
  output$download_prism <- downloadHandler(
    filename = function() {
      paste0(input$normalize_plate, "_prism.csv")
    },
    content = function(file) {
      df <- normalized_data()
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
