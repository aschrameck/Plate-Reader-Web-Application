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

    detailed <- plate_groups %>%
      dplyr::group_by(group) %>%
      dplyr::group_modify(function(df, key) {

        # Ensure 'value' is numeric
        df$value <- as.numeric(as.character(df$value))

        # --- Blank mean ---
        blank_mean <- if (any(df$role == "blank")) {
          mean(df$value[df$role == "blank"], na.rm = TRUE)
        } else 0

        # --- Blank-corrected values ---
        df$blank_corrected <- df$value - blank_mean

        # --- Control mean (from blank-corrected controls) ---
        control_mean <- if (any(df$role == "control")) {
          mean(df$blank_corrected[df$role == "control"], na.rm = TRUE)
        } else NA_real_

        # --- Normalized values (skip blanks) ---
        df$normalized_value <- NA_real_
        to_normalize <- df$role != "standard" & df$role != "blank"
        if (!is.na(control_mean) && control_mean != 0) {
          df$normalized_value[to_normalize] <- df$blank_corrected[to_normalize] / control_mean
        }

        # Save blank/control mean for reference
        df$blank_mean <- blank_mean
        df$control_mean <- control_mean

        df
      }) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(group, role)  # sort by group then role

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
          dplyr::select(row, col, group, role, value, blank_mean, control_mean, normalized_value),
        file,
        row.names = FALSE
      )
    }
  )

  # --- Prism-style download (normalized only, exclude standards and blanks) ---
  output$download_prism <- downloadHandler(
    filename = function() {
      paste0(input$normalize_plate, "_prism.csv")
    },
    content = function(file) {
      df <- normalized_data()
      prism <- df %>%
        dplyr::filter(role != "standard", role != "blank") %>%
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
