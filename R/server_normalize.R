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

    # ---- Normalize per group ----
    normalized <- plate_groups %>%
      dplyr::group_by(group) %>%
      dplyr::group_modify(function(df, key) {

        controls <- df %>% dplyr::filter(role == "control")
        blanks   <- df %>% dplyr::filter(role == "blank")
        normals  <- df %>% dplyr::filter(role == "normal")

        # If no controls OR no normals, skip safely
        if (nrow(controls) == 0 || nrow(normals) == 0) {
          return(tibble::tibble())
        }

        control_mean <- mean(controls$value, na.rm = TRUE)
        blank_mean   <- if (nrow(blanks) > 0)
          mean(blanks$value, na.rm = TRUE)
        else 0

        # If control_mean invalid, skip safely
        if (is.na(control_mean) || is.nan(control_mean) || control_mean == 0) {
          return(tibble::tibble())
        }

        normals %>%
          dplyr::mutate(
            normalized_value = (value - blank_mean) / control_mean
          )
      }) %>%
      dplyr::ungroup()

    req(nrow(normalized) > 0)

    # ---- Join back to full plate layout ----
    plate <- plates()[[plate_name]]

    plate %>%
      dplyr::left_join(
        normalized %>%
          dplyr::select(row, col, normalized_value),
        by = c("row", "col")
      )
  })

  # --- Plot (same layout as inspect, read-only) ---
  output$normalize_plot <- renderPlot({

    req(state$screen == "normalize")

    plot_df <- normalized_data()

    plot_df$label_text <- ifelse(
      !is.na(plot_df$normalized_value),
      round(plot_df$normalized_value, 2),
      ""
    )

    ggplot2::ggplot(plot_df, ggplot2::aes(x = col, y = row)) +
      ggplot2::geom_tile(
        ggplot2::aes(fill = normalized_value),
        color = "black"
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = label_text),
        size = 3
      ) +
      ggplot2::scale_fill_viridis_c(na.value = "white") +
      ggplot2::scale_y_reverse() +
      ggplot2::coord_fixed() +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = paste("Normalized Plate:", input$normalize_plate),
        fill = "Normalized"
      )
  })

  # --- Download normalized data ---
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0(input$normalize_plate, "_normalized.csv")
    },
    content = function(file) {
      write.csv(normalized_data(), file, row.names = FALSE)
    }
  )

  output$download_txt <- downloadHandler(
    filename = function() {
      paste0(input$normalize_plate, "_normalized.csv")
    },
    content = function(file) {
      write.table(normalized_data(),
                  file,
                  sep = "\t",
                  row.names = FALSE,
                  quote = FALSE)
    }
  )

  output$download_xlsx <- downloadHandler(
    filename = function() {
      paste0(input$normalize_plate, "_normalized.csv")
    },
    content = function(file) {
      writexl::write_xlsx(normalized_data(), file)
    }
  )

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
