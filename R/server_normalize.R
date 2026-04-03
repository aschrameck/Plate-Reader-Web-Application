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
    plate_groups <- gm %>% dplyr::filter(plate == plate_name)
    req(nrow(plate_groups) > 0)

    detailed <- plate_groups %>%
      dplyr::group_by(group) %>%
      dplyr::group_modify(function(df, key) {

        df$value <- as.numeric(as.character(df$value))

        # --- Blank mean ---
        blank_mean <- if (any(df$role == "blank")) mean(df$value[df$role == "blank"], na.rm = TRUE) else 0
        df$blank_corrected <- df$value - blank_mean

        # --- Control mean ---
        control_mean <- if (any(df$role == "control")) mean(df$blank_corrected[df$role == "control"], na.rm = TRUE) else NA_real_

        # --- Normalized values (skip blanks) ---
        df$normalized_value <- NA_real_
        to_normalize <- df$role != "standard" & df$role != "blank"
        if (!is.na(control_mean) && control_mean != 0) {
          df$normalized_value[to_normalize] <- df$blank_corrected[to_normalize] / control_mean
        }

        df$blank_mean <- blank_mean
        df$control_mean <- control_mean

        df
      }) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(group, role)

    req(nrow(detailed) > 0)
    detailed
  })

  # --- Download detailed CSV (exclude standards) ---
  output$download_csv <- downloadHandler(
    filename = function() paste0(input$normalize_plate, "_processed_data.csv"),
    content = function(file) {
      df <- normalized_data() %>%
        dplyr::filter(role != "standard") %>%  # exclude standards
        dplyr::select(
          Row = row,
          Column = col,
          Group = group,
          Role = role,
          Value = value,
          Blank_Mean = blank_mean,
          Control_Mean = control_mean,
          Normalized = normalized_value
        )
      write.csv(df, file, row.names = FALSE)
    }
  )

  # --- Download Prism file (exclude standards, header first, label row after group) ---
  output$download_prism <- downloadHandler(
    filename = function() paste0(input$normalize_plate, "_prism.csv"),
    content = function(file) {
      df <- normalized_data() %>% dplyr::filter(role != "standard")
      groups <- unique(df$group)

      group_tables <- list()
      for (grp in groups) {
        sub <- df %>% dplyr::filter(group == grp)

        normal <- sub$normalized_value[sub$role == "normal"]
        control <- sub$normalized_value[sub$role == "control"]
        blank <- sub$value[sub$role == "blank"]

        n <- max(length(normal), length(control), length(blank))

        # Build table rows
        tbl <- tibble::tibble(
          Normal = c(normal, rep("", n - length(normal))),
          Control = c(control, rep("", n - length(control))),
          Blank = c(blank, rep("", n - length(blank)))
        )

        # Group header first
        header <- tibble::tibble(
          Normal = paste0("Group: ", grp),
          Control = "",
          Blank = ""
        )

        # Column labels row AFTER group header
        colnames_row <- tibble::tibble(
          Normal = "Normal",
          Control = "Control",
          Blank = "Blank"
        )

        group_tables[[grp]] <- dplyr::bind_rows(header, colnames_row, tbl)
      }

      # Combine horizontally with blank spacer columns
      final_tbl <- group_tables[[1]]
      if (length(group_tables) > 1) {
        for (i in 2:length(group_tables)) {
          spacer <- tibble::tibble(` ` = rep("", nrow(final_tbl)))
          final_tbl <- cbind(final_tbl, spacer, group_tables[[i]])
        }
      }

      write.csv(final_tbl, file, row.names = FALSE, na = "")
    }
  )

  # --- Standards CSV (use correct standard_units, remove prefixes and suffixes) ---
  standards_data <- reactive({
    gm <- group_map()
    req(nrow(gm) > 0)

    stds <- gm %>% dplyr::filter(role == "standard")
    if (nrow(stds) == 0) return(NULL)

    stds %>%
      dplyr::group_by(group) %>%
      dplyr::summarize(
        Label = sub("^STD__", "", first(group)),
        Label = sub("_[A-Z]$", "", Label),
        Unit = if ("standard_units" %in% names(stds)) first(na.omit(standard_units)) else NA,
        Value = first(value),
        .groups = "drop"
      ) %>%
      dplyr::select(Label, Unit, Value)
  })

  # --- Download standards CSV ---
  output$download_standards <- downloadHandler(
    filename = function() paste0(input$normalize_plate, "_standards.csv"),
    content = function(file) {
      df <- standards_data()
      req(!is.null(df))  # safety: don't write if NULL
      write.csv(df, file, row.names = FALSE)
    }
  )
}
