server_inspect <- function(input, output, session, state, plates) {
  # --- Select Plate ---
  observeEvent(state$screen, {
    req(state$screen == "inspect")
    req(length(plates()) > 0)

    updateSelectInput(
      session,
      "active_plate",
      choices = names(plates())
    )
  })

  # --- Plotting ---
  output$plate_preview <- renderPlot({
    req(input$active_plate)

    plate <- plates()[[input$active_plate]] %>%
      filter(!is.na(value))

    # Expand wells into groups
    expanded_plate <- plate %>%
      mutate(
        group = purrr::pmap(
          list(is_control, is_blank, is_label,
               control_groups, blanks, labels),
          function(is_control, is_blank, is_label,
                   control_groups, blanks, labels) {

            if (is_control && length(control_groups) > 0) {
              control_groups

            } else if (is_blank && length(blanks) > 0) {
              blanks

            } else if (is_label && length(labels) > 0) {
              labels

            } else {
              NA_character_
            }
          }
        )
      ) %>%
      tidyr::unnest(group)

    # Factor rows
    expanded_plate <- expanded_plate %>%
      mutate(row = factor(row, levels = rev(sort(unique(row)))))

    # Compute sub-tiles for multi-group controls
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

    # Dynamic label colors
    all_labels <- unique(c(
      unlist(plate$labels),
      unlist(plate$control_groups),
      unlist(plate$blanks)
    ))

    all_labels <- all_labels[!is.na(all_labels) & all_labels != ""]

    if (length(all_labels) == 0) all_labels <- "dummy"

    palette <- scales::hue_pal()(length(all_labels))
    names(palette) <- all_labels

    # Plotting
    ggplot(expanded_plate) +
      # Base layer: all wells
      geom_rect(
        aes(xmin = xmin, xmax = xmax,
            ymin = ymin, ymax = ymax,
            fill = group),
        color = "white",
        linewidth = 0.6
      ) +

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

  # --- Buttons ---
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

    has_group <- any(
      lengths(plate$labels) > 0 |
        lengths(plate$control_groups) > 0 |
        lengths(plate$blanks) > 0
    )

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
      plate$is_label[idx]   <- FALSE

      plate$control_groups[idx] <- list(character(0))
      plate$labels[idx]         <- list(character(0))

      plate$blanks[idx] <- lapply(
        plate$blanks[idx],
        function(x) unique(c(x, input$new_label))
      )

    } else if (isTRUE(input$mark_control)) {
      # ---- CONTROL ----
      plate$is_control[idx] <- TRUE
      plate$is_blank[idx]   <- FALSE
      plate$is_label[idx]   <- FALSE

      plate$blanks[idx]  <- list(character(0))
      plate$labels[idx]  <- list(character(0))

      plate$control_groups[idx] <- lapply(
        plate$control_groups[idx],
        function(x) unique(c(x, input$new_label))
      )

    } else {

      # ---- NORMAL LABEL ----
      plate$is_control[idx] <- FALSE
      plate$is_blank[idx]   <- FALSE
      plate$is_label[idx]   <- TRUE

      plate$control_groups[idx] <- list(character(0))
      plate$blanks[idx]         <- list(character(0))

      plate$labels[idx] <- lapply(
        plate$labels[idx],
        function(x) unique(c(x, input$new_label))
      )
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
    plate$labels[idx]         <- list(character(0))
    plate$control_groups[idx] <- list(character(0))
    plate$blanks[idx]         <- list(character(0))

    plate$is_control[idx] <- FALSE
    plate$is_blank[idx]   <- FALSE
    plate$is_label[idx]   <- FALSE

    plate_list[[input$active_plate]] <- plate
    plates(plate_list)
  })
}
