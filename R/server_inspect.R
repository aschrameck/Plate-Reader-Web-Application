#' Plate Inspection Screen Server Logic
#'
#' Manages interactive inspection, labeling, and visualization of wells.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param state ReactiveValues object storing application state
#' @param plates Reactive object containing plate data
#'
#' @details
#' This server function handles:
#' - Updating plate selection choices
#' - Rendering the interactive plate layout
#' - Allowing users to select wells via brushing
#' - Assigning and storing well labels (blank, control, standard, normal)
#' - Ensuring proper group visualizations
#'
#' Reactive outputs include:
#' - `plate_preview` plot of wells
#' - Well selection metadata for downstream normalization
#'
#' @return NULL; updates `state` and plate metadata reactively
#'
#' @seealso inspect_ui
#' @family Server Screens

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
  inspect_plot_reactive <- reactive({
    req(input$active_plate)
    validate(
      need(input$active_plate %in% names(plates()), "Invalid plate selection")
    )

    plate <- plates()[[input$active_plate]]

    # --- Expand wells into groups ---
    expanded_plate <- plate %>%
      mutate(
        group = purrr::pmap(
          list(is_control, is_blank, is_label, is_standard,
               control_groups, blanks, labels, standards, standard_units),
          function(is_control, is_blank, is_label, is_standard,
                   control_groups, blanks, labels, standards, standard_units) {

            if (is_control && length(control_groups) > 0) {
              control_groups

            } else if (is_blank && length(blanks) > 0) {
              blanks

            } else if (is_standard && !is.na(standards)) {
              if (!is.na(standard_units) && standard_units != "") {
                paste0("STD__", standards, " ", standard_units)
              } else {
                paste0("STD__", standards)
              }

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
    row_levels <- sort(unique(plate$row))  # from FULL plate, not filtered

    expanded_plate <- expanded_plate %>%
      mutate(row = factor(row, levels = rev(row_levels)))

    # --- Compute sub-tiles for multi-group wells ---
    # Each well can belong to multiple groups → split it visually into slices
    # This ensures overlapping labels are visible instead of overwritten
    expanded_plate <- expanded_plate %>%
      group_by(row, col) %>%
      mutate(
        n_groups = n(),
        slice_id = row_number(),
        # Horizontal splitting for normal/standard/control wells (x-axis)
        xmin = ifelse(is_blank,
                      col - 0.5,
                      col - 0.5 + (slice_id - 1) / n_groups),
        xmax = ifelse(is_blank,
                      col + 0.5,
                      col - 0.5 + slice_id / n_groups),
        # Vertical splitting for blank wells (y-axis)
        ymin = ifelse(is_blank,
                      as.numeric(row) - 0.5 + (slice_id - 1) / n_groups,
                      as.numeric(row) - 0.5),
        ymax = ifelse(is_blank,
                      as.numeric(row) - 0.5 + slice_id / n_groups,
                      as.numeric(row) + 0.5)
      ) %>%
      ungroup()

    # --- Dynamic label colors ---
    # Only include wells that are actually marked standard
    std_labels <- plate %>%
      dplyr::filter(is_standard) %>%
      dplyr::mutate(
        label = ifelse(!is.na(standard_units) & standard_units != "",
                       paste0("STD__", standards, " ", standard_units),
                       paste0("STD__", standards))
      ) %>%
      .$label

    # Combine with other labels for palette
    all_labels <- unique(c(unlist(plate$labels),
                           unlist(plate$control_groups),
                           unlist(plate$blanks),
                           std_labels))
    all_labels <- all_labels[!is.na(all_labels) & all_labels != ""]
    if (length(all_labels) == 0) all_labels <- "dummy"

    # Split labels
    other_labels <- setdiff(all_labels, std_labels)

    # Standard ordering
    std_df <- plate %>%
      dplyr::filter(is_standard, !is.na(standards)) %>%
      dplyr::mutate(
        standards_num = suppressWarnings(as.numeric(standards)),
        label = ifelse(!is.na(standard_units) & standard_units != "",
                       paste0("STD__", standards, " ", standard_units),
                       paste0("STD__", standards))
      ) %>%
      dplyr::distinct(label, standards, standards_num) %>%
      dplyr::arrange(standards_num, standards)

    # Color palettes
    cat_colors <- if (length(other_labels) > 0) setNames(scales::hue_pal()(length(other_labels)), other_labels) else c()
    std_colors <- if (nrow(std_df) > 0) {
      setNames(colorRampPalette(c("#deebf7", "#08519c"))(nrow(std_df)), std_df$label)
    } else c()
    palette <- c(cat_colors, std_colors)

    # Legend filtering
    non_std_labels <- setdiff(names(palette), std_df$label)

    # --- Plate dimensions ---
    max_row <- length(unique(plate$row))
    max_col <- length(unique(plate$col))

    row_levels <- sort(unique(plate$row))
    col_levels <- sort(unique(plate$col))

    # Column numbers (top row)
    col_header <- data.frame(
      row = max_row + 0.8,
      col = col_levels,
      label = col_levels
    )

    # Row letters (left column)
    row_header <- data.frame(
      row = row_levels,
      col = 0,
      label = row_levels
    )

    # --- Plotting ---
    plot <- ggplot(expanded_plate) +
      # Base layer: all wells
      geom_rect(
        aes(xmin = xmin, xmax = xmax,
            ymin = ymin, ymax = ymax,
            fill = group)
      ) +

      # Column headers
      geom_text(
        data = col_header,
        aes(x = col, y = row, label = label),
        inherit.aes = FALSE,
        size = 4,
        fontface = "bold"
      ) +

      # Row headers
      geom_text(
        data = row_header,
        aes(x = col, y = row, label = label),
        inherit.aes = FALSE,
        size = 4,
        fontface = "bold"
      ) +

      # Blanks
      geom_rect_pattern(
        data = dplyr::filter(expanded_plate, is_blank),
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = group),
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
        pattern_color = "grey",
        pattern_angle = 45,
        pattern_density = 0.1,
        pattern_spacing = 0.02,
        pattern_alpha = 0.8,
        show.legend = c(fill=TRUE, pattern=FALSE)
      ) +

      geom_tile(
        data = plate,
        aes(x = col, y = row),
        fill = NA,
        color = "white",
        linewidth = 1.2
      ) +

      # Values
      geom_text(
        aes(x = col, y = row,
            label = ifelse(is.na(value), "", value)),
        size = 3
      ) +

      # Dynamic coloring
      scale_fill_manual(
        values = palette,
        breaks = non_std_labels,
        labels = identity,
        na.value = "grey"
      ) +

      scale_y_discrete(
        expand = expansion(add = c(0.5, max_row * 0.15))
      ) +  # more space on top
      scale_x_continuous(expand = expansion(add = c(1, 1))) +  # keep symmetric padding

      coord_cartesian(clip = "off") +  # prevents cropping of headers

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
        )
      ) +
      labs(
        fill = "Label",
        caption = "Stripes indicate controls, dots indicate blanks \nGrey cells will be discarded"
      )
  })

  output$plate_preview <- renderPlot({
    p <- inspect_plot_reactive()

    # save per plate
    state$inspect_plots[[input$active_plate]] <- p

    p
  })

  # --- Buttons ---
  # Checkboxes mutually exclusive
  observeEvent(input$mark_control, {
    if (isTRUE(input$mark_control)) {
      updateCheckboxInput(session, "mark_blank", value = FALSE)
      updateCheckboxInput(session, "mark_standard", value = FALSE)
    }
  })

  observeEvent(input$mark_blank, {
    if (isTRUE(input$mark_blank)) {
      updateCheckboxInput(session, "mark_control", value = FALSE)
      updateCheckboxInput(session, "mark_standard", value = FALSE)
    }
  })

  observeEvent(input$mark_standard, {
    if (isTRUE(input$mark_standard)) {
      updateCheckboxInput(session, "mark_control", value = FALSE)
      updateCheckboxInput(session, "mark_blank", value = FALSE)
    }
  })


  # Disable next button until label applied
  observe({
    if (state$screen != "inspect") {
      shinyjs::disable("to_normalize")
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
      "to_normalize",
      condition = has_group
    )
  })

  # Apply label to brushed wells
  observeEvent(input$apply_label, {
    req(input$plate_brush, input$active_plate)

    # Prevent empty labels
    if (!isTRUE(input$mark_standard) && (is.null(input$new_label) || input$new_label == "")) {
      showNotification("Label cannot be empty.", type = "error")
      return()
    }

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

    if (isTRUE(input$mark_standard)) {
      conc <- suppressWarnings(as.numeric(input$new_label))

      if (is.na(conc)) {
        showNotification("Standard labels must be numeric concentrations.", type = "error"); return() }

      plate$is_standard[idx] <- TRUE
      plate$is_blank[idx] <- FALSE
      plate$is_control[idx] <- FALSE
      plate$is_label[idx] <- FALSE
      plate$standards[idx] <- conc

      plate$standard_units[idx] <- input$standard_units

      plate$blanks[idx] <- list(character(0))
      plate$control_groups[idx] <- list(character(0))
      plate$labels[idx] <- list(character(0))

    } else if (isTRUE(input$mark_blank)) {
      plate$is_standard[idx] <- FALSE
      plate$is_blank[idx] <- TRUE
      plate$is_control[idx] <- FALSE
      plate$is_label[idx] <- FALSE

      plate$standards[idx] <- list(character(0))
      plate$control_groups[idx] <- list(character(0))
      plate$labels[idx] <- list(character(0))

      plate$blanks[idx] <- lapply(plate$blanks[idx], function(x) unique(c(x, input$new_label)))

    } else if (isTRUE(input$mark_control)) {
      plate$is_standard[idx] <- FALSE
      plate$is_blank[idx] <- FALSE
      plate$is_control[idx] <- TRUE
      plate$is_label[idx] <- FALSE

      plate$blanks[idx] <- list(character(0))
      plate$standards[idx] <- list(character(0))
      plate$labels[idx] <- list(character(0))
      plate$control_groups[idx] <- lapply(plate$control_groups[idx], function(x) unique(c(x, input$new_label)))

    } else {
      plate$is_standard[idx] <- FALSE
      plate$is_blank[idx] <- FALSE
      plate$is_control[idx] <- FALSE
      plate$is_label[idx] <- TRUE

      plate$blanks[idx] <- list(character(0))
      plate$control_groups[idx] <- list(character(0))
      plate$standards[idx] <- list(character(0))
      plate$labels[idx] <- lapply(plate$labels[idx], function(x) unique(c(x, input$new_label)))
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
    plate$standards[idx]      <- list(character(0))

    plate$is_control[idx] <- FALSE
    plate$is_blank[idx]   <- FALSE
    plate$is_label[idx]   <- FALSE
    plate$is_standard[idx] <- FALSE

    plate_list[[input$active_plate]] <- plate
    plates(plate_list)
  })

  # --- Standard Legend ---
  make_standard_legend_plot <- function(std_vals, units = NULL) {
    req(length(std_vals) >= 2)
    df <- data.frame(x = seq_along(std_vals), y = 1, value = sort(std_vals))

    ggplot(df, aes(x = x, y = y, fill = value)) +
      geom_tile() +
      scale_fill_gradient(low = "#deebf7", high = "#08519c", guide = "none") +
      scale_x_continuous(
        limits = c(1, length(std_vals)),
        breaks = c(1, length(std_vals)),
        labels = round(c(min(std_vals), max(std_vals)), 3)
      ) +
      theme_void() +
      theme(
        plot.margin = margin(2, 2, 2, 2),
        axis.text.x = element_text(size = 12)
      ) +
      labs(
        title = if (!is.null(units)) paste("Standard Wells (", units[1], ")", sep="") else "Standard Wells"
      )
  }

  standard_legend_reactive <- reactive({
    req(input$active_plate)
    plate <- plates()[[input$active_plate]]

    std_vals <- plate$standards
    std_vals <- as.numeric(unlist(std_vals))
    std_vals <- std_vals[!is.na(std_vals)]

    units <- unique(na.omit(plate$standard_units))
    if (length(std_vals) < 2) return(NULL)

    gradient_css <- "linear-gradient(to right, #deebf7, #08519c)"

    legend_ui <- tags$div(
      style = "padding: 8px 12px;",
      tags$div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        tags$strong("Standard Wells"),
        tags$span(style = "font-size: 12px; color: grey;", if (length(units) > 0) units[1] else "")
      ),
      tags$div(
        style = paste0(
          "height: 12px; border-radius: 6px; margin: 6px 0;",
          "background: ", gradient_css, ";"
        )
      ),
      tags$div(
        style = "display: flex; justify-content: space-between; font-size: 12px;",
        tags$span(round(min(std_vals), 3)),
        tags$span(round(max(std_vals), 3))
      )
    )

    # Store in state for results server
    state$standard_legend[[input$active_plate]] <- legend_ui

    legend_ui
  })

  output$standard_legend <- renderUI({
    standard_legend_reactive()
  })
}
