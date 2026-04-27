#' Analysis Screen Server Logic
#'
#' Provides statistical analysis and validation tools for normalized plate data.
#' Supports hypothesis testing, visualization selection, and pre-analysis validation.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param state ReactiveValues object storing global application state
#' @param plates Reactive expression containing plate-level metadata
#' @param normalized_data Reactive expression providing normalized plate values
#'
#' @details
#' This module manages the analysis configuration and validation pipeline prior to
#' statistical execution.
#'
#' **Core responsibilities**
#' - Plate selection and persistence of user settings
#' - Storage of per-plate analysis and visualization preferences
#' - Validation of statistical assumptions and requirements
#' - Dynamic UI feedback (errors and warnings)
#' - Enable/disable progression based on readiness
#'
#' **Supported analyses**
#' - T-test (Pairwise and Control vs Treatment)
#' - One-way ANOVA (multi-group comparison)
#' - Outlier detection (IQR-based warnings)
#'
#' **Supported visualizations**
#' - Boxplot
#' - Bar chart
#' - Standard curve regression plot
#'
#' **Validation logic**
#' The module checks:
#' - Minimum number of groups
#' - Presence of required controls (for t-tests)
#' - Minimum replicates per group
#' - Adequacy of data for standard curve fitting
#'
#' **State persistence**
#' - Stores per-plate checkbox selections in `state$analysis_selections`
#' - Restores selections when revisiting analysis screen
#' - Propagates final selections to downstream workflow (`state`)
#'
#' @return NULL (all outputs are reactive side effects)
#'
#' @seealso analysis_ui
#' @family Server Screens

server_analysis <- function(input, output, session, state, plates, normalized_data) {

  # --- Initialize persistent selection storage ---
  observeEvent(TRUE, {
    if (is.null(state$analysis_selections)) {
      state$analysis_selections <- list()
    }
  }, once = TRUE)

  # --- Plate selection initialization ---
  observeEvent(state$screen, {

    req(state$screen == "analysis")
    req(length(plates()) > 0)

    current_plate <- isolate(input$analysis_plate)

    selected_plate <- if (!is.null(current_plate) &&
                          current_plate %in% names(plates())) {
      current_plate
    } else {
      names(plates())[1]
    }

    updateSelectInput(
      session,
      "analysis_plate",
      choices = names(plates()),
      selected = selected_plate
    )

  }, ignoreInit = FALSE)

  # --- Restore saved selections when entering screen or switching plates ---
  observeEvent(
    list(state$screen, input$analysis_plate),
    {

      req(state$screen == "analysis")
      req(input$analysis_plate)

      session$onFlushed(function() {

        saved <- isolate(state$analysis_selections[[input$analysis_plate]])

        updateCheckboxGroupInput(
          session,
          "analysis_types",
          selected = saved$analysis_types %||% character(0)
        )

        updateCheckboxGroupInput(
          session,
          "viz_types",
          selected = saved$viz_types %||% character(0)
        )

      }, once = TRUE)

    },
    ignoreInit = FALSE
  )

  # --- Persist analysis type selections per plate ---
  observeEvent(input$analysis_types, {

    req(input$analysis_plate)

    state$analysis_selections[[input$analysis_plate]] <-
      modifyList(
        state$analysis_selections[[input$analysis_plate]] %||% list(),
        list(analysis_types = input$analysis_types)
      )

  }, ignoreInit = TRUE)

  # --- Persist visualization selections per plate ---
  observeEvent(input$viz_types, {

    req(input$analysis_plate)

    state$analysis_selections[[input$analysis_plate]] <-
      modifyList(
        state$analysis_selections[[input$analysis_plate]] %||% list(),
        list(viz_types = input$viz_types)
      )

  }, ignoreInit = TRUE)

  # --- Pre-analysis validation checks ---
  analysis_checks <- reactive({

    req(input$analysis_plate)
    req(normalized_data())

    msgs <- list(errors = character(0), warnings = character(0))

    # Filter data for selected plate and relevant roles
    df <- normalized_data() %>%
      dplyr::filter(
        plate == input$analysis_plate,
        role %in% c("normal", "control", "standard")
      ) %>%
      dplyr::mutate(
        value = dplyr::case_when(
          role == "standard" ~ as.numeric(value),
          role %in% c("normal", "control") ~ as.numeric(normalized_value),
          TRUE ~ NA_real_
        )
      )

    # Build control grouping labels for plotting consistency
    control_labels <- df %>%
      dplyr::filter(role == "control") %>%
      dplyr::group_by(row, col) %>%
      dplyr::summarise(
        groups = paste(sort(unique(group)), collapse = ", "),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        control_label = paste0("Control (", groups, ")")
      )

    df <- df %>%
      dplyr::left_join(control_labels, by = c("row", "col")) %>%
      dplyr::mutate(
        plot_group = dplyr::case_when(
          role == "control"  ~ control_label,
          role == "normal"   ~ as.character(group),
          role == "standard" ~ as.character(group),
          TRUE ~ NA_character_
        ),
        plot_group = factor(plot_group)
      ) %>%
      dplyr::distinct(plate, row, col, plot_group, .keep_all = TRUE)

    # Non-standard observations used for statistical checks
    non_std <- df %>%
      dplyr::filter(role %in% c("normal", "control"))

    n_groups <- dplyr::n_distinct(non_std$plot_group)

    group_sizes <- non_std %>%
      dplyr::distinct(plot_group, row, col) %>%
      dplyr::count(plot_group, name = "n")

    has_control <- any(non_std$role == "control")

    selected_tests <- input$analysis_types %||% character(0)
    selected_viz   <- input$viz_types %||% character(0)

    # --- T-test validation ---
    if ("T-test (Control vs Treatment)" %in% selected_tests) {

      if (!has_control)
        msgs$errors <- c(msgs$errors,
                         "T-test requires at least one control group.")

      if (n_groups < 2)
        msgs$errors <- c(msgs$errors,
                         "T-test requires at least two groups.")

      if (any(group_sizes$n < 2))
        msgs$warnings <- c(msgs$warnings,
                           "Some groups have fewer than 2 observations for T-tests.")
    }

    # --- ANOVA validation ---
    if ("ANOVA (Multigroup)" %in% selected_tests) {

      if (n_groups < 3)
        msgs$errors <- c(msgs$errors,
                         "ANOVA requires at least three groups.")

      if (any(group_sizes$n < 2))
        msgs$errors <- c(msgs$errors,
                         "ANOVA requires at least two observations per group.")
    }

    # --- Outlier detection validation ---
    if ("Outlier Detection" %in% selected_tests) {

      if (any(group_sizes$n < 4))
        msgs$warnings <- c(
          msgs$warnings,
          "Groups with fewer than 4 observations may yield unreliable IQR-based outlier detection."
        )
    }

    # --- Standard curve validation ---
    if ("Standard Curve" %in% selected_viz) {

      std_n <- sum(df$role == "standard", na.rm = TRUE)

      if (std_n < 2)
        msgs$errors <- c(
          msgs$errors,
          "Standard Curve requires at least two standard wells."
        )
    }

    msgs
  })

  # --- Readiness flag for downstream navigation ---
  analysis_ready <- reactive({
    chk <- analysis_checks()
    length(chk$errors) == 0
  })

  # --- Render validation messages in UI ---
  output$analysis_messages <- renderUI({

    chk <- analysis_checks()

    tagList(

      if (length(chk$errors) > 0)
        div(
          style = "color:#842029;background:#f8d7da;
                   padding:10px;border-radius:6px;margin-bottom:8px;",
          tags$b("Errors:"),
          tags$ul(lapply(chk$errors, tags$li))
        ),

      if (length(chk$warnings) > 0)
        div(
          style = "color:#664d03;background:#fff3cd;
                   padding:10px;border-radius:6px;",
          tags$b("Warnings:"),
          tags$ul(lapply(chk$warnings, tags$li))
        )
    )
  })

  # --- Enable/disable navigation based on validation state ---
  observe({

    if (state$screen != "analysis") {
      shinyjs::disable("to_results")
      return()
    }

    chk <- analysis_checks()

    shinyjs::toggleState(
      "to_results",
      condition = length(chk$errors) == 0
    )
  })

  # --- Select/Deselect all options toggle ---
  observeEvent(input$analysis_select_all, {

    updateCheckboxGroupInput(
      session,
      "analysis_types",
      selected = if (isTRUE(input$analysis_select_all)) {
        c(
          "T-test (Control vs Treatment)",
          "ANOVA (Multigroup)",
          "Outlier Detection"
        )
      } else character(0)
    )

    updateCheckboxGroupInput(
      session,
      "viz_types",
      selected = if (isTRUE(input$analysis_select_all)) {
        c(
          "Boxplot",
          "Bar Chart",
          "Standard Curve"
        )
      } else character(0)
    )

  })

  # --- Persist final selections before navigating forward ---
  observeEvent(input$to_results, {
    state$viz_types <- input$viz_types
    state$analysis_types <- input$analysis_types
  })
}
