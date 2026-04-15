#' Analysis Screen Server Logic
#'
#' Handles statistical analysis of normalized plate data.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param state ReactiveValues object storing application state
#' @param plates Reactive object containing normalized plate data
#' @param group_map Optional mapping of wells to groups
#'
#' @details
#' Provides configurable statistical analyses:
#' - One-way ANOVA across groups
#' - Pairwise t-tests
#' - Outlier detection and flagging
#'
#' Users can select:
#' - Plate to analyze
#' - Analysis method and parameters
#'
#' Reactive outputs include plots, summaries, and statistical test results.
#'
#' @return NULL; updates analysis results reactively
#'
#' @seealso analysis_ui
#' @family Server Screens

server_analysis <- function(input, output, session, state, plates, normalized_data) {

  # ---- Plate selector ----
  observe({
    req(state$screen == "analysis")
    req(length(plates()) > 0)

    updateSelectInput(
      session,
      "analysis_plate",
      choices = names(plates()),
      selected = names(plates())[1]
    )
  })

  # --- Check test assumptions ---
  analysis_checks <- reactive({

    req(input$analysis_plate)
    req(normalized_data)

    msgs <- list(errors = character(0), warnings = character(0))

    # ---- Build analysis dataset (DEDUPED + CONSISTENT GROUPING) ----
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
      dplyr::left_join(control_labels, by = c("row", "col"))

    df <- df %>%
      dplyr::mutate(
        plot_group = dplyr::case_when(
          role == "control" ~ control_label,
          role == "normal"  ~ as.character(group),
          role == "standard" ~ as.character(group),
          TRUE ~ NA_character_
        ),
        plot_group = factor(plot_group)
      )

    df <- df %>%
      dplyr::distinct(plate, row, col, plot_group, .keep_all = TRUE)

    # ---- Non-standard data ----
    non_std <- df %>%
      dplyr::filter(role %in% c("normal", "control"))

    n_groups <- dplyr::n_distinct(non_std$plot_group)

    group_sizes <- non_std %>%
      dplyr::distinct(plot_group, row, col) %>%
      dplyr::count(plot_group, name = "n")

    has_control <- any(non_std$role == "control")

    selected_tests <- input$analysis_types %||% character(0)
    selected_viz   <- input$viz_types %||% character(0)

    # ---- T-test ----
    if ("T-test (Control vs Treatment)" %in% selected_tests) {

      if (!has_control)
        msgs$errors <- c(msgs$errors, "T-test requires at least one control group.")

      if (n_groups < 2)
        msgs$errors <- c(msgs$errors, "T-test requires at least two groups.")

      if (any(group_sizes$n < 2))
        msgs$warnings <- c(msgs$warnings, "Some groups have fewer than 2 observations for T-tests.")
    }

    # ---- ANOVA ----
    if ("ANOVA (Multigroup)" %in% selected_tests) {

      if (n_groups < 3)
        msgs$errors <- c(msgs$errors, "ANOVA requires at least three groups.")

      if (any(group_sizes$n < 2))
        msgs$errors <- c(msgs$errors, "ANOVA requires at least two observations per group.")
    }

    # ---- Outliers ----
    if ("Outlier Detection" %in% selected_tests) {

      if (any(group_sizes$n < 4))
        msgs$warnings <- c(msgs$warnings,
                           "Groups with fewer than 4 observations may give unreliable IQR outlier detection."
        )
    }

    # ---- Standard Curve ----
    if ("Standard Curve" %in% selected_viz) {

      std_n <- sum(df$role == "standard", na.rm = TRUE)

      if (std_n < 2)
        msgs$errors <- c(msgs$errors, "Standard Curve requires at least two standard wells.")
    }

    msgs
  })

  analysis_ready <- reactive({
    chk <- analysis_checks()
    length(chk$errors) == 0
  })

  # --- Show Errors/Warnings ---
  output$analysis_messages <- renderUI({
    chk <- analysis_checks()

    tagList(

      if (length(chk$errors) > 0)
        div(
          style = "color:#842029;background:#f8d7da;padding:10px;border-radius:6px;margin-bottom:8px;",
          tags$b("Errors:"),
          tags$ul(lapply(chk$errors, tags$li))
        ),

      if (length(chk$warnings) > 0)
        div(
          style = "color:#664d03;background:#fff3cd;padding:10px;border-radius:6px;",
          tags$b("Warnings:"),
          tags$ul(lapply(chk$warnings, tags$li))
        )
    )
  })

  # --- Disable NEXT until errors cleared ---
  observe({

    if (state$screen != "analysis") {
      shinyjs::disable("to_results")
      return()
    }

    chk <- analysis_checks()

    has_error <- length(chk$errors) > 0

    shinyjs::toggleState(
      "to_results",
      condition = !has_error
    )
  })

  # ---- Select-all toggle ----
  observeEvent(input$analysis_select_all, {

    updateCheckboxGroupInput(
      session,
      "analysis_types",
      selected = if (input$analysis_select_all)
        c("T-test (Control vs Treatment)", "ANOVA (Multigroup)", "Outlier Detection")
      else character(0)
    )

    updateCheckboxGroupInput(
      session,
      "viz_types",
      selected = if (input$analysis_select_all)
        c("Boxplot", "Bar + Jitter Chart", "Standard Curve")
      else character(0)
    )
  })

  # Save selection
  observeEvent(input$to_results, {
    state$viz_types <- input$viz_types
    state$analysis_types <- input$analysis_types
  })
}
