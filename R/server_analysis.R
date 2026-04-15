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

server_analysis <- function(input, output, session, state, plates) {

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
