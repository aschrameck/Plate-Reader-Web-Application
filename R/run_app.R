#' Run Plate Reader Analysis Application
#'
#' Launches the full interactive Shiny application for plate reader data
#' processing, visualization, normalization, and statistical analysis.
#'
#' @details
#' This is the main entry point for the application. It initializes both the
#' user interface and server logic, connecting all workflow stages:
#'
#' **Application Workflow**
#' - Upload raw plate reader data
#' - Inspect and label wells
#' - Normalize data across groups
#' - Perform statistical analysis and view results
#'
#' **Architecture**
#' - `main_ui()` defines the global UI layout and shared resources
#' - `main_server()` coordinates application state and module interactions
#' - Modular server components handle each step of the workflow:
#'   - `server_upload`
#'   - `server_inspect`
#'   - `server_normalize`
#'   - `server_analysis`
#'   - `server_results`
#' - Modular UI components handle each screen:
#'   - `screens_upload`
#'   - `screens_inspect`
#'   - `screens_normalize`
#'   - `screens_analysis`
#'   - `screens_results`
#'
#' @return A running Shiny application
#'
#' @seealso main_ui, main_server
#'
#' @export
run_plate_reader_app <- function() {
  shiny::shinyApp(
    ui = main_ui(),
    server = app_server
  )
}
