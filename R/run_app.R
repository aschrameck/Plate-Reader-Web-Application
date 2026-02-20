#' Run Plate Reader App
#'
#' Launches the interactive plate reader analysis Shiny app.
#'
#' @export
run_plate_reader_app <- function() {
  shiny::shinyApp(
    ui = main_ui(),
    server = app_server
  )
}
