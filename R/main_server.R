#' Main Shiny Server Logic
#'
#' Coordinates application state, navigation, and shared data across all modules.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#'
#' @details
#' Maintains:
#' - Global screen state
#' - Uploaded plate data
#' - Shared group mapping across modules
#'
#' @return None (Shiny server function)

app_server <- function(input, output, session) {

  # ---- GLOBAL STATE ----
  state  <- reactiveValues(screen = "upload")
  plates <- reactiveVal(list())

  # ---- Shared group map ----
  # ---- Shared group map ----
  group_map <- reactive({

    req(length(plates()) > 0)

    purrr::imap_dfr(plates(), function(plate, plate_name) {

      # Remove empty wells early
      expanded <- plate %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::mutate(

          # Expand each well into potentially multiple group-role pairs
          group_list = purrr::pmap(
            list(is_control, is_blank, is_label, is_standard,
                 control_groups, blanks, labels, standards, standard_units),

            function(is_control, is_blank, is_label, is_standard,
                     control_groups, blanks, labels, standards, standard_units) {

              out <- list()

              # NOTE: One well can belong to multiple logical groups
              if (is_label && length(labels) > 0) {
                out[[length(out) + 1]] <- tibble::tibble(group = labels, role = "normal")
              }

              if (is_control && length(control_groups) > 0) {
                out[[length(out) + 1]] <- tibble::tibble(group = control_groups, role = "control")
              }

              if (is_blank && length(blanks) > 0) {
                out[[length(out) + 1]] <- tibble::tibble(group = blanks, role = "blank")
              }

              if (is_standard && !is.na(standards)) {
                std_group <- paste0("STD__", standards)
                out[[length(out) + 1]] <- tibble::tibble(group = std_group, role = "standard")
              }

              # Safety: always return tibble
              if (length(out) == 0) {
                return(tibble::tibble(group = character(0), role = character(0)))
              }

              dplyr::bind_rows(out)
            }
          )
        ) %>%
        tidyr::unnest(group_list)

      # Safety: skip empty plates
      if (nrow(expanded) == 0) return(NULL)

      expanded %>%
        dplyr::mutate(
          plate = plate_name,
          value = suppressWarnings(as.numeric(value))  # prevent crashes on bad input
        ) %>%
        dplyr::select(
          plate, row, col, value, group, role, standard_units
        )
    })
  })

  # ---- Navigation ----
  observeEvent(input$to_upload,      { state$screen <- "upload"; plates(list()) })
  observeEvent(input$to_inspect,     { state$screen <- "inspect" })
  observeEvent(input$to_normalize,   { state$screen <- "normalize" })
  observeEvent(input$to_analysis,    { state$screen <- "analysis" })
  observeEvent(input$to_results,     { state$screen <- "results" })

  # ---- Header label ----
  output$step_label <- renderText({
    switch(
      state$screen,
      upload    = "Step 1/5 - Upload",
      inspect   = "Step 2/5 - Select",
      normalize = "Step 3/5 - Process",
      analysis  = "Step 4/5 - Analyze",
      results   = "Step 5/5 - Results"
    )
  })

  # ---- Progress bar ----
  output$progress_bar <- renderUI({
    value <- switch(
      state$screen,
      upload    = 20,
      inspect   = 40,
      normalize = 60,
      analysis  = 80,
      results   = 100
    )

    tags$div(
      class = "progress w-100",
      style = "height: 1.2rem;",
      tags$div(
        class = "progress-bar",
        role = "progressbar",
        style = sprintf("width: %d%%;", value)
      )
    )
  })

  # ---- Screen rendering ----
  output$current_screen <- renderUI({
    switch(
      state$screen,
      upload    = upload_ui(),
      inspect   = inspect_ui(),
      normalize = normalize_ui(),
      analysis  = analysis_ui(),
      results   = results_ui()
    )
  })

  # ---- Call screen servers ----
  server_upload(input, output, session, state, plates)
  server_inspect(input, output, session, state, plates)
  server_normalize(input, output, session, state, plates, group_map)
  server_analysis(input, output, session, state, plates)
  server_results(input, output, session, state, plates, group_map)
}
