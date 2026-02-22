app_server <- function(input, output, session) {

  # ---- GLOBAL STATE ----
  state  <- reactiveValues(screen = "upload")
  plates <- reactiveVal(list())

  # ---- Shared group map ----
  group_map <- reactive({

    req(length(plates()) > 0)

    purrr::imap_dfr(plates(), function(plate, plate_name) {

      expanded <- plate %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::mutate(
          group_list = purrr::pmap(
            list(is_control, is_blank, is_label,
                 control_groups, blanks, labels),
            function(is_control, is_blank, is_label,
                     control_groups, blanks, labels) {

              if (is_control && length(control_groups) > 0) {
                tibble::tibble(group = control_groups, role = "control")

              } else if (is_blank && length(blanks) > 0) {
                tibble::tibble(group = blanks, role = "blank")

              } else if (is_label && length(labels) > 0) {
                tibble::tibble(group = labels, role = "normal")

              } else {
                tibble::tibble(group = character(0), role = character(0))  # <--- always a tibble
              }
            }
          )
        ) %>%
        tidyr::unnest(group_list)

      if (nrow(expanded) == 0) return(NULL)

      expanded %>%
        dplyr::mutate(
          plate = plate_name,
          value = suppressWarnings(as.numeric(value))
        ) %>%
        dplyr::select(
          plate,
          row,
          col,
          value,
          group,
          role
        )
    })
  })

  # ---- Navigation ----
  observeEvent(input$to_inspect,     { state$screen <- "inspect" })
  observeEvent(input$to_normalize,   { state$screen <- "normalize" })
  observeEvent(input$back_to_upload, { state$screen <- "upload" })
  observeEvent(input$to_analysis,    { state$screen <- "analysis" })
  observeEvent(input$run_analysis,   { state$screen <- "results" })
  observeEvent(input$start_over,     { state$screen <- "upload" })

  # ---- Header label ----
  output$step_label <- renderText({
    switch(
      state$screen,
      upload    = "Step 1/5 - Upload",
      inspect   = "Step 2/5 - Select",
      normalize = "Step 3/5 - Normalization",
      analysis  = "Step 4/5 - Analysis",
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
  server_results(input, output, session, state, plates)
}
