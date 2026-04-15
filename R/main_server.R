#' Main Application Server
#'
#' Controls global application state, navigation, and coordination between
#' all analysis modules in the Plate Reader Analysis app.
#'
#' @details
#' The server manages:
#'
#' **Application State**
#' - `state$screen`: Tracks the current active screen
#' - Controls navigation flow between:
#'   - Upload → Inspect → Normalize → Results
#'
#' **Shared Reactive Data**
#' - `plates`: Uploaded and processed plate datasets
#' - `group_map`: Stores well group assignments and annotations
#'
#' **Screen Routing**
#' - Dynamically renders UI via `output$current_screen`
#' - Delegates logic to modular server functions:
#'   - `server_upload`
#'   - `server_inspect`
#'   - `server_normalize`
#'   - `server_analysis`
#'   - `server_results`
#'
#' **Progress Tracking**
#' - Updates step indicator text
#' - Renders progress bar based on workflow stage
#'
#' **Global Events**
#' - User guide modal trigger
#' - Navigation button observers
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#'
#' @return None (server-side side effects)
#'
#' @seealso main_ui, run_app
#' @family Main App

app_server <- function(input, output, session) {

  # --- Global State ---
  state <- reactiveValues(screen = "upload",
                          viz_types = NULL)

  plates <- reactiveVal(list())

  # ---- Shared group map ----
  group_map <- reactive({

    req(length(plates()) > 0)

    purrr::imap_dfr(plates(), function(plate, plate_name) {

      expanded <- plate %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::mutate(
          group_list = purrr::pmap(
            list(is_control, is_blank, is_label, is_standard,
                 control_groups, blanks, labels, standards, standard_units),
            function(is_control, is_blank, is_label, is_standard,
                     control_groups, blanks, labels, standards, standard_units) {

              out <- list()

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

              if (length(out) == 0) {
                return(tibble::tibble(group = character(0), role = character(0)))
              }

              dplyr::bind_rows(out)
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
          role,
          standard_units
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
  normalized_data <- server_normalize(input, output, session, state, plates, group_map)
  server_analysis(input, output, session, state, plates, normalized_data)
  server_results(input, output, session, state, plates, normalized_data)

  # ---- USER GUIDE ----
  observeEvent(input$user_guide_icon, {

    showModal(
      modalDialog(
        title = "User Guide",
        size = "l",
        easyClose = TRUE,
        footer = NULL,

        div(class="guide-container",

            # Sidebar
            div(class="guide-sidebar",
                textInput("guide_search", NULL, placeholder = "Search..."),

                tags$ul(class="guide-nav",
                        tags$li(tags$a("Overview", href="#guide_overview")),
                        tags$li(tags$a("Workflow", href="#guide_workflow")),
                        tags$li(tags$a("Upload", href="#guide_upload")),
                        tags$li(tags$a("Labeling", href="#guide_label")),
                        tags$li(tags$a("Normalization", href="#guide_norm")),
                        tags$li(tags$a("Analysis", href="#guide_analysis")),
                        tags$li(tags$a("Results", href="#guide_results")),
                        tags$li(tags$a("Examples", href="#guide_examples")),
                        tags$li(tags$a("FAQ", href="#guide_faq"))
                )
            ),

            # Content
            div(class="guide-content",

                div(id="guide_overview", class="guide-section",
                    tags$h2("Overview"),
                    tags$p("This application guides you through analyzing plate reader data from raw files to statistical results and publication-ready outputs.")
                ),

                div(id="guide_workflow", class="guide-section",
                    tags$h2("Workflow"),
                    tags$ol(
                      tags$li("Upload raw plate data"),
                      tags$li("Inspect and label wells"),
                      tags$li("Normalize values"),
                      tags$li("Run statistical analysis"),
                      tags$li("Export results")
                    )
                ),

                div(id="guide_upload", class="guide-section",
                    tags$h2("Upload Data"),
                    tags$p("Upload CSV, TXT, or Excel files. Multiple plates are supported."),
                    tags$div(class="guide-img","[Add screenshot here]")
                ),

                div(id="guide_label", class="guide-section",
                    tags$h2("Label Wells"),
                    tags$p("Use brushing to select wells and assign roles."),
                    tags$ul(
                      tags$li("Label = experimental condition"),
                      tags$li("Control = normalization baseline"),
                      tags$li("Blank = background signal"),
                      tags$li("Standard = known concentration")
                    )
                ),

                div(id="guide_norm", class="guide-section",
                    tags$h2("Normalization"),
                    tags$p("Normalization is performed per group using blank subtraction and optional control scaling.")
                ),

                div(id="guide_analysis", class="guide-section",
                    tags$h2("Analysis"),
                    tags$p("Perform ANOVA, Tukey post-hoc tests, and visualize distributions.")
                ),

                div(id="guide_results", class="guide-section",
                    tags$h2("Results"),
                    tags$p("Download plots, statistical outputs, and teaching reports.")
                ),

                div(id="guide_examples", class="guide-section",
                    tags$h2("Examples"),
                    tags$h4("Example 1: Drug Response"),
                    tags$p("Compare treatment vs control groups using ANOVA."),
                    tags$h4("Example 2: Standard Curve"),
                    tags$p("Use standards to generate a calibration curve.")
                ),

                div(id="guide_faq", class="guide-section",
                    tags$h2("Frequently Asked Questions"),
                    tags$h4("Why is my plot empty?"),
                    tags$p("Ensure groups are labeled and selected in analysis."),
                    tags$h4("Why is normalization incorrect?"),
                    tags$p("Check that blanks and controls are properly assigned."),
                    tags$h4("Why no standard curve?"),
                    tags$p("At least two standard values are required.")
                )

            )
        )
      )
    )
  })
}
