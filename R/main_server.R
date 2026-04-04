app_server <- function(input, output, session) {

  state <- reactiveValues(screen = "upload")
  plates <- reactiveVal(list())

  observeEvent(input$to_upload, { state$screen <- "upload"; plates(list()) })
  observeEvent(input$to_inspect, { state$screen <- "inspect" })
  observeEvent(input$to_normalize, { state$screen <- "normalize" })
  observeEvent(input$to_analysis, { state$screen <- "analysis" })
  observeEvent(input$to_results, { state$screen <- "results" })

  output$step_label <- renderText({
    switch(state$screen,
           upload = "Step 1/5 - Upload",
           inspect = "Step 2/5 - Select",
           normalize = "Step 3/5 - Process",
           analysis = "Step 4/5 - Analyze",
           results = "Step 5/5 - Results"
    )
  })

  output$progress_bar <- renderUI({
    value <- switch(state$screen, upload=20, inspect=40, normalize=60, analysis=80, results=100)
    tags$div(class="progress",
             tags$div(class="progress-bar", style=paste0("width:", value, "%"))
    )
  })

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

  output$current_screen <- renderUI({
    switch(state$screen,
           upload = upload_ui(),
           inspect = inspect_ui(),
           normalize = normalize_ui(),
           analysis = analysis_ui(),
           results = results_ui()
    )
  })

  server_upload(input, output, session, state, plates)
  server_inspect(input, output, session, state, plates)
  server_normalize(input, output, session, state, plates, NULL)
  server_analysis(input, output, session, state, plates)
  server_results(input, output, session, state, plates, NULL)
}
