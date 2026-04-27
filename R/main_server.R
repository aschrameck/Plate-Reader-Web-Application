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
#'   - Upload → Inspect → Normalize → Analysis → Results
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

  # --- User Guide Search ---
  observe({

    query <- input$guide_search

    # Reset
    if (is.null(query) || trimws(query) == "") {
      shinyjs::runjs("
      window.guideMatches = [];
      window.guideIndex = -1;

      $('.guide-section').each(function() {
        $(this).html($(this).data('original-html'));
      });
    ")
      return()
    }

    query <- tolower(query)

    shinyjs::runjs(sprintf("
    var query = '%s';

    window.guideMatches = [];
    window.guideIndex = -1;

    $('.guide-section').each(function() {

      var el = $(this);

      if (!el.data('original-html')) {
        el.data('original-html', el.html());
      }

      var html = el.data('original-html');

      var safeQuery = query.replace(/[.*+?^${}()|[\\]\\\\]/g, '\\\\$&');
      var regex = new RegExp('(' + safeQuery + ')', 'gi');

      var newHtml = html.replace(regex, '<span class=\"guide-mark\">$1</span>');

      el.html(newHtml);
    });

    // Collect matches
    window.guideMatches = $('.guide-mark');

    if (window.guideMatches.length > 0) {

      window.guideIndex = 0;

      $('.guide-mark').removeClass('guide-mark-active');

      var el = $(window.guideMatches[0]);
      el.addClass('guide-mark-active');

      // FIXED SCROLL (wait for DOM)
      setTimeout(function() {
        var container = $('.modal-body');

        if (el.length) {
          container.animate({
            scrollTop: el.position().top + container.scrollTop() - container.height()/2
          }, 300);
        }
      }, 50);
    }

  ", query))
  })

  observe({

    shinyjs::runjs("
    if (!window.guideKeyListenerAdded) {

      window.guideKeyListenerAdded = true;

      $(document).on('keydown', function(e) {

        if (!window.guideMatches || window.guideMatches.length === 0) return;

        if (e.key === 'Enter') {

          e.preventDefault();

          if (e.shiftKey) {
            window.guideIndex--;
          } else {
            window.guideIndex++;
          }

          if (window.guideIndex >= window.guideMatches.length) {
            window.guideIndex = 0;
          }

          if (window.guideIndex < 0) {
            window.guideIndex = window.guideMatches.length - 1;
          }

          $('.guide-mark').removeClass('guide-mark-active');

          var el = $(window.guideMatches[window.guideIndex]);
          el.addClass('guide-mark-active');

          // FIXED SCROLL (always to active)
          setTimeout(function() {
            var container = $('.modal-body');

            if (el.length) {
              container.animate({
                scrollTop: el.position().top + container.scrollTop() - container.height()/2
              }, 200);
            }
          }, 50);

          // subtle visual feedback
          el.fadeOut(70).fadeIn(70);
        }
      });
    }
  ")
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
                        tags$li(tags$a("Inspect", href="#guide_label")),
                        tags$li(tags$a("Normalization", href="#guide_norm")),
                        tags$li(tags$a("Analysis", href="#guide_analysis")),
                        tags$li(tags$a("Results", href="#guide_results")),
                        tags$li(tags$a("FAQs", href="#guide_faq"))
                )
            ),

            # Content
            div(class="guide-content",

                # ---------------- Overview ----------------
                div(id="guide_overview", class="guide-section",
                    tags$h2("Overview"),
                    tags$p("This application provides an end-to-end workflow for analyzing plate reader data, from raw file upload to statistical testing and publication-ready outputs."),

                    tags$h4("What you can do:"),
                    tags$ul(
                      tags$li("Upload and parse multiple plate files"),
                      tags$li("Interactively inspect and label wells"),
                      tags$li("Normalize data using blanks, controls, and standards"),
                      tags$li("Run statistical tests and generate visualizations"),
                      tags$li("Export results in multiple formats")
                    ),

                    tags$h4("How-To Video"),
                    tags$div(class="guide-video", "[Embed tutorial video here]")
                ),

                tags$hr(),

                # ---------------- Workflow ----------------
                div(id="guide_workflow", class="guide-section",
                    tags$h2("Workflow"),
                    tags$ol(
                      tags$li(tags$b("Upload:"), " Import raw plate data (CSV, TXT, Excel)."),
                      tags$li(tags$b("Inspect & Label:"), " Assign groups, controls, blanks, and standards."),
                      tags$li(tags$b("Normalization:"), " Apply background correction and scaling."),
                      tags$li(tags$b("Analysis & Visualization:"), " Select statistical tests and plots."),
                      tags$li(tags$b("Results & Export:"), " Download figures, tables, or full reports.")
                    )
                ),

                tags$hr(),

                # ---------------- Upload ----------------
                div(id="guide_upload", class="guide-section",
                    tags$h2("Upload Data"),

                    tags$p("Upload one or more CSV, TXT, or Excel files. Files containing multiple plates are automatically seperated and labelled with a number and the original filename."),

                    tags$h4("Example Input: With Headers"),
                    tags$div(class="guide-img", tags$img(src="guide/Raw-Data-With-Headers.png")),
                    tags$div(class="guide-img", tags$img(src="guide/Inspect-With-Headers.png")),

                    tags$h4("Example Input: Without Headers"),
                    tags$div(class="guide-img", tags$img(src="guide/Raw-Data-Without-Headers.png")),
                    tags$div(class="guide-img", tags$img(src="guide/Inspect-Without-Headers.png")),

                    tags$p(tags$b("Tip:"), " If headers are present, ensure the 'File contains headers' checkbox is selected. Incorrect selection can cause spacing or alignment issues.")
                ),

                tags$hr(),

                # ---------------- Inspect ----------------
                div(id="guide_label", class="guide-section",
                    tags$h2("Inspect Wells"),

                    tags$p("Use the brushing tool to select wells and assign them to experimental groups."),

                    tags$h4("Label Types"),
                    tags$ul(
                      tags$li(tags$b("Label:"), " Experimental condition (what you want to compare)"),
                      tags$li(tags$b("Control:"), " Baseline reference group"),
                      tags$li(tags$b("Blank:"), " Background signal for subtraction"),
                      tags$li(tags$b("Standard:"), " Known concentration for calibration")
                    ),

                    tags$h4("Applying Labels:"),
                    tags$div(class="guide-img", tags$img(src="guide/Brushing-Wells.png")),
                    tags$div(class="guide-img", tags$img(src="guide/Label-Wells.png")),

                    tags$p(tags$b("Tip:"), " The groups you assign determine what will be compared in analysis."),

                    tags$p(tags$b("Controls across multiple groups:"),
                           " If a control applies to multiple experimental groups, it must be assigned separately to each group."
                    ),

                    tags$div(class="guide-img", tags$img(src="guide/Multigroup-Controls.png")),

                    tags$h4("Standards"),
                    tags$p("To assign numeric concentrations to standard wells: "),
                    tags$ol(
                      tags$li("Select the 'Standard' checkbox"),
                      tags$li("Enter the concentration as a label"),
                      tags$li("Enter the units (if available)"),
                      tags$li("Drag and select desired well"),
                      tags$li("Click 'Apply Label'")
                    ),
                    tags$div(class="guide-img", tags$img(src="guide/Standard-Wells.png"))
                ),

                tags$hr(),

                # ---------------- Normalization ----------------
                div(id="guide_norm", class="guide-section",
                    tags$h2("Normalization"),

                    tags$p("Normalization adjusts raw values to remove background signal and optionally scale relative to controls."),

                    tags$h4("Normalization Formula"),
                    tags$pre("(Value - Mean Blank) / Mean Control"),

                    tags$p("Steps:"),
                    tags$ul(
                      tags$li("Subtract blank signal from all wells"),
                      tags$li("Divide by control mean"),
                      tags$li("Process is applied per group")
                    ),

                    tags$h4("Standard Curve Prediction"),
                    tags$p("If standards are provided, a calibration curve is generated and used to estimate concentrations for unknown samples."),

                    tags$h2("Download Options"),

                    tags$h4(tags$b("Detailed CSV:"), " Fully processed dataset with all metadata"),
                    tags$div(class="guide-img", tags$img(src="guide/Processed-Data.png")),

                    tags$h4(tags$b("Prism-Compatible:"), " Formatted for GraphPad Prism import"),
                    tags$div(class="guide-img", tags$img(src="guide/Prism-Data.png")),

                    tags$h4(tags$b("Standards File:"), " Standard curve data and fitted values"),
                    tags$div(class="guide-img", tags$img(src="guide/Standards-Data.png")),
                ),

                tags$hr(),

                # ---------------- Analysis ----------------
                div(id="guide_analysis", class="guide-section",
                    tags$h2("Analysis & Visualization"),

                    tags$h4("Statistical Tests"),
                    tags$ul(
                      tags$li(tags$b("T-test:"), " Includes pairwise comparisons and control vs treatment"),
                      tags$li(tags$b("ANOVA:"), " Multi-group comparison with Tukey post-hoc significance"),
                      tags$li(tags$b("Outlier Detection:"), " Identifies extreme values using 1.5 x IQR")
                    ),

                    tags$h4("Visualizations"),
                    tags$ul(
                      tags$li("Boxplot"),
                      tags$li("Bar chart"),
                      tags$li("Standard curve")
                    ),

                    tags$h4("Warnings vs Errors"),
                    tags$p(tags$b("Warnings:"),
                           " Provide information about potential issues (e.g., variance differences). Results can still be interpreted but with caution."
                    ),

                    tags$p(tags$b("Errors:"),
                           " Indicate that assumptions of a statistical test are violated (e.g., insufficient groups, missing data). These must be fixed before proceeding."
                    ),

                    tags$p("If errors occur, consider adjusting labels or deselecting incompatible tests."),

                    tags$div(class="guide-img", tags$img(src="guide/Warnings-Errors.png"))
                ),

                tags$hr(),

                # ---------------- Results ----------------
                div(id="guide_results", class="guide-section",
                    tags$h2("Results & Export"),

                    tags$p("Download results in multiple formats depending on your needs:"),

                    tags$ul(
                      tags$li(tags$b("Visualizations:"), " Plots only"),
                      tags$li(tags$b("Statistical Analysis:"), " Tables and test results"),
                      tags$li(tags$b("Full Report:"), " Combined methods, figures, and tables")
                    ),

                    tags$h4(tags$b("Example:"), " Boxplot"),
                    tags$div(class="guide-img", tags$img(src="guide/Box-Plot.png")),

                    tags$h4(tags$b("Example:"), " Bar Chart"),
                    tags$div(class="guide-img", tags$img(src="guide/Bar-Chart.png")),

                    tags$h4(tags$b("Example:"), " Standard Curve"),
                    tags$div(class="guide-img", tags$img(src="guide/Standard-Curve.png"))
                ),

                tags$hr(),

                # ---------------- FAQ ----------------
                div(id="guide_faq", class="guide-section",
                    tags$h2("Frequently Asked Questions"),

                    tags$h4("Why is the spacing weird or headers appear incorrectly?"),
                    tags$p("This usually occurs when the 'Has Header' option was selected incorrectly or non-numeric values were included in the plate."),

                    tags$h4("How do I switch between plates?"),
                    tags$p("Multiple plates are loaded as 'Plate_X_filename'. Use the dropdown selector on each page to switch between them."),

                    tags$h4("Why is my plot empty?"),
                    tags$p("Ensure groups are labeled and selected in the analysis step."),

                    tags$h4("Why is normalization incorrect?"),
                    tags$p("Check that blanks and controls are properly assigned."),

                    tags$h4("Why is there no standard curve?"),
                    tags$p("At least two standard values are required.")
                )

            )
        )
      )
    )
  })
}
