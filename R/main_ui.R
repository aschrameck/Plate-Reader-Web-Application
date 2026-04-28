#' Main Application UI
#'
#' Constructs the top-level user interface for the Plate Reader Analysis app.
#' This function defines the global layout, navigation/header controls, theming,
#' and shared frontend resources (CSS/JavaScript) that persist across all screens.
#'
#' @details
#' The UI is organized into the following layers:
#'
#' **1. Global Theme & Dependencies**
#' - Bootswatch "Zephyr" theme via `{bslib}`
#' - `shinyjs` initialization for client-side interactivity
#' - Bootstrap Icons CDN for consistent iconography
#'
#' **2. Header / Navigation Bar**
#' A responsive horizontal layout containing:
#' - Application title ("Plate Reader Analysis")
#' - Dark mode toggle (`input_dark_mode()`)
#' - User Guide trigger icon (opens modal via JS → Shiny input)
#' - Bug report icon (opens external Google Form)
#' - Step label (`textOutput("step_label")`)
#' - Progress bar (`uiOutput("progress_bar")`)
#'
#' **3. Global Styling (CSS)**
#' Custom styles applied application-wide:
#'
#' *Selectize Enhancements*
#' - Truncates long selected values with ellipsis
#' - Applies hover tooltips to reveal full text
#' - Ensures dropdown options also truncate cleanly
#'
#' *User Guide Modal Layout*
#' - Expands modal width to ~95% of viewport
#' - Uses flexbox layout for sidebar + content
#' - Scrollable content panel with fixed sidebar
#' - Responsive image scaling:
#'   - Max width: 100%
#'   - Max height: 60% of viewport
#'   - Maintains aspect ratio with `object-fit: contain`
#'
#' *Search Highlighting*
#' - `.guide-mark`: default highlight style
#' - `.guide-mark-active`: active match emphasis
#'
#' **4. Global JavaScript Behavior**
#'
#' *Selectize Tooltip Sync*
#' - Dynamically updates tooltips for:
#'   - Selected value
#'   - Dropdown options
#' - Runs on:
#'   - Initial page load
#'   - Selection change
#'   - Dropdown open
#'
#' *Header Interactions*
#' - User guide icon → triggers `input$user_guide_icon`
#' - Bug icon → opens external bug report form in new tab
#'
#' *User Guide UX Enhancements*
#' - Smooth scrolling for sidebar navigation links
#' - Live search filter:
#'   - Filters `.guide-section` elements in real time
#'   - Case-insensitive matching
#'
#' **5. Dynamic Content Region**
#' - `uiOutput("current_screen")` serves as the main viewport
#' - Server dynamically injects the active workflow step:
#'   - Upload
#'   - Inspect
#'   - Normalize
#'   - Analysis
#'   - Results
#'
#' @return
#' A `shiny::fluidPage` object representing the complete application UI shell.
#'
#' @seealso main_server, run_app
#'
#' @family Main App

main_ui <- function() {

  # Initialize application theme
  lab_theme <- bs_theme(version = 5, bootswatch = "zephyr")

  fluidPage(
    theme = lab_theme,

    # Enable shinyjs utilities globally
    shinyjs::useShinyjs(),

    # ---- Header Row ----
    fluidRow(
      class = "pt-3 pb-2",
      style = "display: flex; align-items: center;",

      # ---- Left: Title + Controls ----
      column(
        width = 7,
        div(class = "d-flex align-items-center gap-2",

            # Application title
            h2("Plate Reader Analysis", class = "m-0"),

            # Dark mode toggle
            input_dark_mode(),

            # User Guide trigger icon (handled via JS → Shiny input)
            tags$span(
              id = "user_guide_icon",
              style = "cursor:pointer;",
              HTML('<i class="bi bi-question-lg fs-5"></i>'),
              title = "User Guide"
            ),

            # Bug report icon (opens external form)
            tags$span(
              id = "bug_report_icon",
              style = "cursor:pointer;",
              HTML('<i class="bi bi-bug-fill fs-5"></i>'),
              title = "Report a Bug"
            )
        )
      ),

      # ---- Right: Step + Progress ----
      column(
        width = 5,
        div(
          class = "d-flex justify-content-end align-items-center gap-3",

          # Current workflow step label
          textOutput("step_label"),

          # Progress bar container
          div(style = "width:180px;", uiOutput("progress_bar"))
        )
      ),

      # ---- Global Head Resources ----
      tags$head(

        # Bootstrap Icons
        tags$link(
          rel = "stylesheet",
          href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.1/font/bootstrap-icons.css"
        ),

        # ---------- Global CSS ----------
        tags$style(HTML("

        /* =========================
           Selectize Enhancements
        ========================= */

        /* Truncate selected value */
        .selectize-input > div {
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
          max-width: 90%;
          cursor: help;
        }

        /* Truncate dropdown options */
        .selectize-dropdown .option {
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }

        /* =========================
           Modal Layout (User Guide)
        ========================= */

        .modal-dialog {
          max-width: 95vw !important;
        }

        .modal-body {
          overflow: hidden;
        }

        /* Flex container for sidebar + content */
        .guide-container {
          display: flex;
          height: 80vh;
        }

        /* Sidebar navigation */
        .guide-sidebar {
          width: 260px;
          border-right: 1px solid #ddd;
          padding: 15px;
        }

        /* Scrollable content area */
        .guide-content {
          flex-grow: 1;
          padding: 25px;
          overflow-y: auto;
        }

        /* Section spacing */
        .guide-section {
          margin-bottom: 60px;
        }

        /* Navigation list */
        .guide-nav {
          list-style: none;
          padding-left: 0;
        }

        .guide-nav li {
          margin: 10px 0;
        }

        .guide-nav a {
          text-decoration: none;
          color: #0d6efd;
          cursor: pointer;
        }

        /* =========================
           Guide Images
        ========================= */

        .guide-img {
          width: 100%;
          margin: 15px 0;
          text-align: center;
        }

        .guide-img img {
          max-width: 100%;
          height: auto;
          max-height: 60vh;
          object-fit: contain;
          border-radius: 8px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.08);
        }

        /* =========================
           Search Highlighting
        ========================= */

        .guide-mark {
          background-color: #0d6efd;
          color: white !important;
          padding: 1px 3px;
          border-radius: 3px;
        }

        .guide-mark-active {
          background-color: #084298 !important;
          color: white !important;
          outline: 2px solid rgba(13,110,253,0.5);
        }

        ")),

        # ---------- Global JavaScript ----------
        tags$script(HTML("

        // =====================================
        // Selectize Tooltip Synchronization
        // =====================================
        function updatePlateTooltips() {
          var select = $('#active_plate')[0].selectize;
          if (!select) return;

          // Selected value tooltip
          select.$control.children('div.item, div.single')
            .attr('title', select.getValue());

          // Dropdown option tooltips
          select.$dropdown_content.find('.option').each(function() {
            var val = $(this).attr('data-value');
            $(this).attr('title', val);
          });
        }

        // Initial load
        $(document).ready(function() {
          updatePlateTooltips();
        });

        // On selection change
        $(document).on('change', '#active_plate', function() {
          updatePlateTooltips();
        });

        // When dropdown is opened
        $(document).on('click',
          '#active_plate + .selectize-control .selectize-input',
          function() {
            updatePlateTooltips();
          }
        );

        // =====================================
        // Header Actions
        // =====================================

        // Trigger Shiny event for user guide
        $(document).on('click', '#user_guide_icon', function() {
          Shiny.setInputValue('user_guide_icon', Date.now(), {priority:'event'});
        });

        // Open bug report form
        $(document).on('click', '#bug_report_icon', function() {
          window.open('https://forms.gle/odLZSkXPvjNz3aVx9', '_blank');
        });

        // =====================================
        // User Guide Interactions
        // =====================================

        // Smooth scrolling for sidebar navigation
        $(document).on('click', '.guide-nav a', function(e) {
          e.preventDefault();

          var target = $(this).attr('href');
          var container = $('.guide-content');
          var scrollTo = $(target).position().top + container.scrollTop();

          container.animate({ scrollTop: scrollTo }, 400);
        });

        // Live search filter for guide content
        $(document).on('keyup', '#guide_search', function() {
          var value = $(this).val().toLowerCase();

          $('.guide-section').each(function() {
            var text = $(this).text().toLowerCase();
            $(this).toggle(text.indexOf(value) > -1);
          });
        });

        "))
      )
    ),

    hr(),

    # ---- Dynamic Screen Output ----
    uiOutput("current_screen")
  )
}
