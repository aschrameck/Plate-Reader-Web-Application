#' Main Application UI
#'
#' Defines the top-level user interface for the Plate Reader Analysis app.
#' This includes the global layout, navigation header, theming, and shared
#' resources (CSS/JS) that apply across all screens.
#'
#' @details
#' The UI is composed of:
#'
#' **Header Section**
#' - Application title
#' - Dark mode toggle
#' - User guide access icon
#' - Bug report link
#' - Step indicator and progress bar
#'
#' **Global Styling and Scripts**
#' - Bootstrap icon integration
#' - Custom CSS for layout and user guide modal
#' - Global Selectize.js enhancements:
#'   - Ellipsis truncation for long dropdown values
#'   - Tooltip display for full text on hover
#'
#' **Dynamic Content Area**
#' - `uiOutput("current_screen")` dynamically renders the active screen:
#'   - Upload
#'   - Inspect
#'   - Normalize
#'   - Analysis
#'   - Results
#'
#' **User Guide Modal Features**
#' - Sidebar navigation with smooth scrolling
#' - Search/filter functionality
#' - Responsive layout for large content display
#'
#' @return A `fluidPage` UI object for the full application
#'
#' @seealso main_server, run_app
#'
#' @family Main App

main_ui <- function() {

  lab_theme <- bs_theme(version = 5, bootswatch = "zephyr")

  fluidPage(
    theme = lab_theme,
    shinyjs::useShinyjs(),

    fluidRow(
      class = "pt-3 pb-2",
      style = "display: flex; align-items: center;",

      column(
        width = 7,
        div(class = "d-flex align-items-center gap-2",

            h2("Plate Reader Analysis", class = "m-0"),

            input_dark_mode(),

            tags$span(
              id = "user_guide_icon",
              style = "cursor:pointer;",
              HTML('<i class="bi bi-question-lg fs-5"></i>'),
              title = "User Guide"
            ),

            tags$span(
              id = "bug_report_icon",
              style = "cursor:pointer;",
              HTML('<i class="bi bi-bug-fill fs-5"></i>'),
              title = "Report a Bug"
            )
        )
      ),

      column(
        width = 5,
        div(
          class = "d-flex justify-content-end align-items-center gap-3",
          textOutput("step_label"),
          div(style = "width:180px;", uiOutput("progress_bar"))
        )
      ),

      tags$head(

        tags$link(
          rel = "stylesheet",
          href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.1/font/bootstrap-icons.css"
        ),

        tags$style(HTML("
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

      /* Modal width */
      .modal-dialog {
        max-width: 95vw !important;
      }

      /* Layout */
      .guide-container {
        display: flex;
        height: 80vh;
      }

      /* Sidebar */
      .guide-sidebar {
        width: 260px;
        border-right: 1px solid #ddd;
        padding: 15px;
      }

      /* Content */
      .guide-content {
        flex-grow: 1;
        padding: 25px;
        overflow-y: auto;
      }

      /* Sections */
      .guide-section {
        margin-bottom: 60px;
      }

      /* Nav */
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

      /* Screenshot formatting */
      .guide-img {
        width: 100%;
        border-radius: 8px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
      }

      .guide-mark {
        background-color: #0d6efd;   /* Bootstrap primary blue */
        color: white !important;
        padding: 1px 3px;
        border-radius: 3px;
      }

      .guide-mark-active {
        background-color: #084298 !important; /* darker blue for active */
        color: white !important;
        outline: 2px solid rgba(13,110,253,0.5);
      }

      ")),

        tags$script(HTML("

        function updatePlateTooltips() {
      var select = $('#active_plate')[0].selectize;
      if (!select) return;

      // Selected item div
      select.$control.children('div.item, div.single').attr('title', select.getValue());

      // Each dropdown option
      select.$dropdown_content.find('.option').each(function() {
        var val = $(this).attr('data-value');
        $(this).attr('title', val);
      });
    }

    // Initial load
    $(document).ready(function() {
      updatePlateTooltips();
    });

    // Update on change
    $(document).on('change', '#active_plate', function() {
      updatePlateTooltips();
    });

    // Update when dropdown opens
    $(document).on('click', '#active_plate + .selectize-control .selectize-input', function() {
      updatePlateTooltips();
    });

    // Open guide
    $(document).on('click', '#user_guide_icon', function() {
      Shiny.setInputValue('user_guide_icon', Date.now(), {priority:'event'});
    });

    // Bug link
    $(document).on('click', '#bug_report_icon', function() {
      window.open('https://forms.gle/egyr4C6T96c5RFfAA', '_blank');
    });

    // Smooth scroll
    $(document).on('click', '.guide-nav a', function(e) {
      e.preventDefault();
      var target = $(this).attr('href');
      var container = $('.guide-content');
      var scrollTo = $(target).position().top + container.scrollTop();

      container.animate({ scrollTop: scrollTo }, 400);
    });

    // Search
    $(document).on('keyup', '#guide_search', function() {
      var value = $(this).val().toLowerCase();

      $('.guide-section').each(function() {
        var text = $(this).text().toLowerCase();
        $(this).toggle(text.indexOf(value) > -1);
      });
    });"))
      )
    ),

    hr(),

    uiOutput("current_screen")
  )
}
