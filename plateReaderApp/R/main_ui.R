main_ui <- function() {

  lab_theme <- bs_theme(
    version = 5,
    bootswatch = "zephyr"
  )

  fluidPage(
    theme = lab_theme,
    useShinyjs(),

    # Top header (persistent)
    fluidRow(
      class = "pt-3 pb-2",   # spacing top/bottom
      style = "display: flex; align-items: center;",  # vertical center for all children

      # Title + dark mode toggle
      column(
        width = 6,
        div(
          class = "d-flex align-items-center gap-3",
          h2("Plate Reader Analysis", class = "m-0"),  # remove default margin
          input_dark_mode()
        )
      ),

      # Step + progress (right-aligned, fixed layout)
      column(
        width = 6,
        div(
          class = "d-flex align-items-center justify-content-end gap-3",
          textOutput("step_label"),
          div(
            style = "width: 220px;",
            uiOutput("progress_bar")
          )
        )
      ),

      # Formatting
      tags$head(
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
  "))
      )
    ),

    hr(),
    uiOutput("current_screen")
  )}
