#' Results Screen UI
#'
#' Displays final analysis results, visualizations, and export options.
#'
#' @details
#' Users can:
#' - View statistical outputs and plots
#' - Download visualizations
#' - Generate a comprehensive teaching document
#'
#' Outputs may include:
#' - Boxplots of normalized values
#' - Statistical test results (ANOVA, etc.)
#' - Exportable PDF files
#'
#' @return A Shiny UI layout with plots, tables, and download controls
#'
#' @seealso server_results
#' @family UI Screens

results_ui <- function() {
  tagList(
    # ---- Top Card: Plate selector + Downloads ----
    fluidRow(
      column(
        width = 8,
        offset = 2,
        card(
          card_header("Download Outputs"),
          card_body(
            class = "h-100 w-100",
            div(
              class = "d-flex align-items-stretch",

              # --- Left: Plate selector ---
              div(
                class = "pe-3 border-end d-flex flex-column",
                style = "width: 50%;",
                selectInput(
                  "active_plate",
                  "Select plate",
                  choices = NULL
                ),
              ),

              # --- Right: Download buttons ---
              div(
                class = "ps-3 d-flex flex-column",
                style = "width: 50%;",
                tags$label("Download", class = "form-label"),

                div(
                  class = "d-grid gap-2",

                  # Analysis outputs
                  downloadButton("download_visualizations", "Visualizations (PDF/PNG)", class = "w-100"),
                  downloadButton("download_stats", "Statistical Test Outputs", class = "w-100"),

                  # Teaching material
                  downloadButton("download_teaching_pdf", "Teaching Document (PDF)", class = "w-100")
                )
              )
            )
          )
        )
      )
    ),

    # Small spacing
    div(class = "mt-2"),

    # ---- Bottom Card: Navigation ----
    fluidRow(
      column(
        width = 8,
        offset = 2,
        card(
          div(
            class = "d-flex justify-content-between",
            actionButton("to_analysis", "Back", class = "btn-secondary"),
            actionButton("to_upload", "Start New Analysis", class = "btn-primary")
          )
        )
      )
    )
  )
}
