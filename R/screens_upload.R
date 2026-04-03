#' Upload Screen UI
#'
#' Provides interface for uploading plate reader data files and initializing
#' the application workflow.
#'
#' @details
#' This is the entry point of the application. Users can:
#' - Upload raw plate reader files
#' - Initialize plate data structures
#' - Transition to inspection and analysis workflows
#'
#' Expected input:
#' - File formats compatible with the plate parsing logic
#'
#' @return A Shiny UI layout containing file input controls and navigation buttons
#'
#' @seealso server_upload

upload_ui <- function() {
  tagList(
    tags$head(
      tags$style(HTML("
        .upload-wrapper .progress {
          display: none;
        }

        .upload-status {
          min-width: 100px;
          margin-left: 3px;
          margin-top: 8px;
          font-size: 1rem;
          color: #6c757d;
          white-space: nowrap;
        }

        .upload-status.success {
          color: #198754;
          font-size: 1.5rem;
        }
      "))
    ),

    fluidRow(
      column(
        width = 8,
        offset = 2,
        card(
          card_header("Upload Plate Data"),
          card_body(
            div(
              class = "upload-wrapper",
              style = "display: flex; align-items: center;",

              fileInput(
                "data_files",
                "Upload files",
                multiple = TRUE,
                accept = c(".csv", ".txt", ".xlsx"),
                width = "100%"
              ),

              div(
                id = "upload_status",
                class = "upload-status"
              )
            ),

            tags$small(
              class = "text-muted d-block",
              style = "margin-top: -30px; margin-left: 2px",
              "Accepted file types: .csv, .txt, .xlsx"
            ),

            checkboxInput(
              "has_header",
              "File contains headers",
              value = TRUE
            ),

            div(
              style = "text-align: right;",
              actionButton(
                "to_inspect",
                "Next",
                class = "btn-primary",
                disabled = TRUE
              )
            )
          )
        )
      )
    )
  )
}
