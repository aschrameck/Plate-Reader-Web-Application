#' Normalization Screen UI
#'
#' Provides controls for configuring and previewing normalization of plate data.
#'
#' @details
#' Normalization is performed per group using:
#' - Blank subtraction (background correction)
#' - Control-based scaling (relative normalization)
#'
#' Users can:
#' - Select plate for normalization
#' - Preview normalized values
#' - Validate group assignments before analysis
#'
#' Assumptions:
#' - Each group may contain blank and/or control wells
#' - If no control is present, normalization falls back to blank-corrected values
#'
#' @return A Shiny UI layout with normalization controls and preview outputs
#'
#' @seealso server_normalize

normalize_ui <- function() {
  tagList(
    # ---- Top Card: Plate selector + Downloads ----
    fluidRow(
      column(
        width = 8,
        offset = 2,
        card(
          card_header("Download Processed Data"),
          card_body(
            class = "h-100 w-100",
            div(
              class = "d-flex align-items-stretch",

              # --- Left: Plate selector ---
              div(
                class = "pe-3 border-end d-flex flex-column",
                style = "width: 50%;",
                selectInput(
                  "normalize_plate",
                  "Select plate",
                  choices = NULL
                )
              ),

              # --- Right: Download buttons ---
              div(
                class = "ps-3 d-flex flex-column",
                style = "width: 50%;",
                tags$label("Download", class = "form-label"),
                div(
                  class = "d-grid gap-2",
                  downloadButton("download_csv", "Detailed CSV", class = "w-100"),
                  downloadButton("download_prism", "Prism Compatible", class = "w-100"),
                  downloadButton("download_standards", "Standard Curves", class = "w-100")
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
            actionButton("to_inspect", "Back", class = "btn-secondary"),
            actionButton("to_analysis", "Next", class = "btn-primary")
          )
        )
      )
    )
  )
}
