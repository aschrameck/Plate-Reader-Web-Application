normalize_ui <- function() {
  fluidRow(
    column(
      width = 10,
      offset = 1,
      card(
        card_header("Processed Data"),
        card_body(
          tableOutput("normalized_preview"),
          br(),
          div(
            class = "d-flex gap-2",
            downloadButton("download_csv", "Download CSV"),
            downloadButton("download_prism", "Download Prism Format")
          ),
          hr(),
          div(
            class = "d-flex justify-content-between",
            actionButton("back_to_upload", "Back"),
            actionButton("to_analysis", "Next", class = "btn-primary")
          )
        )
      )
    )
  )
}
