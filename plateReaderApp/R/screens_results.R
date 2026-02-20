results_ui <- function() {
  fluidRow(
    column(
      width = 10,
      offset = 1,
      card(
        card_header("Results & Downloads"),
        card_body(
          plotOutput("final_plots"),
          br(),
          div(
            class = "d-flex gap-2 flex-wrap",
            downloadButton("download_normalized", "Download Normalized Data"),
            downloadButton("download_pdf", "Download PDF"),
            downloadButton("download_teaching", "Download Teaching Document")
          ),
          hr(),
          div(
            style = "text-align: right;",
            actionButton("start_over", "Start New Analysis", class = "btn-primary")
          )
        )
      )
    )
  )
}
