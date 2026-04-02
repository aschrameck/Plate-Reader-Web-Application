results_ui <- function() {
  tagList(
    # ---- Top Card: preview + download----
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
            )
          )
        )
      )
    ),

    # Small spacing
    div(class = "mt-3"),

    # ---- Bottom Card: Navigation ----
    fluidRow(
      column(
        width = 10,
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
)}
