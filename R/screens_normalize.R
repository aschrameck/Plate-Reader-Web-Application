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
              class = "d-flex align-items-start",

              # --- Left: Plate selector ---
              div(
                class = "pe-3 border-end", # padding + vertical line
                style = "min-width: 200px;",
                selectInput(
                  "normalize_plate",
                  "Select plate",
                  choices = NULL
                )
              ),

              # --- Right: Download buttons ---
              div(
                class = "ps-3 flex-grow-1",
                tags$label("Download", class = "form-label"),
                div(
                  class = "d-grid gap-2",
                  downloadButton("download_csv", "CSV", class = "w-100"),
                  downloadButton("download_prism", "Prism", class = "w-100"),
                  downloadButton("download_standards", "Standards", class = "w-100")
                )
              )
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
