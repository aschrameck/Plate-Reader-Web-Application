normalize_ui <- function() {
  tagList(

    # ---- Top Row ----
    fluidRow(
      class = "align-items-stretch",  # forces equal column height

      column(
        width = 3,
        class = "d-flex",

        card(
          class = "h-100 w-100",

          card_header("Processed Data"),

          selectInput(
            "normalize_plate",
            "Select plate",
            choices = NULL
          ),

          hr(),

          tags$label("Download", class = "form-label"),

          div(
            class = "d-grid gap-2",

            downloadButton(
              "download_csv",
              "CSV",
              class = "w-100"
            ),

            downloadButton(
              "download_txt",
              "TXT",
              class = "w-100"
            ),

            downloadButton(
              "download_xlsx",
              "XLSX",
              class = "w-100"
            ),

            downloadButton(
              "download_prism",
              "Prism",
              class = "w-100"
            )
          )
        )
      ),

      column(
        width = 9,
        class = "d-flex",

        card(
          class = "h-100 w-100",

          plotOutput(
            "normalize_plot",
            height = "500px"
          )
        )
      )
    ),

    # Small spacing instead of big break
    div(class = "mt-3"),

    # ---- Full-Width Bottom Navigation ----
    fluidRow(
      column(
        width = 12,

        card(
          div(
            class = "d-flex justify-content-between",

            actionButton(
              "back_to_upload",
              "Back",
              class = "btn-secondary"
            ),

            actionButton(
              "to_analysis",
              "Next",
              class = "btn-primary"
            )
          )
        )
      )
    )
  )
}
