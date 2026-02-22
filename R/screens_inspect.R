inspect_ui <- function() {
  tagList(

    # ---- Top Row (Controls + Plot) ----
    fluidRow(
      class = "align-items-stretch",  # equal height columns

      # ---- Controls Column ----
      column(
        width = 3,
        class = "d-flex",

        card(
          class = "h-100 w-100",

          card_header("Label Wells"),

          selectInput(
            "active_plate",
            "Select plate",
            choices = NULL
          ),

          hr(),

          textInput(
            "new_label",
            "Label cells",
            placeholder = "5ml, Drug A, etc."
          ),

          actionButton(
            "apply_label",
            "Apply Label",
            class = "btn-primary w-100"
          ),

          actionButton(
            "clear_label",
            "Clear",
            class = "btn-secondary w-100",
            disabled = TRUE
          ),

          checkboxInput(
            "mark_control",
            "Control",
            value = FALSE
          ),

          checkboxInput(
            "mark_blank",
            "Blank",
            value = FALSE
          ),
        )
      ),

      # ---- Plot Column ----
      column(
        width = 9,
        class = "d-flex",

        card(
          class = "h-100 w-100",

          plotOutput(
            "plate_preview",
            height = "500px",
            brush = brushOpts(
              id = "plate_brush",
              resetOnNew = TRUE
            )
          )
        )
      )
    ),

    # Small spacing instead of big break
    div(class = "mt-3"),

    # ---- Bottom Navigation Row ----
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
              "to_normalize",
              "Next",
              class = "btn-primary",
              disabled = TRUE
            )
          )
        )
      )
    )
  )
}
