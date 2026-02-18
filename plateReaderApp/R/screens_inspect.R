inspect_ui <- function() {
  fluidRow(
    column(
      width = 3,
      card(
        card_header("Plate Controls"),

        selectInput(
          "active_plate",
          "Select plate",
          choices = NULL,
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

        hr(),

        actionButton(
          "inspect_ok",
          "Next",
          class = "btn-primary",
          disabled = TRUE
        )
      )
    ),

    column(
      width = 9,
      card(
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
  )
}
