server_results <- function(input, output, session, state, plates) {
  # --- Plate selector ---
  observe({
    req(state$screen == "results")
    req(length(plates()) > 0)

    updateSelectInput(
      session,
      "results",
      choices = names(plates()),
      selected = names(plates())[1]
    )
  })

  # --- Reactive: Selected plate ---
  selected_plate <- reactive({
    req(state$screen == "results")
    req(input$normalize_plate)
    req(input$normalize_plate %in% names(plates()))
    plates()[[input$normalize_plate]]
  })
}
