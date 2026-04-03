server_analysis <- function(input, output, session, state, plates) {

  # ---- Plate selector ----
  observe({
    req(state$screen == "analysis")
    req(length(plates()) > 0)

    updateSelectInput(
      session,
      "analysis_plate",
      choices = names(plates()),
      selected = names(plates())[1]
    )
  })

  # ---- Select-all toggle ----
  observeEvent(input$analysis_select_all, {

    updateCheckboxGroupInput(
      session,
      "analysis_types",
      selected = if (input$analysis_select_all)
        c("ANOVA", "Tukey’s Post-Hoc Test",
          "Outlier Identification", "F Test")
      else character(0)
    )

    updateCheckboxGroupInput(
      session,
      "viz_types",
      selected = if (input$analysis_select_all)
        c("Boxplot", "Bar + Jitter Chart", "Standard Curve")
      else character(0)
    )
  })
}
