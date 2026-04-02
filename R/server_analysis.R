server_analysis <- function(input, output, session, state, plates) {
  # Analysis page select-all
  observeEvent(input$analysis_select_all, {
    updateCheckboxGroupInput(
      session,
      "analysis_types",
      selected = if (input$analysis_select_all)
        c("ANOVA", "Tukey’s Post-Hoc Test", "Outlier Identification", "F Test") else character(0)
    )

    updateCheckboxGroupInput(
      session,
      "viz_types",
      selected = if (input$analysis_select_all)
        c("Boxplot", "Bar + Jitter Chart", "Standard Curve") else character(0)
    )
  })
}
