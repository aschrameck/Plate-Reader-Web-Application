server_analysis <- function(input, output, session, state, plates) {
  # Analysis page select-all
  observeEvent(input$analysis_select_all, {
    updateCheckboxGroupInput(
      session,
      "analysis_types",
      selected = if (input$analysis_select_all)
        c("t-test", "ANOVA", "Nonparametric test") else character(0)
    )

    updateCheckboxGroupInput(
      session,
      "viz_types",
      selected = if (input$analysis_select_all)
        c("Boxplot", "Bar chart", "Heatmap") else character(0)
    )
  })
}
