analysis_ui <- function() {
  fluidRow(
    column(
      width = 8,
      offset = 2,
      card(
        card_header("Statistical Analysis Options"),
        card_body(
          # Statistical tests checkboxes
          checkboxGroupInput(
            "analysis_types",
            "Statistical tests",
            choices = c("t-test", "ANOVA", "Nonparametric test")
          ),

          # Visualizations checkboxes
          checkboxGroupInput(
            "viz_types",
            "Visualizations",
            choices = c("Boxplot", "Bar chart", "Heatmap")
          ),

          # Bottom row: Select All left, Run Analysis right
          div(
            class = "d-flex justify-content-between align-items-center mt-3",

            # Left: Select All
            div(
              class = "d-flex align-items-center",
              style = "margin-top: 20px",
              checkboxInput(
                "analysis_select_all",
                "Select All",
                value = FALSE
              )
            ),

            # Right: Run Analysis
            actionButton("run_analysis", "Run Analysis", class = "btn-primary")
          )
        )
      )
    )
  )
}
