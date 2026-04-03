analysis_ui <- function() {
  tagList(
    # ---- Top Card: analysis selection ----
    fluidRow(
      column(
        width = 8,
        offset = 2,
        card(
          card_header("Statistical Analysis"),
          card_body(
            # ---- Two-column layout ----
            div(
              class = "row",

              # LEFT: Statistical tests
              div(
                class = "col-md-6",
                checkboxGroupInput(
                  "analysis_types",
                  "Statistical tests",
                  choices = c(
                    "ANOVA",
                    "Tukey’s Post-Hoc Test",
                    "Outlier Identification",
                    "F-Test with Bootstrapping"
                  )
                )
              ),

              # RIGHT: Visualizations
              div(
                class = "col-md-6",
                checkboxGroupInput(
                  "viz_types",
                  "Visualizations",
                  choices = c(
                    "Boxplot",
                    "Bar + Jitter Chart",
                    "Standard Curve"
                  )
                )
              )
            ),

            # ---- Select All (bottom) ----
            div(
              class = "d-flex justify-content-start align-items-center mt-3",
              checkboxInput(
                "analysis_select_all",
                "Select All",
                value = FALSE
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
            actionButton("to_normalize", "Back", class = "btn-secondary"),
            actionButton("to_results", "Next", class = "btn-primary")
          )
        )
      )
    )
  )
}
