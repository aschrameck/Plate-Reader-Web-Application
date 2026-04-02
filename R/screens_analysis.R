analysis_ui <- function() {
  tagList(
    # ---- Top Card: analysis selection ----
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
              choices = c("ANOVA", "Tukey’s Post-Hoc Test",
                          "Outlier Identification", "F Test")
            ),

            # Visualizations checkboxes
            checkboxGroupInput(
              "viz_types",
              "Visualizations",
              choices = c("Boxplot", "Bar + Jitter Chart", "Standard Curve")
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
)}
