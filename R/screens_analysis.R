#' Analysis Screen UI
#'
#' Provides tools for selecting and performing statistical analysis on normalized data.
#'
#' @details
#' Users can:
#' - Choose statistical tests (e.g., ANOVA, t-test)
#' - Generate plots and statistical summaries
#'
#' Supported analyses may include:
#' - One-way ANOVA
#' - Pairwise t-tests
#' - Outlier detection
#'
#' Results are based on normalized values from the previous step.
#'
#' @return A Shiny UI layout containing analysis options and result outputs
#'
#' @seealso server_results
#' @family UI Screens

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

            # ---- Plate selector (NEW) ----
            div(
              class = "mb-3",
              selectInput(
                "analysis_plate",
                "Select plate",
                choices = NULL
              )
            ),

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
                    "T-test", "ANOVA", "Tukey’s Post-Hoc Test", "Outlier Detection"
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

            # ---- Select All ----
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
