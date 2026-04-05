#' Results Screen Server Logic
#'
#' Displays and exports final analysis results.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param state ReactiveValues object storing application state
#' @param plates Reactive object containing normalized/analysis-ready plate data
#' @param group_map Optional mapping of wells to groups
#'
#' @details
#' This server function provides:
#' - Reactive visualization of analysis results
#' - Tables summarizing group statistics
#' - Export functionality with correct formatting
#'
#' Users can:
#' - Download processed data
#' - Review group-level results visually and in tables
#'
#' @return NULL; reactive outputs and downloads are handled within
#'
#' @seealso results_ui
#' @family Server Screens

server_results <- function(input, output, session, state, plates, group_map) {

  # ---- Plate selector ----
  observeEvent(state$screen, {
    req(state$screen == "results")
    req(length(plates()) > 0)

    updateSelectInput(
      session,
      "active_plate",
      choices = names(plates())
    )
  })

  # ---- Reactive: Analysis-ready data ----
  analysis_data <- reactive({
    req(input$active_plate)
    req(group_map)

    gm <- group_map()
    req(nrow(gm) > 0)

    df <- gm %>%
      dplyr::filter(
        plate == input$active_plate,
        role %in% c("normal", "control")
      ) %>%
      dplyr::mutate(
        value = as.numeric(value),
        group = as.factor(group)
      )

    req(nrow(df) > 0)
    df
  })

  # ---- Plot generators ----
  make_boxplot <- function(df) {
    ggplot(df, aes(x = group, y = value, fill = group)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Boxplot", x = "Group", y = "Value") +
      theme(legend.position = "none")
  }

  make_bar_jitter <- function(df) {
    ggplot(df, aes(x = group, y = value, fill = group)) +
      stat_summary(fun = mean, geom = "bar", alpha = 0.7) +
      geom_jitter(width = 0.2, size = 2) +
      theme_minimal() +
      labs(title = "Bar + Jitter", x = "Group", y = "Value") +
      theme(legend.position = "none")
  }

  make_standard_curve <- function(df) {
    std <- df %>%
      dplyr::filter(grepl("^STD__", group)) %>%
      dplyr::mutate(
        conc = as.numeric(gsub("STD__", "", group))
      )

    req(nrow(std) > 1)

    ggplot(std, aes(x = conc, y = value)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal() +
      labs(title = "Standard Curve", x = "Concentration", y = "Value")
  }

  # ---- Generate selected plots ----
  selected_plots <- reactive({
    req(state$viz_types)
    df <- analysis_data()

    plots <- list()

    if ("Boxplot" %in% state$viz_types) {
      plots[["Boxplot"]] <- make_boxplot(df)
    }

    if ("Bar + Jitter Chart" %in% state$viz_types) {
      plots[["Bar + Jitter"]] <- make_bar_jitter(df)
    }

    if ("Standard Curve" %in% state$viz_types) {
      sc <- try(make_standard_curve(df), silent = TRUE)
      if (!inherits(sc, "try-error")) {
        plots[["Standard Curve"]] <- sc
      }
    }

    plots
  })

  # ---- Download PDF with all plots ----
  output$download_visualizations <- downloadHandler(
    filename = function() {
      paste0(input$active_plate, "_visualizations.pdf")
    },
    content = function(file) {

      plots <- selected_plots()
      req(length(plots) > 0)

      pdf(file, width = 10, height = 7)

      for (p in plots) {
        print(p)
      }

      dev.off()
    }
  )

}
