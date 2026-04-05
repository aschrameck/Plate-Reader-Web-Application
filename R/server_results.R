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
        role %in% c("normal", "control", "standard")
      ) %>%
      dplyr::mutate(
        value = as.numeric(value),
        group = as.factor(group)
      )

    req(nrow(df) > 0)
    df
  })

  # --- Plot formatting ---
  pub_theme <- function() {
    theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none"
      )
  }

  pub_colors <- c("#4C78A8", "#F58518", "#54A24B", "#E45756", "#72B7B2")

  # ---- Plot generators ----
  make_boxplot <- function(df) {
    ggplot(df, aes(x = group, y = value, fill = group)) +
      geom_boxplot(alpha = 0.8, outlier.shape = 16, outlier.size = 2) +
      scale_fill_manual(values = pub_colors) +
      labs(
        title = "Distribution of Values by Group",
        x = "Group",
        y = "Measured Value"
      ) +
      pub_theme()
  }

  make_bar_jitter <- function(df) {
    ggplot(df, aes(x = group, y = value, fill = group)) +
      stat_summary(fun = mean, geom = "bar", alpha = 0.8) +
      geom_jitter(width = 0.15, size = 2, alpha = 0.7) +
      scale_fill_manual(values = pub_colors) +
      labs(
        title = "Mean Values with Individual Observations",
        x = "Group",
        y = "Measured Value"
      ) +
      pub_theme()
  }

  make_standard_curve <- function(df) {
    # Filter standards explicitly by role
    std <- df %>%
      dplyr::filter(role == "standard") %>%
      dplyr::mutate(
        conc = as.numeric(gsub("^STD__", "", group))  # or Label if you already cleaned it
      )

    req(nrow(std) > 1)

    # --- Fit linear regression ---
    model <- lm(value ~ conc, data = std)
    coefs <- coef(model)
    r_squared <- summary(model)$r.squared

    # Build equation string: y = mx + b
    eqn <- paste0("y = ", round(coefs[2], 4), "x + ", round(coefs[1], 4))

    ggplot(std, aes(x = conc, y = value)) +
      geom_point(size = 3, color = "#4C78A8") +
      geom_smooth(method = "lm", se = TRUE, color = "#E45756") +
      labs(
        title = "Standard Curve",
        x = "Concentration",
        y = "Measured Value"
      ) +
      pub_theme() +
      # Add R^2 and equation annotation
      annotate(
        "text",
        x = min(std$conc, na.rm = TRUE),
        y = max(std$value, na.rm = TRUE),
        label = paste0("R² = ", round(r_squared, 4), "\n", eqn),
        hjust = 0,
        vjust = 1,
        size = 5,
        color = "#000000"
      )
  }

  # ---- Generate selected plots ----
  selected_plots <- reactive({
    req(state$viz_types)
    df <- analysis_data()

    plots <- list()
    fig_num <- 1

    add_plot <- function(plot, title) {
      p <- plot + labs(title = paste0("Figure ", fig_num, ": ", title))
      fig_num <<- fig_num + 1
      p
    }

    if ("Boxplot" %in% state$viz_types) {
      plots[["Boxplot"]] <- add_plot(make_boxplot(df), "Distribution of Values by Group")
    }

    if ("Bar + Jitter Chart" %in% state$viz_types) {
      plots[["Bar + Jitter"]] <- add_plot(make_bar_jitter(df), "Mean Values with Observations")
    }

    if ("Standard Curve" %in% state$viz_types) {
      sc <- try(make_standard_curve(df), silent = TRUE)
      if (!inherits(sc, "try-error")) {
        plots[["Standard Curve"]] <- add_plot(sc, "Standard Curve")
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

      # Standard US letter size
      pdf(file, width = 8.5, height = 11, onefile = TRUE)

      for (p in plots) {
        print(p)
      }

      dev.off()
    }
  )
}
