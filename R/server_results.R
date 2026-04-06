#' Results Screen Server Logic
#'
#' Displays and exports final analysis results.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param state ReactiveValues object storing application state
#' @param plates Reactive object containing normalized/analysis-ready plate data
#' @param normalized_data Processed values
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

server_results <- function(input, output, session, state, plates, normalized_data) {

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
    req(normalized_data)

    df <- normalized_data() %>%
      dplyr::filter(
        plate == input$active_plate,
        role %in% c("normal", "control", "standard")
      )

    # ---- STEP 1: Build control labels based on shared row/col positions ----
    control_labels <- df %>%
      dplyr::filter(role == "control") %>%
      dplyr::group_by(row, col) %>%  # group by physical position
      dplyr::summarise(
        groups = paste(sort(unique(group)), collapse = ", "),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        control_label = paste0("Control (", groups, ")")
      )

    # ---- STEP 2: Join back to main df ----
    df <- df %>%
      dplyr::left_join(control_labels, by = c("row", "col"))

    # ---- STEP 3: Assign plot_group ----
    df <- df %>%
      dplyr::mutate(
        value = dplyr::case_when(
          role == "standard" ~ as.numeric(value),
          role %in% c("normal", "control") ~ as.numeric(normalized_value),
          TRUE ~ NA_real_
        ),

        plot_group = dplyr::case_when(
          role == "control" ~ control_label,  # grouped by row/col now
          role == "normal"  ~ as.character(group),
          TRUE ~ as.character(group)
        ),

        plot_group = factor(plot_group)
      )

    req(nrow(df) > 0)
    df
  })

  # --- Plot formatting ---
  pub_theme <- function() {
    theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),

        legend.position = "bottom",   # moved from "right"
        legend.title = element_blank()
      )
  }

  pub_colors <- c("#4C78A8", "#F58518", "#54A24B", "#E45756", "#72B7B2")

  # ---- Plot generators ----
  make_boxplot <- function(df) {
    box_df <- df %>%
      dplyr::filter(role != "standard")  # keep controls

    ggplot(box_df, aes(x = plot_group, y = value, fill = plot_group)) +
      geom_boxplot(alpha = 0.8, outlier.shape = 16, outlier.size = 2) +
      scale_fill_manual(values = pub_colors) +
      labs(
        title = "Distribution of Normalized Values by Group",
        x = "Group",
        y = "Normalized Value"
      ) +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
      pub_theme() +
      coord_cartesian(ylim = c(min(box_df$value, na.rm = TRUE) * 0.9,
                               max(box_df$value, na.rm = TRUE) * 1.1))
  }

  make_bar_jitter <- function(df) {
    bar_df <- df %>%
      dplyr::filter(role != "standard")  # keep controls

    ggplot(bar_df, aes(x = plot_group, y = value, fill = plot_group)) +
      stat_summary(fun = mean, geom = "bar", alpha = 0.8, color = "black", width = 0.6) +
      geom_jitter(width = 0.15, size = 2, alpha = 0.7) +
      scale_fill_manual(values = pub_colors) +
      labs(
        title = "Normalized Values by Group with Individual Observations",
        x = "Group",
        y = "Normalized Value"
      ) +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
      pub_theme() +
      coord_cartesian(ylim = c(min(bar_df$value, na.rm = TRUE) * 0.9,
                               max(bar_df$value, na.rm = TRUE) * 1.2))
  }

  make_standard_curve <- function(df) {
    std <- df %>%
      dplyr::filter(role == "standard") %>%
      dplyr::mutate(
        conc = as.numeric(gsub("^STD__", "", plot_group))
      )

    req(nrow(std) > 1)
    model <- lm(value ~ conc, data = std)
    coefs <- coef(model)
    r_squared <- summary(model)$r.squared
    eqn <- paste0("y = ", round(coefs[2], 4), "x + ", round(coefs[1], 4))

    ggplot(std, aes(x = conc, y = value)) +
      geom_point(size = 3, color = "#4C78A8") +
      geom_smooth(method = "lm", se = TRUE, color = "#E45756") +
      labs(
        title = "Standard Curve for Plate",
        x = "Concentration",
        y = "Measured Value"
      ) +
      pub_theme() +
      annotate(
        "label",
        x = Inf,
        y = Inf,
        label = paste(
          stringr::str_wrap(paste0("R² = ", round(r_squared, 4)), width = 25),
          stringr::str_wrap(eqn, width = 25),
          sep = "\n"
        ),
        hjust = 1.05,
        vjust = 1.05,
        size = 4,  # slightly smaller text to prevent overflow
        fill = "white",
        color = "black",
        label.padding = unit(0.25, "lines")  # adds space inside box
      )
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

      pdf(file, width = 8.5, height = 11)  # PDF portrait

      for (p in plots) {
        grid.newpage()
        print(p, vp = viewport(
          x = 0.5, y = 0.5,        # center
          width = 0.8, height = 0.6,  # horizontal plot with top/bottom margins
          just = "center"
        ))
      }

      dev.off()
    }
  )
}
