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

  # ---- Plate selector (FIXED) ----
  observe({
    req(state$screen == "results")
    req(length(plates()) > 0)

    updateSelectInput(
      session,
      "analysis_plate",
      choices = names(plates()),
      selected = input$analysis_plate
    )
  })

  # --- Normalize data ---
  analysis_data <- reactive({
    req(state$screen == "results")
    req(input$analysis_plate)

    gm <- group_map()
    req(nrow(gm) > 0)

    df <- gm %>%
      dplyr::filter(plate == input$analysis_plate)

    req(nrow(df) > 0)

    df$value <- as.numeric(as.character(df$value))

    # ---- blank correction ----
    blank_mean <- if (any(df$role == "blank")) {
      mean(df$value[df$role == "blank"], na.rm = TRUE)
    } else 0

    df$blank_corrected <- df$value - blank_mean

    # ---- control mean ----
    control_mean <- if (any(df$role == "control")) {
      mean(df$blank_corrected[df$role == "control"], na.rm = TRUE)
    } else NA_real_

    # ---- normalization ----
    df$norm <- NA_real_
    target <- df$role != "blank" & df$role != "standard"

    if (!is.na(control_mean) && control_mean != 0) {
      df$norm[target] <- df$blank_corrected[target] / control_mean
    } else {
      df$norm[target] <- df$blank_corrected[target]
    }

    df
  })

  # --- Statistical Tests (FIXED CLD EXTRACTION) ---
  stats_results <- reactive({
    req(input$analysis_types)
    req("ANOVA" %in% input$analysis_types ||
          "Tukey’s Post-Hoc Test" %in% input$analysis_types)

    df <- analysis_data()
    req(nrow(df) > 0)

    fit <- aov(norm ~ group, data = df)

    tuk <- TukeyHSD(fit)

    cld <- tryCatch({
      letters <- multcompView::multcompLetters4(fit, tuk)
      data.frame(
        group = names(letters$group$Letters),
        letters = letters$group$Letters
      )
    }, error = function(e) {
      data.frame(group = unique(df$group), letters = "")
    })

    list(
      fit = fit,
      anova = summary(fit),
      tukey = tuk,
      cld = cld
    )
  })

  # --- Boxplot ---
  output$results_boxplot <- renderPlot({
    req("Boxplot" %in% input$viz_types)

    df <- analysis_data()

    ggplot(df, aes(group, norm, fill = group)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.15) +
      ggpubr::stat_compare_means(method = "anova") +
      ggpubr::stat_compare_means(label = "p.signif", method = "t.test") +
      theme_minimal()
  })

  # --- Bar + Jitter ---
  output$results_bar <- renderPlot({
    req("Bar + Jitter Chart" %in% input$viz_types)

    df <- analysis_data()
    cld <- stats_results()$cld

    summary_df <- df %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(
        mean = mean(norm, na.rm = TRUE),
        sd = sd(norm, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::left_join(cld, by = "group")

    ggplot(summary_df, aes(group, mean, fill = group)) +
      geom_col() +
      geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
      geom_text(aes(label = letters, y = mean + sd + 0.1)) +
      geom_jitter(data = df, aes(group, norm),
                  width = 0.15, inherit.aes = FALSE) +
      theme_minimal()
  })

  # --- Standard Curve (FIXED plate reference) ---
  output$results_standard_curve <- renderPlot({
    req("Standard Curve" %in% input$viz_types)

    gm <- group_map()

    stds <- gm %>%
      dplyr::filter(plate == input$analysis_plate, role == "standard") %>%
      dplyr::mutate(conc = as.numeric(sub("STD__", "", group)),
                    value = as.numeric(value))

    req(nrow(stds) > 1)

    fit <- lm(value ~ conc, data = stds)
    r2 <- summary(fit)$r.squared
    eq <- paste0("y = ", round(coef(fit)[2], 3),
                 "x + ", round(coef(fit)[1], 3))

    ggplot(stds, aes(conc, value)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      annotate("text", x = Inf, y = Inf,
               label = paste(eq, "\nR² =", round(r2, 3)),
               hjust = 1.1, vjust = 1.5) +
      theme_minimal()
  })

  # --- Stats Download (FIXED PRINTING) ---
  output$download_stats <- downloadHandler(
    filename = function() paste0(input$analysis_plate, "_stats.pdf"),
    content = function(file) {

      pdf(file)

      res <- stats_results()

      if ("ANOVA" %in% input$analysis_types) {
        print(res$anova)
      }

      if ("Tukey’s Post-Hoc Test" %in% input$analysis_types) {
        print(res$tukey)
      }

      if ("F Test" %in% input$analysis_types) {
        print(ftest())
      }

      if ("Outlier Identification" %in% input$analysis_types) {
        print(outliers())
      }

      dev.off()
    }
  )

  # --- Visualization Download ---
  output$download_visualizations <- downloadHandler(
    filename = function() paste0(input$analysis_plate, "_visualizations.pdf"),
    content = function(file) {

      pdf(file)

      df <- analysis_data()

      if ("Boxplot" %in% input$viz_types) {
        print(
          ggplot(df, aes(group, norm, fill = group)) +
            geom_boxplot() +
            geom_jitter(width = 0.15) +
            theme_minimal()
        )
      }

      if ("Bar + Jitter Chart" %in% input$viz_types) {

        cld <- stats_results()$cld

        summary_df <- df %>%
          dplyr::group_by(group) %>%
          dplyr::summarise(
            mean = mean(norm, na.rm = TRUE),
            sd = sd(norm, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::left_join(cld, by = "group")

        print(
          ggplot(summary_df, aes(group, mean, fill = group)) +
            geom_col() +
            geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
            geom_text(aes(label = letters, y = mean + sd)) +
            theme_minimal()
        )
      }

      if ("Standard Curve" %in% input$viz_types) {

        stds <- df %>% dplyr::filter(role == "standard")

        if (nrow(stds) > 1) {
          print(
            ggplot(stds, aes(as.numeric(group), value)) +
              geom_point() +
              geom_smooth(method = "lm", se = FALSE) +
              theme_minimal()
          )
        }
      }

      dev.off()
    }
  )

  # --- Report Generator (unchanged core, safer stats usage) ---
  generate_report <- function(file, df, stats, ftest_res, outliers_res, plate_name) {

    pdf(file, width = 8.5, height = 11)

    plot.new()
    title(main = "Experimental Analysis Report")

    mtext(paste("Plate:", plate_name), side = 3)
    mtext(paste("Generated:", Sys.Date()), side = 1)

    plot.new()
    title("Methods")

    text(0.05, 0.9,
         paste(
           "Blank correction + control normalization",
           "ANOVA + Tukey HSD + optional tests",
           sep = "\n"
         ),
         adj = 0)

    grid::grid.newpage()
    grid::grid.table(head(df, 50))

    plot.new()
    title("Statistical Results")

    if (!is.null(stats)) print(stats$anova)
    if (!is.null(stats)) print(stats$tukey)
    if (!is.null(ftest_res)) print(ftest_res)
    if (!is.null(outliers_res)) print(outliers_res)

    print(
      ggplot(df, aes(group, norm, fill = group)) +
        geom_boxplot() +
        geom_jitter(width = 0.15) +
        theme_minimal()
    )

    dev.off()
  }

  # --- Report Download ---
  output$download_teaching_pdf <- downloadHandler(
    filename = function() paste0(input$analysis_plate, "_publication_report.pdf"),
    content = function(file) {

      df <- analysis_data()

      generate_report(
        file = file,
        df = df,
        stats = stats_results(),
        ftest_res = ftest(),
        outliers_res = outliers(),
        plate_name = input$analysis_plate
      )
    }
  )
}
