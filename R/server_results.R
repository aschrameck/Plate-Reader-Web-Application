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
      dplyr::filter(role != "standard") %>%
      dplyr::distinct(plot_group, row, col, .keep_all = TRUE)

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
      dplyr::filter(role != "standard") %>%
      dplyr::distinct(plot_group, row, col, .keep_all = TRUE)

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

    # --- Plate Layout ---
    if (!is.null(state$inspect_plots[[input$active_plate]])) {
      # Base plate layout
      plots[["Plate Layout"]] <- state$inspect_plots[[input$active_plate]] +
        labs(title = paste("Plate Layout -", input$active_plate)) +
        theme(
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
        )

      if (!is.null(state$standard_legend[[input$active_plate]])) {
        plate <- plates()[[input$active_plate]]

        std_vals <- as.numeric(unlist(plate$standards))
        std_vals <- std_vals[!is.na(std_vals)]
        units <- unique(na.omit(plate$standard_units))

        if (length(std_vals) >= 2) {
          # --- Build gradient legend grob ---
          n_bins <- 100
          grad_colors <- colorRampPalette(c("#deebf7", "#08519c"))(n_bins)
          grad_df <- data.frame(
            x = seq_len(n_bins),
            y = 1,
            fill = grad_colors
          )

          grad_grob <- ggplot2::ggplot(grad_df, aes(x = x, y = y, fill = fill)) +
            geom_tile() +
            scale_fill_identity() +
            scale_x_continuous(
              limits = c(1, n_bins),
              breaks = c(1, n_bins),
              labels = round(c(min(std_vals), max(std_vals)), 3)
            ) +
            theme_void() +
            theme(
              axis.text.x = element_text(size = 10),
              plot.margin = margin(2, 2, 2, 2)
            ) +
            labs(
              title = if (length(units) > 0) paste("Standard Wells (", units[1], ")", sep="") else "Standard Wells"
            )

          # Combine plate layout and legend vertically
          plots[["Plate Layout"]] <- gridExtra::arrangeGrob(
            plots[["Plate Layout"]],
            grad_grob,
            ncol = 1,
            heights = c(3, 0.4)
          )
        }
      }
    }

    # ---- Other plots ----
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

        vp <- grid::viewport(
          x = 0.5, y = 0.5,       # center
          width = 0.8, height = 0.6,
          just = "center"
        )

        # Detect if it's an arranged grob (Plate Layout) or ggplot
        if (inherits(p, "gtable") || inherits(p, "grob")) {
          grid::pushViewport(vp)
          grid::grid.draw(p)  # correct way to render grobs
          grid::popViewport()

        } else {
          print(p, vp = viewport(
            x = 0.5, y = 0.5,        # center
            width = 0.8, height = 0.6,  # horizontal plot with top/bottom margins
            just = "center"
          ))
        }
      }

      dev.off()
    }
  )

  # ---- Statistical Analysis ----
  analysis_results <- reactive({

    # Allow empty selections
    if (is.null(state$analysis_types)) {
      state$analysis_types <- character(0)
    }

    df <- analysis_data()

    results <- list(
      ttest = NULL,
      anova = NULL,
      tukey = NULL,
      outliers = NULL
    )

    # Keep only non-standard values
    stat_df <- df %>%
      dplyr::filter(role != "standard") %>%
      dplyr::filter(!is.na(value), !is.na(plot_group)) %>%
      dplyr::distinct(plot_group, row, col, .keep_all = TRUE)

    # Ensure factor
    stat_df$plot_group <- factor(stat_df$plot_group)

    # ---- T-TESTS ----
    if ("T-test (Control vs Treatment)" %in% state$analysis_types) {
      groups <- levels(stat_df$plot_group)

      # Pairwise t-tests
      ttest_res <- pairwise.t.test(
        stat_df$value,
        stat_df$plot_group,
        p.adjust.method = "BH"
      )

      results$ttest <- ttest_res
    }

    # ---- ANOVA ----
    if ("ANOVA (Multigroup)" %in% state$analysis_types) {

      fit <- aov(value ~ plot_group, data = stat_df)
      anova_table <- summary(fit)

      results$anova <- anova_table

      # ---- Tukey Post-hoc ----
      tukey <- TukeyHSD(fit)
      results$tukey <- tukey
    }

    # ---- OUTLIER DETECTION (IQR method) ----
    if ("Outlier Detection" %in% state$analysis_types) {

      outliers <- stat_df %>%
        dplyr::group_by(plot_group) %>%
        dplyr::mutate(
          Q1 = quantile(value, 0.25, na.rm = TRUE),
          Q3 = quantile(value, 0.75, na.rm = TRUE),
          IQR = Q3 - Q1,
          lower = Q1 - 1.5 * IQR,
          upper = Q3 + 1.5 * IQR,
          is_outlier = value < lower | value > upper
        ) %>%
        dplyr::filter(is_outlier) %>%
        dplyr::ungroup()

      results$outliers <- outliers
    }

    results
  })

  # ---- Download Analysis ----
  output$download_stats <- downloadHandler(
    filename = function() {
      paste0(input$active_plate, "_analysis.pdf")
    },

    content = function(file) {

      res <- analysis_results()
      df <- analysis_data()

      req(nrow(df) > 0)

      # ---- Prepare statistical dataset ----
      stat_df <- df %>%
        dplyr::filter(role != "standard") %>%
        dplyr::filter(!is.na(value), !is.na(plot_group)) %>%
        dplyr::distinct(plot_group, row, col, .keep_all = TRUE)

      stat_df$plot_group <- factor(stat_df$plot_group)

      # ---- Helper: significance codes ----
      signif_code <- function(p) {
        dplyr::case_when(
          p < 0.001 ~ "***",
          p < 0.01  ~ "**",
          p < 0.05  ~ "*",
          p < 0.1   ~ ".",
          TRUE ~ "ns"
        )
      }

      # ---- Summary statistics ----
      summary_table <- stat_df %>%
        dplyr::group_by(plot_group) %>%
        dplyr::summarise(
          Group = dplyr::first(plot_group),
          Mean = mean(value, na.rm = TRUE),
          Median = median(value, na.rm = TRUE),
          Standard_Deviation = sd(value, na.rm = TRUE),
          Sample_Size = dplyr::n(),
          Standard_Error = Standard_Deviation / sqrt(Sample_Size),
          Minimum = min(value, na.rm = TRUE),
          Maximum = max(value, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::select(-plot_group)

      # ---- Identify control group automatically ----
      control_group <- levels(stat_df$plot_group)[grepl("Control", levels(stat_df$plot_group))][1]

      # ---- Control vs Treatment T-tests ----
      control_tests <- NULL

      if (!is.na(control_group)) {

        control_tests <- lapply(
          setdiff(levels(stat_df$plot_group), control_group),
          function(g) {

            x <- stat_df$value[stat_df$plot_group == control_group]
            y <- stat_df$value[stat_df$plot_group == g]

            if (length(x) < 2 || length(y) < 2) return(NULL)

            test <- t.test(x, y)

            data.frame(
              Comparison = paste(control_group, "vs", g),
              Mean_Control = mean(x, na.rm = TRUE),
              Mean_Treatment = mean(y, na.rm = TRUE),
              T_Statistic = unname(test$statistic),
              Degrees_of_Freedom = unname(test$parameter),
              P_Value = test$p.value,
              Significance = signif_code(test$p.value),
              Outcome = ifelse(test$p.value < 0.05,
                               "Statistically significant difference between the means of the two groups",
                               "No statistically significant difference between the means of the two groups")
            )
          }
        )

        control_tests <- dplyr::bind_rows(control_tests)
      }

      # ---- ANOVA ----
      anova_df <- NULL
      tukey_df <- NULL

      if (!is.null(res$anova)) {

        fit <- aov(value ~ plot_group, data = stat_df)
        a <- summary(fit)[[1]]

        anova_df <- data.frame(
          Term = rownames(a),
          Degrees_of_Freedom = a$Df,
          Sum_of_Squares = a$`Sum Sq`,
          Mean_Square = a$`Mean Sq`,
          F_Statistic = a$`F value`,
          P_Value = a$`Pr(>F)`
        )

        anova_df$Significance <- signif_code(anova_df$P_Value)
        rownames(anova_df) <- NULL

        # ---- Tukey ----
        if (!is.null(res$anova)) {
          tk <- TukeyHSD(fit)[[1]]

          tukey_df <- data.frame(
            Comparison = rownames(tk),
            Difference_in_Means = tk[, "diff"],
            Confidence_Interval_Lower = tk[, "lwr"],
            Confidence_Interval_Upper = tk[, "upr"],
            Adjusted_P_Value = tk[, "p adj"]
          )

          tukey_df$Significance <- signif_code(tukey_df$Adjusted_P_Value)
          rownames(tukey_df) <- NULL
        }
      }

      # ---- Outliers ----
      outlier_df <- NULL

      if (!is.null(res$outliers) && nrow(res$outliers) > 0) {

        outlier_df <- res$outliers %>%
          dplyr::mutate(
            Distance_From_Bound = dplyr::case_when(
              value < lower ~ lower - value,
              value > upper ~ value - upper,
              TRUE ~ NA_real_
            )
          ) %>%
          dplyr::select(
            Group = plot_group,
            Normalized_Value = value,
            Lower_Bound = lower,
            Upper_Bound = upper,
            Distance_From_Bound
          )
      }

      # ---- Start PDF ----
      pdf(file, width = 8.5, height = 11)

      # ---- Helper: format + wrap table ----
      format_table <- function(df, digits = 4, wrap_width = 18) {

        df[] <- lapply(df, function(col) {

          # Round numeric columns
          if (is.numeric(col)) {
            col <- round(col, digits)
          }

          # Convert to character and wrap
          col <- as.character(col)
          stringr::str_wrap(col, width = wrap_width)
        })

        # Wrap column names too
        colnames(df) <- stringr::str_wrap(colnames(df), width = wrap_width)

        df
      }

      # ---- Helper to print clean tables ----
      print_table <- function(title, table_df) {
        grid::grid.newpage()

        # ---- Zebra striping ----
        n_rows <- nrow(table_df)

        core_bg <- rep(c("grey95", "white"), length.out = n_rows)

        table_theme <- gridExtra::ttheme_minimal(
          base_size = 9,  # slightly smaller to prevent overflow
          core = list(
            fg_params = list(hjust = 0.5, x = 0.5, fontsize = 8),
            bg_params = list(fill = core_bg, col = NA)
          ),
          colhead = list(
            fg_params = list(fontface = "bold", hjust = 0.5, fontsize = 9),
            bg_params = list(fill = "grey85")
          )
        )

        # ---- Format + wrap before rendering ----
        table_df <- format_table(table_df)

        tbl <- gridExtra::tableGrob(
          table_df,
          rows = NULL,
          theme = table_theme
        )

        # ---- Let columns size naturally ----
        tbl$widths <- grid::unit.pmax(tbl$widths, grid::unit(1.5, "cm"))

        # ---- If table is too wide, shrink proportionally ----
        total_width <- sum(tbl$widths)
        max_width <- grid::unit(0.95, "npc")

        if (grid::convertWidth(total_width, "npc", valueOnly = TRUE) > 0.95) {
          scale_factor <- 0.95 / grid::convertWidth(total_width, "npc", valueOnly = TRUE)
          tbl$widths <- tbl$widths * scale_factor
        }

        # ---- Final width clamp (prevents overflow for wide tables) ----
        if (grid::convertWidth(sum(tbl$widths), "npc", valueOnly = TRUE) > 0.95) {
          tbl$widths <- tbl$widths * 0.95 /
            grid::convertWidth(sum(tbl$widths), "npc", valueOnly = TRUE)
        }

        # ---- Create title grob ----
        title_grob <- grid::textGrob(
          title,
          gp = grid::gpar(fontsize = 14, fontface = "bold")
        )

        # ---- Combine title + table ----
        combined <- gridExtra::arrangeGrob(
          title_grob,
          tbl,
          ncol = 1,
          heights = c(0.04, 0.96)
        )

        # ---- Draw flush to top ----
        grid::pushViewport(
          grid::viewport(
            x = 0.5,
            y = 0.99,
            just = c("center", "top"),
            width = grid::unit(0.95, "npc"),
            height = grid::unit(0.98, "npc")
          )
        )

        grid::grid.draw(combined)
        grid::popViewport()
      }

      # ---- Render tables ----
      print_table("Summary Statistics", summary_table)

      if ("T-test (Control vs Treatment)" %in% state$analysis_types &&
          !is.null(control_tests) && nrow(control_tests) > 0) {

        print_table("Control Versus Treatment T-Tests", control_tests)
      }

      if (!is.null(res$ttest)) {
        ttest_df <- as.data.frame(res$ttest$p.value)
        ttest_df$Group <- rownames(ttest_df)
        rownames(ttest_df) <- NULL
        print_table("Pairwise T-Test Adjusted P-Values", ttest_df)
      }

      if (!is.null(anova_df)) {
        print_table("Analysis of Variance (ANOVA)", anova_df)
      }

      if (!is.null(tukey_df)) {
        print_table("Tukey Post Hoc Test", tukey_df)
      }

      if ("Outlier Detection" %in% state$analysis_types) {

        if (is.null(outlier_df) || nrow(outlier_df) == 0) {
          outlier_df <- data.frame(
            Message = "No outliers detected using the IQR method"
          )
        }

        print_table("Detected Outliers", outlier_df)
      }

      dev.off()
    }
  )
  # --- Automated Report Generation ---
  output$download_teaching_pdf <- downloadHandler(
    filename = function() {
      paste0(input$active_plate, "_report.pdf")
    },

    content = function(file) {

      df_raw <- normalized_data() %>%
        dplyr::filter(plate == input$active_plate)

      df_analysis <- analysis_data()
      plots <- selected_plots()
      res <- analysis_results()

      req(nrow(df_raw) > 0)

      # =========================================================
      # FLAGS
      # =========================================================
      has_boxplot  <- "Boxplot" %in% state$viz_types
      has_bar      <- "Bar + Jitter Chart" %in% state$viz_types
      has_sc       <- "Standard Curve" %in% state$viz_types

      has_ttest    <- "T-test (Control vs Treatment)" %in% state$analysis_types
      has_anova    <- "ANOVA (Multigroup)" %in% state$analysis_types
      has_outliers <- "Outlier Detection" %in% state$analysis_types

      # =========================================================
      # DATA PREP
      # =========================================================
      stat_df <- df_analysis %>%
        dplyr::filter(role != "standard") %>%
        dplyr::filter(!is.na(value), !is.na(plot_group)) %>%
        dplyr::distinct(plot_group, row, col, .keep_all = TRUE)

      stat_df$plot_group <- factor(stat_df$plot_group)

      blank_mean <- mean(df_raw$value[df_raw$role == "blank"], na.rm = TRUE)
      control_mean <- mean(df_raw$value[df_raw$role == "control"], na.rm = TRUE)

      # =========================================================
      # PAGE + FIGURE ENV
      # =========================================================
      page_env <- new.env(); page_env$page <- 0
      fig_env <- new.env(); fig_env$i <- 0

      next_fig <- function() {
        fig_env$i <- fig_env$i + 1
        paste0("Figure ", fig_env$i)
      }

      # =========================================================
      # HEADER (FIXED 12pt APA RUNNING HEAD)
      # =========================================================
      draw_header <- function() {
        page_env$page <- page_env$page + 1

        if (page_env$page > 1) {

          grid::grid.text(
            "PLATE ANALYSIS REPORT",
            x = 0.02, y = 0.985,
            just = "left",
            gp = grid::gpar(
              fontsize = 12,   # FIXED APA RUNNING HEAD SIZE
              fontfamily = "serif"
            )
          )

          grid::grid.text(
            page_env$page - 1,
            x = 0.98, y = 0.985,
            just = "right",
            gp = grid::gpar(fontsize = 10, fontfamily = "serif")
          )
        }
      }

      # =========================================================
      # TEXT PAGE (CENTERED CONSISTENCY FIX)
      # =========================================================
      draw_text_page <- function(title, text_lines) {

        grid::grid.newpage()
        draw_header()

        body <- paste(
          stringr::str_wrap(paste(text_lines, collapse = " "), 95),
          collapse = "\n"
        )

        gridExtra::grid.arrange(
          grid::textGrob(
            title,
            gp = grid::gpar(fontsize = 16, fontface = "bold", fontfamily = "serif")
          ),
          grid::textGrob(
            body,
            x = 0.5, y = 0.5,
            just = "center",
            gp = grid::gpar(
              fontsize = 11,
              lineheight = 1.6,
              fontfamily = "serif"
            )
          ),
          ncol = 1,
          heights = c(0.1, 0.9)
        )
      }

      # =========================================================
      # TABLE PAGE (CENTER FIX)
      # =========================================================
      draw_table_page <- function(title, df) {

        grid::grid.newpage()
        draw_header()

        if (is.null(df) || nrow(df) == 0) {
          df <- data.frame(Message = "No data available")
        }

        zebra <- rep(c("grey95", "white"), length.out = nrow(df))

        tbl <- gridExtra::tableGrob(
          df,
          rows = NULL,
          theme = gridExtra::ttheme_minimal(
            base_size = 9,
            core = list(bg_params = list(fill = zebra, col = NA)),
            colhead = list(fg_params = list(fontface = "bold"))
          )
        )

        gridExtra::grid.arrange(
          grid::textGrob(
            title,
            gp = grid::gpar(fontsize = 13, fontface = "bold", fontfamily = "serif")
          ),
          tbl,
          ncol = 1,
          heights = c(0.07, 0.93)
        )
      }

      # =========================================================
      # PLOT PAGE (FIXED VIEWPORT AS REQUESTED)
      # =========================================================
      draw_plot_page <- function(plot_obj, title, caption_text) {

        grid::grid.newpage()
        draw_header()

        vp <- grid::viewport(
          x = 0.5, y = 0.5,
          width = 0.8, height = 0.6,
          just = "center"
        )

        plot_grob <- if (inherits(plot_obj, "grob") || inherits(plot_obj, "gtable")) {
          plot_obj
        } else ggplot2::ggplotGrob(plot_obj)

        caption <- paste0(next_fig(), ". ", stringr::str_wrap(caption_text, 110))

        gridExtra::grid.arrange(
          grid::textGrob(
            title,
            gp = grid::gpar(fontsize = 14, fontface = "bold", fontfamily = "serif")
          ),
          plot_grob,
          grid::textGrob(
            caption,
            gp = grid::gpar(fontsize = 9, fontfamily = "serif")
          ),
          ncol = 1,
          heights = c(0.08, 0.84, 0.08),
          vp = vp
        )
      }

      # =========================================================
      # TABLES
      # =========================================================
      summary_table <- stat_df %>%
        dplyr::group_by(plot_group) %>%
        dplyr::summarise(
          Group = dplyr::first(plot_group),
          Mean = mean(value),
          SD = sd(value),
          n = dplyr::n(),
          SE = SD / sqrt(n),
          .groups = "drop"
        )

      normalized_table <- stat_df %>%
        dplyr::transmute(
          Row = row,
          Column = col,
          Group = plot_group,
          Role = role,
          Raw_Value = value,
          Normalized_Value = value
        )

      # =========================================================
      # PDF START
      # =========================================================
      pdf(file, width = 8.5, height = 11, onefile = TRUE)

      # =========================================================
      # TITLE PAGE (UNCHANGED)
      # =========================================================
      grid::grid.newpage()

      grid::grid.text(
        "Plate Analysis Report",
        x = 0.5, y = 0.6,
        gp = grid::gpar(fontsize = 22, fontface = "bold", fontfamily = "serif")
      )

      grid::grid.text(
        paste("Plate ID:", input$active_plate,
              "\nGenerated:", format(Sys.time(), "%Y-%m-%d %H:%M")),
        x = 0.5, y = 0.45,
        gp = grid::gpar(fontsize = 12, fontfamily = "serif")
      )

      # =========================================================
      # TABLE OF CONTENTS
      # =========================================================
      draw_text_page("Table of Contents",
                     c(
                       "1. Methods",
                       "2. Plate Layout",
                       "3. Normalized Dataset",
                       "4. Summary Statistics",
                       "5. Visualizations",
                       "6. Statistical Tests",
                       "7. Standard Curve & Values",
                       "8. Discussion",
                       "9. References"
                     ))

      # =========================================================
      # METHODS
      # =========================================================
      draw_text_page("Methods",
                     c(
                       paste0("Blank mean = ", round(blank_mean, 4),
                              ", Control mean = ", round(control_mean, 4), "."),
                       "Normalization: (Raw Value - Blank Mean) / (Control Mean - Blank Mean).",
                       if (has_ttest) "Welch t-tests were used for control vs treatment comparisons.",
                       if (has_ttest) "Pairwise comparisons used BH-adjusted p-values.",
                       if (has_anova) "One-way ANOVA with Tukey post hoc tests was performed.",
                       if (has_outliers) "Outliers were identified using the 1.5 × IQR rule."
                     ))

      # =========================================================
      # PLATE LAYOUT
      # =========================================================
      if (!is.null(plots[["Plate Layout"]])) {
        draw_plot_page(
          plots[["Plate Layout"]],
          "Plate Layout",
          "Experimental plate layout showing controls, standards, and treatment wells."
        )
      }

      # =========================================================
      # NORMALIZED DATA
      # =========================================================
      draw_table_page("Normalized Dataset (Subset)", head(normalized_table, 25))

      # =========================================================
      # SUMMARY
      # =========================================================
      draw_table_page("Summary Statistics", summary_table)

      # =========================================================
      # VISUALIZATIONS
      # =========================================================
      if (has_boxplot && !is.null(plots[["Boxplot"]])) {
        draw_plot_page(
          plots[["Boxplot"]],
          "Boxplot of Group Distributions",
          "Distribution of normalized values across groups."
        )
      }

      if (has_bar && !is.null(plots[["Bar + Jitter"]])) {
        draw_plot_page(
          plots[["Bar + Jitter"]],
          "Bar + Jitter Plot",
          "Mean values with individual replicate measurements."
        )
      }

      # =========================================================
      # TESTS (UNCHANGED LOGIC, FORMATTED OUTPUTS)
      # =========================================================
      if (has_ttest && !is.null(res$ttest)) {

        ctrl_df <- as.data.frame(res$ttest$p.value)
        ctrl_df$Comparison <- rownames(ctrl_df)
        rownames(ctrl_df) <- NULL

        draw_table_page("Control vs Treatment T-Tests", ctrl_df)
      }

      if (!is.null(res$ttest)) {

        pair_df <- as.data.frame(res$ttest$p.value)
        pair_df$Group <- rownames(pair_df)
        rownames(pair_df) <- NULL

        draw_table_page("Pairwise T-Test Adjusted P-Values", pair_df)
      }

      if (has_anova && !is.null(res$anova)) {

        fit <- aov(value ~ plot_group, data = stat_df)
        a <- summary(fit)[[1]]

        anova_df <- data.frame(
          Term = rownames(a),
          DF = a$Df,
          Sum_Sq = a$`Sum Sq`,
          Mean_Sq = a$`Mean Sq`,
          F_value = a$`F value`,
          P_value = a$`Pr(>F)`
        )

        draw_table_page("One-Way ANOVA", anova_df)
      }

      if (has_outliers) {

        out_df <- stat_df %>%
          dplyr::group_by(plot_group) %>%
          dplyr::mutate(
            Q1 = quantile(value, 0.25),
            Q3 = quantile(value, 0.75),
            IQR = Q3 - Q1,
            Lower = Q1 - 1.5 * IQR,
            Upper = Q3 + 1.5 * IQR,
            Outlier = value < Lower | value > Upper
          ) %>%
          dplyr::filter(Outlier) %>%
          dplyr::select(Group = plot_group, Value = value, Lower, Upper)

        if (nrow(out_df) == 0) {
          out_df <- data.frame(Message = "No outliers detected")
        }

        draw_table_page("Outlier Detection", out_df)
      }

      # =========================================================
      # STANDARD CURVE SECTION
      # =========================================================
      if (has_sc && !is.null(plots[["Standard Curve"]])) {

        draw_plot_page(
          plots[["Standard Curve"]],
          "Standard Curve",
          "Calibration curve with regression fit for standard concentration estimation."
        )
      }

      # =========================================================
      # DISCUSSION
      # =========================================================
      draw_text_page("Discussion",
                     c(
                       "Results integrate statistical inference and visualization for interpretation of experimental effects.",
                       "All outputs are derived from a single normalized dataset ensuring internal consistency."
                     ))

      # =========================================================
      # REFERENCES
      # =========================================================
      draw_text_page("References",
                     c(
                       "R Core Team (2025). R: Statistical Computing Environment.",
                       "Posit Team (2025). Shiny: Web Application Framework for R.",
                       "Schrameck, A. (2026). Plate analysis pipeline development."
                     ))

      dev.off()
    }
  )
}
