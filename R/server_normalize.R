#' Normalization Screen Server Logic
#'
#' Handles plate-level data normalization using blank correction and control scaling.
#' Provides downloadable outputs in both analysis-friendly and external tool formats.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param state ReactiveValues object storing global application state
#' @param plates Reactive expression returning named list of plate data frames
#' @param group_map Data frame mapping wells to experimental groups and roles
#'
#' @details
#' This module implements a two-stage normalization pipeline:
#'
#' **1. Blank correction**
#' - Subtracts mean blank signal within each group
#' - Applied to all wells in the group
#'
#' **2. Control normalization**
#' - Uses group control mean (after blank correction) as scaling factor
#' - If no control exists, falls back to blank-corrected values
#'
#' **Supported roles**
#' - normal: experimental wells
#' - control: normalization reference wells
#' - blank: background signal wells
#' - standard: calibration curve wells (excluded from normalization)
#'
#' **Outputs**
#' - Normalized reactive dataset (`normalized_data`)
#' - Multi-group structured CSV export
#' - Prism-compatible wide-format CSV export
#' - Standards regression table with linear fit
#'
#' @return Reactive expression of normalized data table
#'
#' @seealso normalize_ui
#' @family Server Screens

server_normalize <- function(input, output, session, state, plates, group_map) {

  # --- Plate selector initialization ---
  observe({
    req(state$screen == "normalize")
    req(length(plates()) > 0)

    updateSelectInput(
      session,
      "normalize_plate",
      choices = names(plates()),
      selected = names(plates())[1]
    )
  })

  # --- Reactive selected plate object ---
  selected_plate <- reactive({
    req(state$screen == "normalize")
    req(input$normalize_plate)
    req(input$normalize_plate %in% names(plates()))

    plates()[[input$normalize_plate]]
  })

  # --- Core normalization pipeline ---
  normalized_data <- reactive({

    gm <- group_map()
    req(nrow(gm) > 0)

    plate_name <- input$normalize_plate

    # Filter mapping to current plate only
    plate_groups <- gm %>%
      dplyr::filter(plate == plate_name)

    req(nrow(plate_groups) > 0)

    detailed <- plate_groups %>%
      dplyr::group_by(group) %>%
      dplyr::group_modify(function(df, key) {

        # Ensure numeric values
        df$value <- as.numeric(as.character(df$value))

        # Safe mean helper (handles all-NA groups)
        safe_mean <- function(x) {
          if (all(is.na(x))) return(NA_real_)
          mean(x, na.rm = TRUE)
        }

        # --- Blank correction baseline ---
        blank_mean <- if (any(df$role == "blank")) {
          safe_mean(df$value[df$role == "blank"])
        } else {
          0
        }

        # Apply blank correction to all values
        df$blank_corrected <- df$value - blank_mean

        # --- Control mean after blank correction ---
        control_mean <- if (any(df$role == "control")) {
          safe_mean(df$blank_corrected[df$role == "control"])
        } else {
          NA_real_
        }

        # --- Normalization step ---
        df$normalized_value <- NA_real_

        # Only normalize non-standards and non-blanks
        to_normalize <- df$role != "standard" & df$role != "blank"

        if (!is.na(control_mean) && control_mean != 0) {

          # Standard normalization: divide by control mean
          df$normalized_value[to_normalize] <-
            df$blank_corrected[to_normalize] / control_mean

        } else {

          # Fallback: use blank-corrected values directly
          df$normalized_value[to_normalize] <-
            df$blank_corrected[to_normalize]
        }

        # Store group-level summary values for reporting
        df$blank_mean <- blank_mean
        df$control_mean <- control_mean

        df
      }) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(group, role)

    req(nrow(detailed) > 0)
    detailed
  })

  # --- Download: detailed grouped CSV ---
  output$download_csv <- downloadHandler(
    filename = function() paste0(input$normalize_plate, "_processed_data.csv"),
    content = function(file) {

      tryCatch({

        df <- normalized_data() %>%
          dplyr::filter(role != "standard")

        req(nrow(df) > 0)

        groups <- unique(df$group)
        group_tables <- list()

        safe_val <- function(x) {
          if (is.null(x) || all(is.na(x))) return("")
          round(x[1], 4)
        }

        for (grp in groups) {

          sub <- df %>% dplyr::filter(group == grp)

          normal  <- sub$normalized_value[sub$role == "normal"]
          control <- sub$normalized_value[sub$role == "control"]
          blank   <- sub$value[sub$role == "blank"]

          n <- max(length(normal), length(control), length(blank), 1)

          tbl <- tibble::tibble(
            Normal  = c(normal,  rep("", n - length(normal))),
            Control = c(control, rep("", n - length(control))),
            Blank   = c(blank,   rep("", n - length(blank)))
          )

          blank_mean_val   <- safe_val(unique(sub$blank_mean))
          control_mean_val <- safe_val(unique(sub$control_mean))

          summary_rows <- tibble::tibble(
            Normal = c(
              "",
              paste0("Blank mean: ", blank_mean_val),
              paste0("Control mean: ", control_mean_val)
            ),
            Control = "",
            Blank = ""
          )

          header <- tibble::tibble(
            Normal = paste0("Group: ", grp),
            Control = "",
            Blank = ""
          )

          colnames_row <- tibble::tibble(
            Normal = "Normal",
            Control = "Control",
            Blank = "Blank"
          )

          group_tables[[grp]] <- dplyr::bind_rows(
            header,
            colnames_row,
            tbl,
            summary_rows
          )
        }

        # --- FIX: equalize row counts before cbind ---
        max_rows <- max(sapply(group_tables, nrow))

        pad_rows <- function(df, n) {
          if (nrow(df) < n) {
            df[(nrow(df) + 1):n, ] <- ""
          }
          df
        }

        group_tables <- lapply(group_tables, pad_rows, n = max_rows)

        final_tbl <- group_tables[[1]]

        if (length(group_tables) > 1) {
          for (i in 2:length(group_tables)) {
            spacer <- tibble::tibble(` ` = rep("", max_rows))
            final_tbl <- cbind(final_tbl, spacer, group_tables[[i]])
          }
        }

        colnames(final_tbl) <- NULL
        write.csv(final_tbl, file, row.names = FALSE, na = "")

      }, error = function(e) {
        message("download_csv error: ", e$message)
        stop("Download failed. Check server logs for details.")
      })
    }
  )

  # --- Download: Prism-compatible format ---
  output$download_prism <- downloadHandler(
    filename = function() paste0(input$normalize_plate, "_prism.csv"),
    content = function(file) {

      df <- normalized_data() %>%
        dplyr::filter(role %in% c("normal", "control", "blank"))

      req(nrow(df) > 0)

      groups <- unique(df$group)
      prism_list <- list()

      for (grp in groups) {

        sub <- df %>% dplyr::filter(group == grp)

        # Helper for extracting role-specific vectors
        make_vec <- function(role_name, value_col) {
          vals <- sub[[value_col]][sub$role == role_name]
          vals <- as.numeric(vals[!is.na(vals)])
          vals
        }

        normal  <- make_vec("normal", "normalized_value")
        control <- make_vec("control", "normalized_value")
        blank   <- make_vec("blank", "value")

        max_len <- max(length(normal), length(control), length(blank), 1)

        pad <- function(x) {
          length(x) <- max_len
          x
        }

        # Only include non-empty columns
        if (length(normal) > 0) {
          prism_list[[paste0(grp, "_Normal")]] <- pad(normal)
        }
        if (length(control) > 0) {
          prism_list[[paste0(grp, "_Control")]] <- pad(control)
        }
        if (length(blank) > 0) {
          prism_list[[paste0(grp, "_Blank")]] <- pad(blank)
        }
      }

      prism_tbl <- as.data.frame(prism_list, check.names = FALSE)

      write.csv(prism_tbl, file, row.names = FALSE, na = "")
    }
  )

  # --- Reactive: standards regression table ---
  standards_data <- reactive({

    gm <- group_map()
    req(nrow(gm) > 0)

    stds <- gm %>% dplyr::filter(role == "standard")
    if (nrow(stds) == 0) return(NULL)

    df <- stds %>%
      dplyr::mutate(
        Label = sub("^STD__", "", group),
        Label = sub("_[A-Z]$", "", Label),
        Label = as.numeric(Label),
        Unit  = if ("standard_units" %in% names(stds)) standard_units else "",
        Value = as.numeric(value)
      ) %>%
      dplyr::select(Label, Unit, Value)

    # Fit linear calibration curve if sufficient data exists
    if (nrow(df) >= 2 &&
        all(!is.na(df$Label)) &&
        all(!is.na(df$Value))) {

      model <- lm(Value ~ Label, data = df)

      df$Predicted <- predict(model, newdata = df)

      r_squared <- summary(model)$r.squared
      coefs <- coef(model)

      eqn <- paste0(
        "y = ", round(coefs[2], 4),
        "x + ", round(coefs[1], 4)
      )

    } else {
      df$Predicted <- NA_real_
      r_squared <- NA_real_
      eqn <- NA_character_
    }

    df_char <- df %>% dplyr::mutate(across(everything(), as.character))

    blank_row <- tibble::tibble(
      Label = "", Unit = "", Value = "", Predicted = ""
    )

    summary_rows <- tibble::tibble(
      Label = c("R^2:", "Equation:"),
      Unit = c(round(r_squared, 4), eqn),
      Value = c("", ""),
      Predicted = c("", "")
    )

    dplyr::bind_rows(df_char, blank_row, summary_rows)
  })

  # --- Download standards CSV ---
  output$download_standards <- downloadHandler(
    filename = function() paste0(input$normalize_plate, "_standards.csv"),
    content = function(file) {
      df <- standards_data()
      req(!is.null(df))
      write.csv(df, file, row.names = FALSE)
    }
  )

  # Return normalized dataset for downstream modules
  return(normalized_data)
}
