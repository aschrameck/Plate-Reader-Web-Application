#' Normalization Screen Server Logic
#'
#' Performs per-group normalization of plate data based on blanks and controls.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param state ReactiveValues object storing application state
#' @param plates Reactive object containing plate data
#'
#' @details
#' Normalization workflow:
#' 1. Blank correction: subtracts group blank mean from all wells
#' 2. Control normalization: scales wells relative to group control mean
#'
#' Users can preview normalized values and validate group assignments.
#' Reactive outputs include normalized data tables in several formats
#'
#' @return NULL; updates normalized plate values reactively
#'
#' @seealso normalize_ui
#' @family Server Screens

server_normalize <- function(input, output, session, state, plates, group_map) {

  # --- Plate selector ---
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

  # --- Reactive: Selected plate ---
  selected_plate <- reactive({
    req(state$screen == "normalize")
    req(input$normalize_plate)
    req(input$normalize_plate %in% names(plates()))
    plates()[[input$normalize_plate]]
  })

  # --- Reactive: Normalized data table ---
  normalized_data <- reactive({
    req(state$screen == "normalize")
    req(input$normalize_plate)

    gm <- group_map()
    req(nrow(gm) > 0)

    plate_name <- input$normalize_plate
    plate_groups <- gm %>% dplyr::filter(plate == plate_name)
    req(nrow(plate_groups) > 0)

    detailed <- plate_groups %>%
      dplyr::group_by(group) %>%
      dplyr::group_modify(function(df, key) {

        df$value <- as.numeric(as.character(df$value))

        # Compute means
        safe_mean <- function(x) {
          if (all(is.na(x))) return(NA_real_)
          mean(x, na.rm = TRUE)
        }

        blank_mean <- if (any(df$role == "blank")) {
          safe_mean(df$value[df$role == "blank"])
        } else 0

        control_mean <- if (any(df$role == "control")) {
          safe_mean(df$blank_corrected[df$role == "control"])
        } else NA_real_

        # Guard against invalid normalization
        if (is.na(control_mean) || control_mean == 0) {
          warning("Control mean missing or zero — using blank-corrected values.")
        }

        # --- Normalized values (skip blanks) ---
        df$normalized_value <- NA_real_
        to_normalize <- df$role != "standard" & df$role != "blank"

        if (!is.na(control_mean) && control_mean != 0) {
          # Standard case: divide by control
          df$normalized_value[to_normalize] <- df$blank_corrected[to_normalize] / control_mean
        } else {
          # No control: fall back to blank-corrected values
          df$normalized_value[to_normalize] <- df$blank_corrected[to_normalize]
        }

        df$blank_mean <- blank_mean
        df$control_mean <- control_mean

        df
      }) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(group, role)

    req(nrow(detailed) > 0)
    detailed
  })

  # --- Download detailed CSV ---
  output$download_csv <- downloadHandler(
    filename = function() paste0(input$normalize_plate, "_processed_data.csv"),
    content = function(file) {
      df <- normalized_data() %>% dplyr::filter(role != "standard")
      groups <- unique(df$group)

      group_tables <- list()
      for (grp in groups) {
        sub <- df %>% dplyr::filter(group == grp)

        normal <- sub$normalized_value[sub$role == "normal"]
        control <- sub$normalized_value[sub$role == "control"]
        blank <- sub$value[sub$role == "blank"]

        n <- max(length(normal), length(control), length(blank))

        # Build data rows
        tbl <- tibble::tibble(
          Normal = c(normal, rep("", n - length(normal))),
          Control = c(control, rep("", n - length(control))),
          Blank = c(blank, rep("", n - length(blank)))
        )

        # Extract already-computed means (constant within group)
        blank_mean_val <- unique(df$blank_mean[!is.na(df$blank_mean)])[1]
        control_mean_val <- unique(df$control_mean[!is.na(df$control_mean)])[1]

        # Add summary rows
        summary_rows <- tibble::tibble(
          Normal = c(
            "",
            paste0("Blank mean: ", round(blank_mean_val, 4)),
            paste0("Control mean: ", round(control_mean_val, 4))
          ),
          Control = "",
          Blank = ""
        )

        # Group header
        header <- tibble::tibble(
          Normal = paste0("Group: ", grp),
          Control = "",
          Blank = ""
        )

        # Column labels row
        colnames_row <- tibble::tibble(
          Normal = "Normal",
          Control = "Control",
          Blank = "Blank"
        )

        group_tables[[grp]] <- dplyr::bind_rows(
          header,
          colnames_row,
          tbl,
          summary_rows,
        )
      }

      # Combine horizontally with blank spacer columns
      final_tbl <- group_tables[[1]]
      if (length(group_tables) > 1) {
        for (i in 2:length(group_tables)) {
          spacer <- tibble::tibble(` ` = rep("", nrow(final_tbl)))
          final_tbl <- cbind(final_tbl, spacer, group_tables[[i]])
        }
      }
      colnames(final_tbl) <- NULL
      write.csv(final_tbl, file, row.names = FALSE, na = "")
    }
  )

  # --- Download Prism Compatible CSV ---
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

        # ONLY include columns that actually exist (non-empty)
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

      write.csv(
        prism_tbl,
        file,
        row.names = FALSE,
        na = ""
      )
    }
  )


  # --- Standards CSV ---
  standards_data <- reactive({
    gm <- group_map()
    req(nrow(gm) > 0)

    stds <- gm %>% dplyr::filter(role == "standard")
    if (nrow(stds) == 0) return(NULL)

    stds %>%
      dplyr::mutate(
        Label = sub("^STD__", "", group),          # remove STD__ prefix
        Label = sub("_[A-Z]$", "", Label),         # remove trailing _A, _B etc
        Unit = if ("standard_units" %in% names(stds)) standard_units else NA,
        Value = value
      ) %>%
      dplyr::select(Label, Unit, Value)
  })

  # --- Download Standards CSV ---
  output$download_standards <- downloadHandler(
    filename = function() paste0(input$normalize_plate, "_standards.csv"),
    content = function(file) {
      df <- standards_data()
      req(!is.null(df))  # safety: don't write if NULL
      write.csv(df, file, row.names = FALSE)
    }
  )
}
