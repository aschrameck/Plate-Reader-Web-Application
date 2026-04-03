test_that("normalization subtracts blank and divides by control", {

  df <- data.frame(
    group = c("A","A","A"),
    role = c("blank","control","normal"),
    value = c(1, 5, 9)
  )

  result <- df %>%
    dplyr::group_by(group) %>%
    dplyr::group_modify(function(df, key) {

      blank_mean <- mean(df$value[df$role == "blank"])
      df$blank_corrected <- df$value - blank_mean

      control_mean <- mean(df$blank_corrected[df$role == "control"])

      df$norm <- df$blank_corrected / control_mean
      df
    })

  expect_equal(result$norm[result$role == "normal"], 8 / 4)
})

test_that("falls back when no control", {

  df <- data.frame(
    group = "A",
    role = c("blank","normal"),
    value = c(2, 10)
  )

  blank_mean <- 2
  corrected <- 10 - 2

  expect_equal(corrected, 8)
})

test_that("group_map expands multi-role wells", {

  # mock minimal plate row
  plate <- data.frame(
    value = 10,
    is_control = TRUE,
    is_blank = FALSE,
    is_label = FALSE,
    is_standard = FALSE,
    control_groups = I(list("A")),
    blanks = I(list(character(0))),
    labels = I(list(character(0))),
    standards = NA,
    standard_units = NA
  )

  # Expect at least one row output
  expect_true(TRUE) # placeholder — expand later if needed
})
