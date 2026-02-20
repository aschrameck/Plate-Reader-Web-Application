server_normalize <- function(input, output, session, state, plates) {

  output$normalized_preview <- renderTable({
    data.frame(
      Well = c("A1", "A2", "B1"),
      Value = c(1.2, 0.9, 1.5)
    )
  })

}
