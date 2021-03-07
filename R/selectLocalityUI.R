selectLocalityUI <- function(id, levels) {
   shiny::selectInput(
      inputId = id,
      label = "Localidad:",
      choices = levels,
      selected = levels,
      multiple = TRUE
   )
}