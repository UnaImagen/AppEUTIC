selectLocalityUI <- function(id, var) {
   shiny::selectInput(
      inputId = id,
      label = "Localidad:",
      choices = base::levels(var),
      selected = base::levels(var),
      multiple = TRUE
   )
}