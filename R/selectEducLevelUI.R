selectEducLevelUI <- function(id, var) {
   shiny::selectInput(
      inputId = id,
      label = "Nivel educativo:",
      choices = base::levels(var),
      selected = base::levels(var),
      multiple = TRUE
   )
}