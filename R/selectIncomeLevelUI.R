selectIncomeLevelUI <- function(id, levels) {
   shiny::selectInput(
      inputId = id,
      label = "Nivel de ingresos del hogar:",
      choices = levels,
      selected = levels,
      multiple = TRUE
   )
}