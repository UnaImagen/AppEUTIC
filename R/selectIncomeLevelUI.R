selectIncomeLevelUI <- function(id, var) {
   shiny::selectInput(
      inputId = id,
      label = "Nivel de ingresos del hogar:",
      choices = base::levels(var),
      selected = base::levels(var),
      multiple = TRUE
   )
}