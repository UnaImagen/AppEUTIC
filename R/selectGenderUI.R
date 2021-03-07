selectGenderUI <- function(id, var) {
   shiny::selectInput(
      inputId = id,
      label = "Sexo:",
      choices = base::levels(var),
      selected = base::levels(var),
      multiple = TRUE
   )
}