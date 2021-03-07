infoSelectorHomesUI <- function() {
   shiny::radioButtons(
      inputId = "hogares",
      label = "Hogares que tengan...",
      choiceNames = base::list(
         shiny::icon("desktop"),
         shiny::icon("laptop"),
         shiny::icon("tablet"),
         shiny::icon("at")
      ),
      choiceValues = base::list(
         "tiene_desktop",
         "tiene_laptop",
         "tiene_tablet",
         "tiene_internet"
      ),
      selected = "tiene_desktop",
      inline = TRUE
   )
}