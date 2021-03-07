infoSelectorPeopleUI <- function() {
   shiny::radioButtons(
      inputId = "personas",
      label = "Personas que usen...",
      choiceNames = base::list(
         shiny::icon("mobile-alt"),
         shiny::icon("at")
      ),
      choiceValues = base::list(
         "uso_celular",
         "uso_internet"
      ),
      selected = "uso_celular",
      inline = TRUE
   )
}
