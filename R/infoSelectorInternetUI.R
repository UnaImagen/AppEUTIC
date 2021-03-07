infoSelectorInternetUI <- function() {
   shiny::radioButtons(
      inputId = "internet",
      label = "Personas que usan internet para...",
      choiceNames = base::list(
         shiny::icon("info"),
         shiny::icon("book"),
         shiny::icon("briefcase"),
         shiny::icon("comments"),
         shiny::icon("theater-masks"),
         shiny::icon("shopping-cart")
      ),
      choiceValues = base::list(
         "_buscar_info_",
         "_estudio_",
         "_trabajo_",
         "_comms_",
         "_ocio_",
         "_comercio_"
      ),
      selected = "_buscar_info_",
      inline = TRUE
   )
}