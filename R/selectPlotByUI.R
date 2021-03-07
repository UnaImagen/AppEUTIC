selectPlotByUI <- function(id) {

   if (id == "hogares_graficar_segun") {

      choice_names <- base::list(
         shiny::icon("map-marked-alt"),
         shiny::icon("dollar-sign")
      )

      choice_values <- base::list(
         "localidad",
         "ingresos_total"
      )

   } else {

      choice_names <- base::list(
         shiny::icon("map-marked-alt"),
         shiny::icon("dollar-sign"),
         shiny::icon("venus-mars"),
         shiny::icon("graduation-cap")
      )

      choice_values <- base::list(
         "localidad",
         "ingresos_total",
         "sexo",
         "nivel_educ"
      )
   }

   shiny::radioButtons(
      inputId = id,
      label = "Graficar según:",
      choiceNames = choice_names,
      choiceValues = choice_values,
      selected = "localidad",
      inline = TRUE
   )

}