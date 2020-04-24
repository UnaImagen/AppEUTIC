#=================#
#### APP ENCOR ####
#=================#

library(shiny, quietly = TRUE)
library(magrittr, quietly = TRUE)

# UI ----------------------------------------------------------------------
ui <- shiny::tagList(

      # shinythemes::themeSelector(),

   shiny::navbarPage(

      theme = shinythemes::shinytheme(theme = "flatly"),

      title = "EUTIC",

      # Tab ideales -------------------------------------------------------------
      shiny::tabPanel(

         title = "Tab Name",

         shiny::sidebarPanel(

            shiny::h4("Encuesta de Usos de las Tecnologías de la Información y Comunicación"),


            shiny::p("Fuente: Instituto Nacional de Estadística")

         )

      )

   )

)

# Server ------------------------------------------------------------------
server <- function(input, output) {


}


# Shiny App ---------------------------------------------------------------

shiny::shinyApp(
   ui = ui,
   server = server
)

#===============#
#### THE END ####
#===============#