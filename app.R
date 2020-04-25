#=================#
#### APP ENCOR ####
#=================#

library(shiny, quietly = TRUE)
library(magrittr, quietly = TRUE)

eutic <- readr::read_rds(path = "eutic.rds")

# UI ----------------------------------------------------------------------
ui <- shiny::tagList(

   # shinythemes::themeSelector(),

   shiny::navbarPage(

      theme = shinythemes::shinytheme(theme = "flatly"),

      title = "EUTIC",

      # Tab ideales -------------------------------------------------------------
      shiny::tabPanel(

         title = "Hogares",

         shiny::sidebarPanel(

            shiny::h4("Encuesta de Usos de las Tecnologías de la Información y Comunicación"),

            shiny::selectInput(
               inputId = "localidad",
               label = "Localidad:",
               choices = base::levels(eutic$localidad),
               selected = base::levels(eutic$localidad),
               multiple = TRUE
            ),

            shiny::selectInput(
               inputId = "ingresos",
               label = "Nivel de ingresos del hogar:",
               choices = base::levels(eutic$ingresos),
               selected = base::levels(eutic$ingresos),
               multiple = TRUE
            ),

            shiny::strong("Hogares que tengan:"),

            shiny::checkboxInput(
               inputId = "tienen_desktop",
               label = "Desktop",
               value = TRUE
            ),

            shiny::checkboxInput(
               inputId = "tienen_laptop",
               label = "Laptop",
               value = TRUE
            ),

            shiny::checkboxInput(
               inputId = "tienen_tablet",
               label = "Tablet",
               value = TRUE
            ),

            shiny::selectInput(
               inputId = "condicion",
               label = "Condición:",
               choices = base::c("Todas las seleccionadas", "Al menos una de las seleccionadas"),
               selected = "Al menos una de las seleccionadas",
               multiple = FALSE
            ),

            shiny::p("Fuente: Instituto Nacional de Estadística"),

            shiny::p("Nota: el nivel de ingresos se presenta por quintil, donde Q5 son los hogares de mayores ingresos, y Q1 son los hogares de
                     menores ingresos."),

         ),

         shiny::mainPanel(

            shiny::verbatimTextOutput("tienen_desktop"),
            shiny::verbatimTextOutput("tienen_laptop"),
            shiny::verbatimTextOutput("tienen_tablet"),

            shiny::verbatimTextOutput("que_tienen")

         )

      )

   )

)

# Server ------------------------------------------------------------------
server <- function(input, output) {

   tienen_

   output$tienen_desktop <- shiny::renderText({
      dplyr::case_when(
         input$tienen_desktop == TRUE ~ "tiene_desktop",
         TRUE ~ ""
      )
   })

   output$tienen_laptop <- shiny::renderText({
      dplyr::case_when(
         input$tienen_laptop == TRUE ~ "tiene_laptop",
         TRUE ~ ""
      )
   })

   output$tienen_tablet <- shiny::renderText({
      dplyr::case_when(
         input$tienen_tablet == TRUE ~ "tiene_tablet",
         TRUE ~ ""
      )
   })

   output$que_tienen <- shiny::reactive({

      stringr::str_c(output$tienen_desktop)

   })

}


# Shiny App ---------------------------------------------------------------

shiny::shinyApp(
   ui = ui,
   server = server
)

#===============#
#### THE END ####
#===============#