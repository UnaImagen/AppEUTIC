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

            # shiny::selectInput(
            #    inputId = "localidad",
            #    label = "Localidad:",
            #    choices = base::levels(eutic$localidad),
            #    selected = base::levels(eutic$localidad),
            #    multiple = TRUE
            # ),

            shiny::selectInput(
               inputId = "ingresos",
               label = "Nivel de ingresos del hogar:",
               choices = base::levels(eutic$ingresos_total),
               selected = base::levels(eutic$ingresos_total),
               multiple = TRUE
            ),

            shiny::selectInput(
               inputId = "hogares_que_tienen",
               label = "Hogares que tengan:",
               choices = base::c("Desktop", "Laptop", "Tablet", "Internet"),
               selected = "Desktop"
            ),

            shiny::p("Fuente: Instituto Nacional de Estadística"),

            shiny::p("Nota: el nivel de ingresos se presenta por quintil, donde Q5 son los hogares de mayores ingresos, y Q1 son los hogares de
                     menores ingresos del país."),

         ),

         shiny::mainPanel(

            plotly::plotlyOutput(outputId = "hogares")

         )

      )

   )

)

# Server ------------------------------------------------------------------
server <- function(input, output) {

   # Funciones ---------------------------------------------------------------

   plotly_hogares_tienen <- function(tienen) {

      legend_title <- dplyr::case_when(
         tienen == "tiene_desktop" ~ "¿Tiene desktop en el hogar?",
         tienen == "tiene_laptop" ~ "¿Tiene laptop en el hogar?",
         tienen == "tiene_tablet" ~ "¿Tiene tablet en el hogar?",
         tienen == "tiene_internet" ~ "¿Tiene internet en el hogar?"
      )

      eutic %>%
         dplyr::filter(
            ingresos_total %in% input$ingresos
         ) %>%
         dplyr::transmute(
            localidad,
            tiene = !!rlang::sym(tienen),
            peso_hogar
         ) %>%
         dplyr::group_by(
            localidad,
            tiene
         ) %>%
         dplyr::summarise(
            n = base::sum(peso_hogar)
         ) %>%
         dplyr::mutate(
            prop = n / base::sum(n)
         ) %>%
         plotly::plot_ly() %>%
         plotly::add_trace(
            x = ~localidad,
            y = ~prop,
            color = ~tiene,
            type = "bar",
            hovertemplate = ~paste0(
               "%{y:0.2%}"
            )
         ) %>%
         plotly::layout(
            xaxis = base::list(
               title = NA
            ),
            yaxis = base::list(
               title = "<b>Porcentaje de los hogares</b>",
               tickformat = "%"
            ),
            legend = base::list(
               title = base::list(
                  text = base::paste("<b>", legend_title, "</b>")
               ),
               bgcolor = "#E2E2E2",
               orientation = "h",
               yanchor = "bottom",
               xanchor = "left",
               y = -.40
            )
         ) %>%
         plotly::config(
            locale = "es",
            displayModeBar = TRUE
         )

   }

   # Tab hogares -------------------------------------------------------------

   var_hogar_tiene <- shiny::reactive({

      dplyr::case_when(
         input$hogares_que_tienen == "Desktop" ~ "tiene_desktop",
         input$hogares_que_tienen == "Laptop" ~ "tiene_laptop",
         input$hogares_que_tienen == "Tablet" ~ "tiene_tablet",
         input$hogares_que_tienen == "Internet" ~ "tiene_internet",
      )

   })

   output$hogares <- plotly::renderPlotly({

      plotly_hogares_tienen(var_hogar_tiene())

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