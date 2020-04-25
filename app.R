#=================#
#### APP ENCOR ####
#=================#

library(shiny, quietly = TRUE)
library(magrittr, quietly = TRUE)

eutic <- readr::read_rds(path = "eutic.rds")
tipo_internet <- readr::read_rds(path = "tipo_internet.rds")

# UI ----------------------------------------------------------------------
ui <- shiny::tagList(

   # shinythemes::themeSelector(),

   shiny::navbarPage(

      theme = shinythemes::shinytheme(theme = "flatly"),

      title = "EUTIC",


      # Tab: Hogares - Dispositivos ---------------------------------------------
      shiny::tabPanel(

         title = "Hogares - Dispositivos",

         shiny::sidebarPanel(

            shiny::h4("Encuesta de Usos de las Tecnologías de la Información y Comunicación"),

            shiny::radioButtons(
               inputId = "hogares_que_tienen_dispositivo",
               label = "Hogares que tengan:",
               choiceNames = base::list(
                  shiny::icon("desktop"),
                  shiny::icon("laptop"),
                  shiny::icon("tablet")
               ),
               choiceValues = base::list(
                  "tiene_desktop",
                  "tiene_laptop",
                  "tiene_tablet"
               ),
               selected = "tiene_desktop",
               inline = TRUE
            ),

            shiny::radioButtons(
               inputId = "resultados_por",
               label = "Graficar según:",
               choiceNames = base::list(
                  shiny::icon("map-marked-alt"),
                  shiny::icon("dollar-sign")
               ),
               choiceValues = base::list(
                  "localidad",
                  "ingresos_total"
               ),
               selected = "localidad",
               inline = TRUE
            ),

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
               choices = base::levels(eutic$ingresos_total),
               selected = base::levels(eutic$ingresos_total),
               multiple = TRUE
            ),

            shiny::p("Fuente: Instituto Nacional de Estadística"),

            shiny::p("Nota: el nivel de ingresos se presenta por quintil, donde Q5 son los hogares de mayores ingresos, y Q1 son los hogares de
                     menores ingresos del país.")

         ),

         shiny::mainPanel(

            plotly::plotlyOutput(outputId = "plotly_hogares_dispositivos")

         )

      )

      # Tab: Hogares - Internet -------------------------------------------------
      # shiny::tabPanel(
      #
      #    title = "Hogares - Internet",
      #
      #    shiny::sidebarPanel(
      #
      #       shiny::h4("Encuesta de Usos de las Tecnologías de la Información y Comunicación"),
      #
      #       shiny::selectInput(
      #          inputId = "hogares_que_tienen_internet",
      #          label = "Hogares que tengan:",
      #          choices = base::c("Internet"),
      #          selected = "Internet"
      #       ),
      #
      #       shiny::selectInput(
      #          inputId = "resultados_por",
      #          label = "Graficar según:",
      #          choices = base::c("Nivel de ingresos", "Localidad"),
      #          selected = "Localidad"
      #       ),
      #
      #       shiny::selectInput(
      #          inputId = "localidad",
      #          label = "Localidad:",
      #          choices = base::levels(eutic$localidad),
      #          selected = base::levels(eutic$localidad),
      #          multiple = TRUE
      #       ),
      #
      #       shiny::selectInput(
      #          inputId = "ingresos",
      #          label = "Nivel de ingresos del hogar:",
      #          choices = base::levels(eutic$ingresos_total),
      #          selected = base::levels(eutic$ingresos_total),
      #          multiple = TRUE
      #       ),
      #
      #       shiny::p("Fuente: Instituto Nacional de Estadística"),
      #
      #       shiny::p("Nota: el nivel de ingresos se presenta por quintil, donde Q5 son los hogares de mayores ingresos, y Q1 son los hogares de
      #                menores ingresos del país."),
      #
      #    ),
      #
      #    shiny::mainPanel(
      #
      #       plotly::plotlyOutput(outputId = "hogares_internet")
      #
      #    )
      #
      # )

   )

)

# Server ------------------------------------------------------------------
server <- function(input, output) {

   # Funciones ---------------------------------------------------------------

   plotly_hogares_tienen <- function(.data, group_var_1, group_var_2) {

      legend_title <- dplyr::case_when(
         group_var_2 == "tiene_desktop" ~ "¿Tiene desktop en el hogar?",
         group_var_2 == "tiene_laptop" ~ "¿Tiene laptop en el hogar?",
         group_var_2 == "tiene_tablet" ~ "¿Tiene tablet en el hogar?",
         group_var_2 == "tiene_internet" ~ "¿Tiene internet en el hogar?"
      )

      .data %>%
         dplyr::transmute(
            group_var_1 = !!rlang::sym(group_var_1),
            group_var_2 = !!rlang::sym(group_var_2),
            peso_hogar
         ) %>%
         dplyr::group_by(
            group_var_1,
            group_var_2
         ) %>%
         dplyr::summarise(
            n = base::sum(peso_hogar)
         ) %>%
         dplyr::mutate(
            proporcion = n / base::sum(n)
         ) %>%
         dplyr::ungroup() %>%
         plotly::plot_ly() %>%
         plotly::add_trace(
            x = ~group_var_1,
            y = ~proporcion,
            color = ~group_var_2,
            type = "bar",
            hovertemplate = ~base::paste0(
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
            ),
            hovermode = "x"
         ) %>%
         plotly::config(
            locale = "es",
            displayModeBar = TRUE
         )

   }

   # Tab: Hogares - Dispositivos ---------------------------------------------

   output$plotly_hogares_dispositivos <- plotly::renderPlotly({

      eutic %>%
         dplyr::filter(
            localidad %in% input$localidad,
            ingresos_total %in% input$ingresos
         ) %>%
         plotly_hogares_tienen(
            group_var_1 = input$resultados_por,
            group_var_2 = input$hogares_que_tienen_dispositivo
         )

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