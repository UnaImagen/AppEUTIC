#=================#
#### APP EUTIC ####
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
               inputId = "resultados_por_hogares_dispositivos",
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
               inputId = "localidad_dispositivos",
               label = "Localidad:",
               choices = base::levels(eutic$localidad),
               selected = base::levels(eutic$localidad),
               multiple = TRUE
            ),

            shiny::selectInput(
               inputId = "ingresos_dispositivos",
               label = "Nivel de ingresos del hogar:",
               choices = base::levels(eutic$ingresos_total),
               selected = base::levels(eutic$ingresos_total),
               multiple = TRUE
            ),

            shiny::p("Fuente: Instituto Nacional de Estadística"),

            shiny::p(
               "Nota: el nivel de ingresos se presenta por quintil (grupos de a 20%), donde Q5 son los hogares de mayores ingresos, y Q1 son
               los hogares de menores ingresos del país."
            )

         ),

         shiny::mainPanel(

            plotly::plotlyOutput(outputId = "plotly_hogares_dispositivos"),

            plotly::plotlyOutput(outputId = "plotly_cantidad_dispositivos_hogar")

         )

      ),

      # Tab: Hogares - Internet -------------------------------------------------
      shiny::tabPanel(

         title = "Hogares - Internet",

         shiny::sidebarPanel(

            shiny::h4("Encuesta de Usos de las Tecnologías de la Información y Comunicación"),

            shiny::radioButtons(
               inputId = "hogares_que_tienen_internet",
               label = "Hogares que tengan:",
               choiceNames = base::list(
                  "Internet"
               ),
               choiceValues = base::list(
                  "tiene_internet"
               ),
               selected = "tiene_internet",
               inline = TRUE
            ),

            shiny::radioButtons(
               inputId = "resultados_por_hogares_conexion",
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
               inputId = "localidad_conexion",
               label = "Localidad:",
               choices = base::levels(eutic$localidad),
               selected = base::levels(eutic$localidad),
               multiple = TRUE
            ),

            shiny::selectInput(
               inputId = "ingresos_conexion",
               label = "Nivel de ingresos del hogar:",
               choices = base::levels(eutic$ingresos_total),
               selected = base::levels(eutic$ingresos_total),
               multiple = TRUE
            ),

            shiny::p("Fuente: Instituto Nacional de Estadística"),

            shiny::p(
               "Nota: el nivel de ingresos se presenta por quintil (grupos de a 20%), donde Q5 son los hogares de mayores ingresos, y Q1 son
               los hogares de menores ingresos del país."
            )

         ),

         shiny::mainPanel(

            plotly::plotlyOutput(outputId = "hogares_internet"),

            plotly::plotlyOutput(outputId = "hogares_tipo_conexion")

         )

      )

   )

)

# Server ------------------------------------------------------------------
server <- function(input, output) {

   # Funciones ---------------------------------------------------------------

   plotly_hogares_tienen <- function(.data, group_var_1, group_var_2) {

      xaxis_title <- dplyr::case_when(
         group_var_1 == "localidad" ~ "Localidad",
         group_var_1 == "ingresos_total" ~ "Nivel de ingresos"
      )

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
            n = base::sum(peso_hogar, na.rm = TRUE)
         ) %>%
         dplyr::mutate(
            proporcion = n / base::sum(n, na.rm = TRUE)
         ) %>%
         dplyr::ungroup() %>%
         plotly::plot_ly() %>%
         plotly::add_trace(
            x = ~group_var_1,
            y = ~proporcion,
            color = ~group_var_2,
            colors = "Paired",
            type = "bar",
            hovertemplate = ~base::paste0(
               "%{y:0.2%}"
            )
         ) %>%
         plotly::layout(
            xaxis = base::list(
               title = base::paste("<b>", xaxis_title, "</b>")
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

   plotly_hogares_cantidad <- function(.data, group_var_1, group_var_2, filter_var = group_var_2) {

      legend_title <- dplyr::case_when(
         group_var_1 == "localidad" ~ "Localidad",
         group_var_1 == "ingresos_total" ~ "Nivel de ingresos"
      )

      xaxis_title <- dplyr::case_when(
         group_var_2 == "tiene_desktop" ~ "Cantidad de desktops en el hogar<br>(para hogares que tienen)",
         group_var_2 == "tiene_laptop" ~ "Cantidad de laptops en el hogar<br>(para hogares que tienen)",
         group_var_2 == "tiene_tablet" ~ "Cantidad de tablets en el hogar<br>(para hogares que tienen)"
      )

      .data %>%
         dplyr::mutate(
            group_var_1 = !!rlang::sym(group_var_1),
            group_var_2 = !!rlang::sym(
               stringr::str_replace(
                  string = group_var_2,
                  pattern = "tiene",
                  replacement = "cantidad"
               )
            ),
            filter_var = !!rlang::sym(filter_var)
         ) %>%
         dplyr::filter(
            filter_var == "Sí"
         ) %>%
         dplyr::group_by(
            group_var_1,
            group_var_2
         ) %>%
         dplyr::summarise(
            n = base::sum(peso_hogar, na.rm = TRUE)
         ) %>%
         dplyr::mutate(
            prop = n / base::sum(n, na.rm = TRUE)
         ) %>%
         dplyr::ungroup() %>%
         plotly::plot_ly() %>%
         plotly::add_trace(
            x = ~group_var_2,
            y = ~prop,
            color = ~group_var_1,
            colors = "Accent",
            type = "bar",
            hovertemplate = ~base::paste0(
               "%{y:0.2%}"
            )
         ) %>%
         plotly::layout(
            xaxis = base::list(
               title = base::paste("<b>", xaxis_title, "</b>")
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
               y = -.45
            ),
            hovermode = "x"
         ) %>%
         plotly::config(
            locale = "es",
            displayModeBar = TRUE
         )

   }

   genera_data_tipo_conexion <- function(.data, group_by_var) {

      ## Hogares con Banda Ancha Fija
      aux_data <- .data %>%
         dplyr::filter(
            tiene_internet == "Sí"
         ) %>%
         dplyr::group_by(
            group_by_var = !!rlang::sym(group_by_var),
            banda_ancha_fija
         ) %>%
         dplyr::summarise(
            n = base::sum(peso_hogar, na.rm = TRUE)
         ) %>%
         dplyr::mutate(
            prop = n / base::sum(n, na.rm = TRUE)
         ) %>%
         dplyr::ungroup() %>%
         dplyr::filter(
            banda_ancha_fija == "Sí"
         ) %>%
         dplyr::transmute(
            group_by_var,
            tipo_conexion = "banda_ancha_fija",
            prop = prop
         )

      aux_data %<>%

         ## Hogares con Banda Ancha Móvil
         dplyr::bind_rows(
            .data %>%
               dplyr::filter(
                  tiene_internet == "Sí"
               ) %>%
               dplyr::group_by(
                  group_by_var = !!rlang::sym(group_by_var),
                  banda_ancha_movil
               ) %>%
               dplyr::summarise(
                  n = base::sum(peso_hogar, na.rm = TRUE)
               ) %>%
               dplyr::mutate(
                  prop = n / base::sum(n, na.rm = TRUE)
               ) %>%
               dplyr::ungroup() %>%
               dplyr::filter(
                  banda_ancha_movil == "Sí"
               ) %>%
               dplyr::transmute(
                  group_by_var,
                  tipo_conexion = "banda_ancha_movil",
                  prop = prop
               ),

            ## Hogares con otros tipos de conexión
            .data %>%
               dplyr::filter(
                  tiene_internet == "Sí"
               ) %>%
               dplyr::group_by(
                  group_by_var = !!rlang::sym(group_by_var),
                  otra_conexion
               ) %>%
               dplyr::summarise(
                  n = base::sum(peso_hogar, na.rm = TRUE)
               ) %>%
               dplyr::mutate(
                  prop = n / base::sum(n, na.rm = TRUE)
               ) %>%
               dplyr::ungroup() %>%
               dplyr::filter(
                  otra_conexion == "Sí"
               ) %>%
               dplyr::transmute(
                  group_by_var,
                  tipo_conexion = "otra_conexion",
                  prop = prop
               )

         ) %>%
         dplyr::mutate(
            tipo_conexion = dplyr::case_when(
               tipo_conexion == "banda_ancha_fija" ~ "Banda ancha fija",
               tipo_conexion == "banda_ancha_movil" ~ "Banda ancha móvil",
               tipo_conexion == "otra_conexion" ~ "Otra conexión"
            ),
            tipo_conexion = forcats::as_factor(tipo_conexion)
         )

   }

   plotly_tipo_conexion <- function(.data, group_by_var) {

      xaxis_title <- dplyr::case_when(
         group_by_var == "localidad" ~ "Localidad",
         group_by_var == "ingresos_total" ~ "Nivel de ingresos"
      )

      .data %>%
         plotly::plot_ly() %>%
         plotly::add_trace(
            x = ~group_by_var,
            y = ~prop,
            color = ~tipo_conexion,
            colors = "Accent",
            type = "bar",
            hovertemplate = ~base::paste0(
               "%{y:0.2%}"
            )
         ) %>%
         plotly::layout(
            xaxis = base::list(
               title = base::paste("<b>", xaxis_title, "</b>")
            ),
            yaxis = base::list(
               title = "<b>Porcentaje de los hogares</b>",
               tickformat = "%"
            ),
            legend = base::list(
               title = base::list(
                  text = base::paste("<b>", "Tipo de conexión", "</b>")
               ),
               bgcolor = "#E2E2E2",
               orientation = "h",
               yanchor = "bottom",
               xanchor = "left",
               y = -.35
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
            localidad %in% input$localidad_dispositivos,
            ingresos_total %in% input$ingresos_dispositivos
         ) %>%
         plotly_hogares_tienen(
            group_var_1 = input$resultados_por_hogares_dispositivos,
            group_var_2 = input$hogares_que_tienen_dispositivo
         )

   })

   output$plotly_cantidad_dispositivos_hogar <- plotly::renderPlotly({

      eutic %>%
         dplyr::filter(
            localidad %in% input$localidad_dispositivos,
            ingresos_total %in% input$ingresos_dispositivos
         ) %>%
         plotly_hogares_cantidad(
            group_var_1 = input$resultados_por_hogares_dispositivos,
            group_var_2 = input$hogares_que_tienen_dispositivo
         )

   })

   # Tab: Hogares - Conexión -------------------------------------------------

   output$hogares_internet <- plotly::renderPlotly({

      eutic %>%
         dplyr::filter(
            localidad %in% input$localidad_conexion,
            ingresos_total %in% input$ingresos_conexion
         ) %>%
         plotly_hogares_tienen(
            group_var_1 = input$resultados_por_hogares_conexion,
            group_var_2 = input$hogares_que_tienen_internet
         )

   })

   output$hogares_tipo_conexion <- plotly::renderPlotly({

      eutic %>%
         dplyr::filter(
            localidad %in% input$localidad_conexion,
            ingresos_total %in% input$ingresos_conexion
         ) %>%
         genera_data_tipo_conexion(
            group_by_var = input$resultados_por_hogares_conexion
         ) %>%
         plotly_tipo_conexion(
            group_by_var = input$resultados_por_hogares_conexion
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