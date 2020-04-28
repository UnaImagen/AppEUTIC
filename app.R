#=================#
#### APP EUTIC ####
#=================#

library(shiny, quietly = TRUE)
library(magrittr, quietly = TRUE)

eutic <- readr::read_rds(path = "eutic.rds")

# UI ----------------------------------------------------------------------
ui <- shiny::tagList(

   shiny::navbarPage(

      collapsible = TRUE,

      theme = shinythemes::shinytheme(theme = "flatly"),

      title = "EUTIC",

      # Tab: Hogares ------------------------------------------------------------

      shiny::tabPanel(

         icon = shiny::icon("home"),

         title = " Hogares",

         shiny::sidebarPanel(

            shiny::h4("Encuesta de Usos de las Tecnologías de la Información y Comunicación"),

            shiny::radioButtons(
               inputId = "hogares",
               label = "Hogares que tengan:",
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
            ),

            shiny::radioButtons(
               inputId = "hogares_graficar_segun",
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
               inputId = "hogares_localidad",
               label = "Localidad:",
               choices = base::levels(eutic$localidad),
               selected = base::levels(eutic$localidad),
               multiple = TRUE
            ),

            shiny::selectInput(
               inputId = "hogares_ingresos",
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

            plotly::plotlyOutput(outputId = "hogares_plot_uno"),

            plotly::plotlyOutput(outputId = "hogares_plot_dos")

         )

      ),

      # Tab: Personas -----------------------------------------------------------

      shiny::tabPanel(

         icon = shiny::icon("user"),

         title = "Personas",

         shiny::sidebarPanel(

            shiny::h4("Encuesta de Usos de las Tecnologías de la Información y Comunicación"),

            shiny::radioButtons(
               inputId = "personas",
               label = "TIC:",
               choiceNames = base::list(
                  shiny::icon("mobile-alt"),
                  shiny::icon("laptop"),
                  shiny::icon("tablet"),
                  shiny::icon("at")
               ),
               choiceValues = base::list(
                  "uso_celular",
                  "uso_pc",
                  "uso_tablet",
                  "uso_internet"
               ),
               selected = "uso_celular",
               inline = TRUE
            ),

            shiny::radioButtons(
               inputId = "personas_graficar_segun",
               label = "Graficar según:",
               choiceNames = base::list(
                  shiny::icon("map-marked-alt"),
                  shiny::icon("dollar-sign"),
                  shiny::icon("venus-mars"),
                  shiny::icon("graduation-cap")

               ),
               choiceValues = base::list(
                  "localidad",
                  "ingresos_total",
                  "sexo",
                  "nivel_educ"
               ),
               selected = "localidad",
               inline = TRUE
            ),

            shiny::selectInput(
               inputId = "localidad_personas",
               label = "Localidad:",
               choices = base::levels(eutic$localidad),
               selected = base::levels(eutic$localidad),
               multiple = TRUE
            ),

            shiny::selectInput(
               inputId = "ingresos_personas",
               label = "Nivel de ingresos del hogar:",
               choices = base::levels(eutic$ingresos_total),
               selected = base::levels(eutic$ingresos_total),
               multiple = TRUE
            ),

            shiny::sliderInput(
               inputId = "edad_personas",
               label = "Edad:",
               min = base::min(eutic$edad, na.rm = TRUE),
               max = base::max(eutic$edad, na.rm = TRUE),
               value = base::c(
                  base::min(eutic$edad, na.rm = TRUE),
                  base::max(eutic$edad, na.rm = TRUE)
               ),
               step = 1,
               animate = TRUE
            ),

            shiny::selectInput(
               inputId = "sexo_personas",
               label = "Sexo:",
               choices = base::levels(eutic$sexo),
               selected = base::levels(eutic$sexo),
               multiple = TRUE
            ),

            shiny::selectInput(
               inputId = "nivel_educ_personas",
               label = "Nivel educativo:",
               choices = base::levels(eutic$nivel_educ),
               selected = base::levels(eutic$nivel_educ),
               multiple = TRUE
            ),

            shiny::p("Fuente: Instituto Nacional de Estadística"),

            shiny::p(
               "Nota: el nivel de ingresos se presenta por quintil (grupos de a 20%), donde Q5 son los hogares de mayores ingresos, y Q1 son
               los hogares de menores ingresos del país."
            )

         ),

         shiny::mainPanel(

            plotly::plotlyOutput(outputId = "personas_plot_uno"),

            plotly::plotlyOutput(outputId = "personas_plot_dos"),

            plotly::plotlyOutput(outputId = "personas_plot_tres")

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

   plotly_hogares_cantidad_dispositivos <- function(.data, group_var_1, group_var_2, filter_var = group_var_2) {

      xaxis_title <- dplyr::case_when(
         group_var_1 == "localidad" ~ "Localidad",
         group_var_1 == "ingresos_total" ~ "Nivel de ingresos"
      )

      legend_title <- dplyr::case_when(
         group_var_2 == "tiene_desktop" ~ "Cantidad de desktops en el hogar<br> (para hogares que tienen)",
         group_var_2 == "tiene_laptop" ~ "Cantidad de laptops en el hogar<br> (para hogares que tienen)",
         group_var_2 == "tiene_tablet" ~ "Cantidad de tablets en el hogar<br> (para hogares que tienen)"
      )

      .data %>%
         dplyr::filter(
            !!rlang::sym(filter_var) == "Sí"
         ) %>%
         base::droplevels() %>%
         dplyr::mutate(
            group_var_1 = !!rlang::sym(group_var_1),
            group_var_2 = !!rlang::sym(
               stringr::str_replace(
                  string = group_var_2,
                  pattern = "tiene",
                  replacement = "cantidad"
               )
            )
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
            x = ~group_var_1,
            y = ~prop,
            color = ~group_var_2,
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
                  text = base::paste("<b>", "Tipo de conexión<br> (para hogares que tienen)", "</b>")
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

   plotly_personas_uso_tic <- function(.data, group_var_1, group_var_2, plotly_legend_y = -0.30) {

      colors <- dplyr::if_else(group_var_2 %in% base::c("frecuencia_uso_internet", "frecuencia_uso_internet_celular"), "Accent", "Paired")

      xaxis_title <- dplyr::case_when(
         group_var_1 == "localidad" ~ "Localidad",
         group_var_1 == "ingresos_total" ~ "Nivel del ingresos (del hogar)",
         group_var_1 == "sexo" ~ "Sexo",
         group_var_1 == "nivel_educ" ~ "Nivel educativo"
      )

      legend_title <- dplyr::case_when(
         group_var_2 == "uso_celular" ~ "¿Utilizó un celular en los<br> últimos 3 meses?",
         group_var_2 == "uso_tablet" ~ "¿Utilizó una tablet en los<br> últimos 3 meses?",
         group_var_2 == "uso_pc" ~ "¿Utilizó una PC en los<br> últimos 3 meses?",
         group_var_2 == "uso_internet" ~ "¿Utilizó alguna vez Internet?",
         group_var_2 == "frecuencia_uso_internet" ~ "¿Con qué frecuencia utilizó<br> Internet en los últimos<br> 3 meses? (para quienes lo<br> utilizaron)",
         group_var_2 == "frecuencia_uso_internet_celular" ~ "¿Con qué frecuencia utilizó Internet<br> en el celular en los últimos 3 meses?<br> (para quienes lo utilizaron)"
      )

      .data %>%
         dplyr::mutate(
            group_var_1 = !!rlang::sym(group_var_1),
            group_var_2 = !!rlang::sym(group_var_2)
         ) %>%
         dplyr::group_by(
            group_var_1,
            group_var_2
         ) %>%
         dplyr::summarise(
            n = base::sum(peso_persona, na.rm = TRUE)
         ) %>%
         dplyr::mutate(
            prop = n / base::sum(n, na.rm = TRUE)
         ) %>%
         dplyr::ungroup() %>%
         plotly::plot_ly() %>%
         plotly::add_trace(
            x = ~group_var_1,
            y = ~prop,
            color = ~group_var_2,
            colors = colors,
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
               title = "<b>Porcentaje de las personas</b>",
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
               y = plotly_legend_y
            ),
            hovermode = "x"
         ) %>%
         plotly::config(
            locale = "es",
            displayModeBar = TRUE
         )

   }

   generar_data_usos_celular <- function(.data, group_by_var) {

      ## Genera data uso celular llamadas
      aux_data <- .data %>%
         dplyr::mutate(
            group_by_var = !!rlang::sym(group_by_var)
         ) %>%
         dplyr::group_by(
            group_by_var,
            usos_celular_llamadas
         ) %>%
         dplyr::summarise(
            n = base::sum(peso_hogar, na.rm = TRUE)
         ) %>%
         dplyr::mutate(
            proporcion = n / base::sum(n, na.rm = TRUE)
         ) %>%
         dplyr::ungroup() %>%
         dplyr::filter(
            usos_celular_llamadas == "Sí"
         ) %>%
         dplyr::transmute(
            group_by_var,
            tipo_uso = "llamadas",
            proporcion
         )

      aux_data %<>%
         dplyr::bind_rows(

            ## Genera data uso celular mensajes
            .data %>%
               dplyr::mutate(
                  group_by_var = !!rlang::sym(group_by_var)
               ) %>%
               dplyr::group_by(
                  group_by_var,
                  usos_celular_mensajes
               ) %>%
               dplyr::summarise(
                  n = base::sum(peso_hogar, na.rm = TRUE)
               ) %>%
               dplyr::mutate(
                  proporcion = n / base::sum(n, na.rm = TRUE)
               ) %>%
               dplyr::ungroup() %>%
               dplyr::filter(
                  usos_celular_mensajes == "Sí"
               ) %>%
               dplyr::transmute(
                  group_by_var,
                  tipo_uso = "mensajes",
                  proporcion
               ),

            ## Genera data uso celular multimedia y redes
            .data %>%
               dplyr::mutate(
                  group_by_var = !!rlang::sym(group_by_var)
               ) %>%
               dplyr::group_by(
                  group_by_var,
                  usos_celular_multimedia_y_redes
               ) %>%
               dplyr::summarise(
                  n = base::sum(peso_hogar, na.rm = TRUE)
               ) %>%
               dplyr::mutate(
                  proporcion = n / base::sum(n, na.rm = TRUE)
               ) %>%
               dplyr::ungroup() %>%
               dplyr::filter(
                  usos_celular_multimedia_y_redes == "Sí"
               ) %>%
               dplyr::transmute(
                  group_by_var,
                  tipo_uso = "multimedia y redes",
                  proporcion
               ),

            ## Genera data uso celular buscar información
            .data %>%
               dplyr::mutate(
                  group_by_var = !!rlang::sym(group_by_var)
               ) %>%
               dplyr::group_by(
                  group_by_var,
                  usos_celular_informacion
               ) %>%
               dplyr::summarise(
                  n = base::sum(peso_hogar, na.rm = TRUE)
               ) %>%
               dplyr::mutate(
                  proporcion = n / base::sum(n, na.rm = TRUE)
               ) %>%
               dplyr::ungroup() %>%
               dplyr::filter(
                  usos_celular_informacion == "Sí"
               ) %>%
               dplyr::transmute(
                  group_by_var,
                  tipo_uso = "informacion",
                  proporcion
               ),

            ## Genera data uso celular compras
            .data %>%
               dplyr::mutate(
                  group_by_var = !!rlang::sym(group_by_var)
               ) %>%
               dplyr::group_by(
                  group_by_var,
                  usos_celular_compras
               ) %>%
               dplyr::summarise(
                  n = base::sum(peso_hogar, na.rm = TRUE)
               ) %>%
               dplyr::mutate(
                  proporcion = n / base::sum(n, na.rm = TRUE)
               ) %>%
               dplyr::ungroup() %>%
               dplyr::filter(
                  usos_celular_compras == "Sí"
               ) %>%
               dplyr::transmute(
                  group_by_var,
                  tipo_uso = "compras",
                  proporcion
               )
         ) %>%
         dplyr::mutate(
            tipo_uso = dplyr::case_when(
               tipo_uso == "llamadas" ~ "Llamadas",
               tipo_uso == "mensajes" ~ "Mensajes",
               tipo_uso == "multimedia y redes" ~ "Multimedia y Redes Sociales",
               tipo_uso == "informacion" ~ "Buscar información",
               tipo_uso == "compras" ~ "Realizar compras"
            ),
            tipo_uso = forcats::as_factor(tipo_uso)
         )

   }

   generar_data_usos_internet <- function(.data, group_by_var) {

      ## Genera data uso celular llamadas
      aux_data <- .data %>%
         dplyr::mutate(
            group_by_var = !!rlang::sym(group_by_var)
         ) %>%
         dplyr::group_by(
            group_by_var,
            usos_internet_comms
         ) %>%
         dplyr::summarise(
            n = base::sum(peso_hogar, na.rm = TRUE)
         ) %>%
         dplyr::mutate(
            proporcion = n / base::sum(n, na.rm = TRUE)
         ) %>%
         dplyr::ungroup() %>%
         dplyr::filter(
            usos_internet_comms == "Sí"
         ) %>%
         dplyr::transmute(
            group_by_var,
            tipo_uso = "comms",
            proporcion
         )

      aux_data %<>%
         dplyr::bind_rows(

            ## Genera data uso celular mensajes
            .data %>%
               dplyr::mutate(
                  group_by_var = !!rlang::sym(group_by_var)
               ) %>%
               dplyr::group_by(
                  group_by_var,
                  usos_internet_laboral
               ) %>%
               dplyr::summarise(
                  n = base::sum(peso_hogar, na.rm = TRUE)
               ) %>%
               dplyr::mutate(
                  proporcion = n / base::sum(n, na.rm = TRUE)
               ) %>%
               dplyr::ungroup() %>%
               dplyr::filter(
                  usos_internet_laboral == "Sí"
               ) %>%
               dplyr::transmute(
                  group_by_var,
                  tipo_uso = "laboral",
                  proporcion
               ),

            ## Genera data uso celular multimedia y redes
            .data %>%
               dplyr::mutate(
                  group_by_var = !!rlang::sym(group_by_var)
               ) %>%
               dplyr::group_by(
                  group_by_var,
                  usos_internet_estudio
               ) %>%
               dplyr::summarise(
                  n = base::sum(peso_hogar, na.rm = TRUE)
               ) %>%
               dplyr::mutate(
                  proporcion = n / base::sum(n, na.rm = TRUE)
               ) %>%
               dplyr::ungroup() %>%
               dplyr::filter(
                  usos_internet_estudio == "Sí"
               ) %>%
               dplyr::transmute(
                  group_by_var,
                  tipo_uso = "estudio",
                  proporcion
               ),

            ## Genera data uso celular buscar información
            .data %>%
               dplyr::mutate(
                  group_by_var = !!rlang::sym(group_by_var)
               ) %>%
               dplyr::group_by(
                  group_by_var,
                  usos_internet_ocio
               ) %>%
               dplyr::summarise(
                  n = base::sum(peso_hogar, na.rm = TRUE)
               ) %>%
               dplyr::mutate(
                  proporcion = n / base::sum(n, na.rm = TRUE)
               ) %>%
               dplyr::ungroup() %>%
               dplyr::filter(
                  usos_internet_ocio == "Sí"
               ) %>%
               dplyr::transmute(
                  group_by_var,
                  tipo_uso = "ocio",
                  proporcion
               ),

            ## Genera data uso celular compras
            .data %>%
               dplyr::mutate(
                  group_by_var = !!rlang::sym(group_by_var)
               ) %>%
               dplyr::group_by(
                  group_by_var,
                  usos_internet_otro
               ) %>%
               dplyr::summarise(
                  n = base::sum(peso_hogar, na.rm = TRUE)
               ) %>%
               dplyr::mutate(
                  proporcion = n / base::sum(n, na.rm = TRUE)
               ) %>%
               dplyr::ungroup() %>%
               dplyr::filter(
                  usos_internet_otro == "Sí"
               ) %>%
               dplyr::transmute(
                  group_by_var,
                  tipo_uso = "otro",
                  proporcion
               )
         ) %>%
         dplyr::mutate(
            tipo_uso = dplyr::case_when(
               tipo_uso == "comms" ~ "Comunicación",
               tipo_uso == "laboral" ~ "Laboral",
               tipo_uso == "estudio" ~ "Estudio",
               tipo_uso == "ocio" ~ "Ocio",
               tipo_uso == "otro" ~ "Otras"
            ),
            tipo_uso = forcats::as_factor(tipo_uso)
         )

   }

   plotly_personas_usos_tics <- function(.data, group_by_var, plotly_legend_y = -0.3) {

      xaxis_title <- dplyr::case_when(
         group_by_var == "localidad" ~ "Localidad",
         group_by_var == "ingresos_total" ~ "Nivel de ingresos (del hogar)",
         group_by_var == "sexo" ~ "Sexo",
         group_by_var == "nivel_educ" ~ "Nivel educativo"
      )

      .data %>%
         plotly::plot_ly() %>%
         plotly::add_trace(
            x = ~group_by_var,
            y = ~proporcion,
            color = ~tipo_uso,
            colors = "Set3",
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
               title = "<b>Porcentaje de las personas</b>",
               tickformat = "%"
            ),
            legend = base::list(
               title = base::list(
                  text = base::paste("<b>", "Activdades realizadas durante<br> los últimos 3 meses", "</b>")
               ),
               bgcolor = "#E2E2E2",
               orientation = "h",
               yanchor = "bottom",
               xanchor = "left",
               y = plotly_legend_y
            ),
            hovermode = "x"
         ) %>%
         plotly::config(
            locale = "es",
            displayModeBar = TRUE
         )

   }

   # Tab: Hogares ------------------------------------------------------------

   output$hogares_plot_uno <- plotly::renderPlotly({

      eutic %>%
         dplyr::filter(
            localidad %in% input$hogares_localidad,
            ingresos_total %in% input$hogares_ingresos
         ) %>%
         plotly_hogares_tienen(
            group_var_1 = input$hogares_graficar_segun,
            group_var_2 = input$hogares
         )

   })

   output$hogares_plot_dos <- plotly::renderPlotly({

      if (input$hogares == "tiene_internet") {

         eutic %>%
            dplyr::filter(
               localidad %in% input$hogares_localidad,
               ingresos_total %in% input$hogares_ingresos
            ) %>%
            genera_data_tipo_conexion(
               group_by_var = input$hogares_graficar_segun
            ) %>%
            plotly_tipo_conexion(
               group_by_var = input$hogares_graficar_segun
            )

      } else {

         eutic %>%
            dplyr::filter(
               localidad %in% input$hogares_localidad,
               ingresos_total %in% input$hogares_ingresos
            ) %>%
            plotly_hogares_cantidad_dispositivos(
               group_var_1 = input$hogares_graficar_segun,
               group_var_2 = input$hogares
            )

      }

   })

   # Tab: Personas -----------------------------------------------------------

   output$personas_plot_uno <- plotly::renderPlotly({

      if (input$personas == "uso_internet") {

         eutic %>%
            dplyr::filter(
               localidad %in% input$localidad_personas,
               ingresos_total %in% input$ingresos_personas,
               dplyr::between(edad, input$edad_personas[1], input$edad_personas[2]),
               sexo %in% input$sexo_personas,
               nivel_educ %in% input$nivel_educ_personas
            ) %>%
            plotly_personas_uso_tic(
               group_var_1 = input$personas_graficar_segun,
               group_var_2 = "uso_internet"
            )

      } else {

         eutic %>%
            dplyr::filter(
               localidad %in% input$localidad_personas,
               ingresos_total %in% input$ingresos_personas,
               dplyr::between(edad, input$edad_personas[1], input$edad_personas[2]),
               sexo %in% input$sexo_personas,
               nivel_educ %in% input$nivel_educ_personas
            ) %>%
            plotly_personas_uso_tic(
               group_var_1 = input$personas_graficar_segun,
               group_var_2 = input$personas,
               plotly_legend_y = -0.50
            )

      }

   })

   output$personas_plot_dos <- plotly::renderPlotly({

      if (input$personas == "uso_celular") {

         eutic %>%
            dplyr::filter(
               uso_internet == "Sí"
            ) %>%
            base::droplevels() %>%
            dplyr::filter(
               localidad %in% input$localidad_personas,
               ingresos_total %in% input$ingresos_personas,
               dplyr::between(edad, input$edad_personas[1], input$edad_personas[2]),
               sexo %in% input$sexo_personas,
               nivel_educ %in% input$nivel_educ_personas
            ) %>%
            plotly_personas_uso_tic(
               group_var_1 = input$personas_graficar_segun,
               group_var_2 = "frecuencia_uso_internet_celular",
               plotly_legend_y = -0.60
            )

      } else if (input$personas == "uso_internet") {

         eutic %>%
            dplyr::filter(
               uso_internet == "Sí"
            ) %>%
            base::droplevels() %>%
            dplyr::filter(
               localidad %in% input$localidad_personas,
               ingresos_total %in% input$ingresos_personas,
               dplyr::between(edad, input$edad_personas[1], input$edad_personas[2]),
               sexo %in% input$sexo_personas,
               nivel_educ %in% input$nivel_educ_personas
            ) %>%
            plotly_personas_uso_tic(
               group_var_1 = input$personas_graficar_segun,
               group_var_2 = "frecuencia_uso_internet",
               plotly_legend_y = -0.60
            )

      }

   })

   output$personas_plot_tres <- plotly::renderPlotly({

      if (input$personas == "uso_celular") {

         eutic %>%
            dplyr::filter(
               localidad %in% input$localidad_personas,
               ingresos_total %in% input$ingresos_personas,
               dplyr::between(edad, input$edad_personas[1], input$edad_personas[2]),
               sexo %in% input$sexo_personas,
               nivel_educ %in% input$nivel_educ_personas
            ) %>%
            generar_data_usos_celular(
               group_by_var = input$personas_graficar_segun
            ) %>%
            plotly_personas_usos_tics(
               group_by_var = input$personas_graficar_segun,
               plotly_legend_y = -0.40
            )

      } else if (input$personas == "uso_internet") {

         eutic %>%
            dplyr::filter(
               localidad %in% input$localidad_personas,
               ingresos_total %in% input$ingresos_personas,
               dplyr::between(edad, input$edad_personas[1], input$edad_personas[2]),
               sexo %in% input$sexo_personas,
               nivel_educ %in% input$nivel_educ_personas
            ) %>%
            generar_data_usos_internet(
               group_by_var = input$personas_graficar_segun
            ) %>%
            plotly_personas_usos_tics(
               group_by_var = input$personas_graficar_segun,
               plotly_legend_y = -0.50
            )

      }

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