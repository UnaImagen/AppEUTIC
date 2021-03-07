#=================#
#### APP EUTIC ####
#=================#

library(shiny, quietly = TRUE)
library(magrittr, quietly = TRUE)

eutic <- readr::read_rds(file = "eutic.rds")

title <- "Encuesta de Usos de las Tecnologías de la Información y Comunicación (2016)"

data_source <- "Fuente: INE y AGESIC"

nota_quintiles <- base::c(
   "Nota: el nivel de ingresos se presenta por quintil (grupos de a 20%), donde Q5 son los hogares de mayores ingresos, y Q1 son los hogares de
   menores ingresos del país."
)

links <- shiny::HTML(
   '<a href="https://danielczarnievicz.netlify.app/portfolio/tecnolog%C3%ADas-de-la-informaci%C3%B3n/"><i class="fas fa-arrow-circle-left"></i></a>&nbsp;
   <a href="https://github.com/daczarne/AppENCoR"><i class="fab fa-github"></i></a>&nbsp;
   <a href="https://stackoverflow.com/users/5908830/daniel?tab=profile"><i class="fab fa-stack-overflow"></i></a>&nbsp;
   <a href="https://twitter.com/daczarne"><i class="fab fa-twitter"></i></a>&nbsp;
   <a href="https://www.linkedin.com/in/danielczarnievicz/"><i class="fab fa-linkedin"></i></a>&nbsp;
   <a href="https://danielczarnievicz.netlify.app/portfolio/"><i class="fas fa-home"></i></a>&nbsp;'
)

# UI ----------------------------------------------------------------------
ui <- shiny::tagList(

   shiny::navbarPage(

      collapsible = TRUE,

      theme = shinythemes::shinytheme(theme = "flatly"),

      title = "EUTIC",

      # Tab: Hogares ------------------------------------------------------------

      shiny::tabPanel(

         # Navbar
         icon = shiny::icon("home"),
         title = "Hogares",

         # Side bar
         shiny::sidebarPanel(
            shiny::h4(title),
            # radio buttons homes
            infoSelectorHomesUI(),
            # Plot-by selector
            selectPlotByUI(id = "hogares_graficar_segun"),
            # Locality selector
            selectLocalityUI(id = "hogares_localidad", var = eutic[["localidad"]]),
            # Income level selector
            selectIncomeLevelUI(id = "hogares_ingresos", var = eutic[["ingresos_total"]]),
            # Notes
            shiny::p(data_source),
            shiny::p(nota_quintiles),
            shiny::p(links),
            shiny::icon(" ")
         ),

         # Main panel
         shiny::mainPanel(
            # Section 1
            questionDivUI(id = "hogares_texto_pregunta_uno"),
            plotly::plotlyOutput(outputId = "hogares_plot_uno"),
            # Section 2
            questionDivUI(id = "hogares_texto_pregunta_dos"),
            plotly::plotlyOutput(outputId = "hogares_plot_dos")
         )

      ),

      # Tab: Personas -----------------------------------------------------------

      shiny::tabPanel(

         # Navbar
         icon = shiny::icon("user"),
         title = "Personas",

         # Side panel
         shiny::sidebarPanel(
            shiny::h4(title),
            # Info selector
            infoSelectorPeopleUI(),
            # Plot-by selector
            selectPlotByUI(id = "personas_graficar_segun"),
            # Locality selector
            selectLocalityUI(id = "localidad_personas", var = eutic[["localidad"]]),
            # Income level selector
            selectIncomeLevelUI(id = "ingresos_personas", var = eutic[["ingresos_total"]]),
            # Age selector
            selectAgeUI(id = "edad_personas", var = eutic[["edad"]]),
            # Gender selector
            selectGenderUI(id = "sexo_personas",  var = eutic[["sexo"]]),
            # Educational level selector
            selectEducLevelUI(id = "nivel_educ_personas", var = eutic[["nivel_educ"]]),
            # Notes
            shiny::p(data_source),
            shiny::p(nota_quintiles),
            shiny::p(links)
         ),

         # Main panel
         shiny::mainPanel(
            # Section 1
            questionDivUI(id = "personas_texto_pregunta_uno"),
            plotly::plotlyOutput(outputId = "personas_plot_uno"),
            # Section 2
            questionDivUI(id = "personas_texto_pregunta_dos"),
            plotly::plotlyOutput(outputId = "personas_plot_dos"),
            # Section 3
            questionDivUI(id = "personas_texto_pregunta_tres"),
            plotly::plotlyOutput(outputId = "personas_plot_tres")
         )

      ),

      # Tab: Internet -----------------------------------------------------------

      shiny::tabPanel(

         # Navbar
         icon = shiny::icon("wifi"),
         title = "Internet",

         # Side bar
         shiny::sidebarPanel(
            shiny::h4(title),
            # Info selector
            infoSelectorInternetUI(),
            # Plot-by selector
            selectPlotByUI(id = "internet_graficar_segun"),
            # Locality selector
            selectLocalityUI(id = "localidad_internet", var = eutic[["localidad"]]),
            # Income level selector
            selectIncomeLevelUI(id = "ingresos_internet", var = eutic[["ingresos_total"]]),
            # Age selector
            selectAgeUI(id = "edad_internet", var = eutic[["edad"]]),
            # Gender selector
            selectGenderUI(id = "sexo_internet",  var = eutic[["sexo"]]),
            # Educational level selector
            selectEducLevelUI(id = "nivel_educ_internet", var = eutic[["nivel_educ"]]),
            # Notes
            shiny::p(data_source),
            shiny::p(nota_quintiles),
            shiny::p(links)

         ),

         # Main panel
         shiny::mainPanel(
            # Section 1
            questionDivUI(id = "internet_texto_pregunta_uno"),
            plotly::plotlyOutput(outputId = "internet_plot_uno"),
            # Section 2
            shiny::conditionalPanel(
               condition = "input.internet != '_buscar_info_' & input.internet != '_estudio_'",
               questionDivUI(id = "internet_texto_pregunta_dos"),
               plotly::plotlyOutput(outputId = "internet_plot_dos")
            ),
            # Section 3
            shiny::conditionalPanel(
               condition = "input.internet == '_comms_'",
               questionDivUI(id = "internet_texto_pregunta_tres"),
               plotly::plotlyOutput(outputId = "internet_plot_tres")
            )
         )

      )

   )

)

# Server ------------------------------------------------------------------
server <- function(input, output) {


   # Server functions --------------------------------------------------------

   # Filters for Hogares tab
   filterHogaresTab <- function(.data) {
      .data %>%
         dplyr::filter(
            localidad %in% input[["hogares_localidad"]],
            ingresos_total %in% input[["hogares_ingresos"]]
         )
   }

   # Filters for Personas tab
   filterPersonasTab <- function(.data) {
      .data %>%
         dplyr::filter(
            localidad %in% input[["localidad_personas"]],
            ingresos_total %in% input[["ingresos_personas"]],
            dplyr::between(edad, input[["edad_personas"]][1L], input[["edad_personas"]][2L]),
            sexo %in% input[["sexo_personas"]],
            nivel_educ %in% input[["nivel_educ_personas"]]
         )
   }

   # Filters for Internet tab
   filterInternetTab <- function(.data) {
      .data %>%
         dplyr::filter(
            localidad %in% input[["localidad_internet"]],
            ingresos_total %in% input[["ingresos_internet"]],
            dplyr::between(edad, input[["edad_internet"]][1L], input[["edad_internet"]][2L]),
            sexo %in% input[["sexo_internet"]],
            nivel_educ %in% input[["nivel_educ_internet"]]
         )
   }

   # Tab: Hogares ------------------------------------------------------------

   # Section 1
   output[["hogares_texto_pregunta_uno"]] <- shiny::renderText({
      texto_pregunta <- dplyr::case_when(
         input[["hogares"]] == "tiene_desktop" ~ "¿Tiene computadora tradicional o de escritorio en el hogar?",
         input[["hogares"]] == "tiene_laptop" ~ "¿Tiene laptop, netbook o similar en el hogar?",
         input[["hogares"]] == "tiene_tablet" ~ "¿Tiene tablet en el hogar?",
         input[["hogares"]] == "tiene_internet" ~ "¿Tiene su hogar conexión a Internet?"
      )
      glue::glue("Pregunta: {texto_pregunta}")
   })

   output[["hogares_plot_uno"]] <- plotly::renderPlotly({
      eutic %>%
         filterHogaresTab() %>%
         plotly_hogares_tienen(
            group_var_1 = input[["hogares_graficar_segun"]],
            group_var_2 = input[["hogares"]]
         )
   })

   # Section 2
   output[["hogares_texto_pregunta_dos"]] <- shiny::renderText({
      texto_pregunta <- dplyr::case_when(
         input[["hogares"]] == "tiene_desktop" ~ "¿Cuántas? (para los hogares que tienen)",
         input[["hogares"]] == "tiene_laptop" ~ "¿Cuántas? (para los hogares que tienen)",
         input[["hogares"]] == "tiene_tablet" ~ "¿Cuántas? (para los hogares que tienen)",
         input[["hogares"]] == "tiene_internet" ~ "¿Qué tipos de conexión a Internet tiene en su hogar? (para los hogares que tienen)"
      )
      glue::glue("Pregunta: {texto_pregunta}")
   })

   output[["hogares_plot_dos"]] <- plotly::renderPlotly({

      if (input[["hogares"]] == "tiene_internet") {

         eutic %>%
            filterHogaresTab() %>%
            genera_data_tipo_conexion(
               group_by_var = input[["hogares_graficar_segun"]]
            ) %>%
            plotly_tipo_conexion(
               group_by_var = input[["hogares_graficar_segun"]]
            )

      } else {

         eutic %>%
            filterHogaresTab() %>%
            plotly_hogares_cantidad_dispositivos(
               group_var_1 = input[["hogares_graficar_segun"]],
               group_var_2 = input[["hogares"]]
            )

      }

   })

   # Tab: Personas -----------------------------------------------------------

   # Section 1
   output[["personas_texto_pregunta_uno"]] <- shiny::renderText({
      texto_pregunta <- dplyr::case_when(
         input[["personas"]] == "uso_celular" ~ "¿Utilizó un celular en los últimos 3 meses?",
         input[["personas"]] == "uso_internet" ~ "¿Utilizó alguna vez Internet?",
      )
      glue::glue("Pregunta: {texto_pregunta}")
   })

   output[["personas_plot_uno"]] <- plotly::renderPlotly({

      if (input[["personas"]] == "uso_internet") {

         eutic %>%
            filterPersonasTab() %>%
            plotly_personas_uso_tic(
               group_var_1 = input[["personas_graficar_segun"]],
               group_var_2 = "uso_internet"
            )

      } else {

         eutic %>%
            filterPersonasTab() %>%
            plotly_personas_uso_tic(
               group_var_1 = input$personas_graficar_segun,
               group_var_2 = input$personas
            )

      }

   })

   # Section 2
   output[["personas_texto_pregunta_dos"]] <- shiny::renderText({
      texto_pregunta <- dplyr::case_when(
         input[["personas"]] == "uso_celular" ~ "¿Con qué frecuencia utilizó Internet en el celular en los últimos 3 meses? (para quienes lo utilizaron)",
         input[["personas"]] == "uso_internet" ~ "¿Con qué frecuencia utilizó Internet en los últimos 3 meses? (para quienes lo utilizaron)"
      )
      glue::glue("Pregunta: {texto_pregunta}")
   })

   output[["personas_plot_dos"]] <- plotly::renderPlotly({

      if (input[["personas"]] == "uso_celular") {

         eutic %>%
            dplyr::filter(
               uso_internet == "Sí"
            ) %>%
            base::droplevels() %>%
            filterPersonasTab() %>%
            plotly_personas_uso_tic(
               group_var_1 = input[["personas_graficar_segun"]],
               group_var_2 = "frecuencia_uso_internet_celular"
            )

      } else if (input[["personas"]] == "uso_internet") {

         eutic %>%
            dplyr::filter(
               uso_internet == "Sí"
            ) %>%
            base::droplevels() %>%
            filterPersonasTab() %>%
            plotly_personas_uso_tic(
               group_var_1 = input[["personas_graficar_segun"]],
               group_var_2 = "frecuencia_uso_internet"
            )

      }

   })

   # Section 3
   output[["personas_texto_pregunta_tres"]] <- shiny::renderText({
      texto_pregunta <- dplyr::case_when(
         input[["personas"]] == "uso_celular" ~ "En los últimos 3 meses, ¿qué actividades realizó con el celular? (para quienes lo utilizaron)",
         input[["personas"]] == "uso_internet" ~ "En los últimos 3 meses, ¿con qué finalidades utilizó Internet? (para quienes lo utilizaron)"
      )
      glue::glue("Pregunta: {texto_pregunta}")
   })

   output[["personas_plot_tres"]] <- plotly::renderPlotly({

      if (input[["personas"]] == "uso_celular") {

         eutic %>%
            filterPersonasTab() %>%
            generar_data_usos_celular(
               group_by_var = input[["personas_graficar_segun"]]
            ) %>%
            plotly_personas_usos_tics(
               group_by_var = input[["personas_graficar_segun"]]
            )

      } else if (input[["personas"]] == "uso_internet") {

         eutic %>%
            filterPersonasTab() %>%
            generar_data_usos_internet(
               group_by_var = input[["personas_graficar_segun"]]
            ) %>%
            plotly_personas_usos_tics(
               group_by_var = input[["personas_graficar_segun"]]
            )

      }

   })

   # Tab: Internet -----------------------------------------------------------

   # Section 1
   output[["internet_texto_pregunta_uno"]] <- shiny::renderText({
      texto_pregunta <- dplyr::case_when(
         input[["internet"]] == "_buscar_info_" ~ "En los últimos 3 meses, ¿buscó en Internet información sobre...?",
         input[["internet"]] == "_estudio_" ~ "En los últimos 3 meses, ¿qué actividades realizó en Internet vinculadas al estudio?",
         input[["internet"]] == "_trabajo_" ~ "En los últimos 3 meses, ¿qué actividades realizó en Internet relacionadas con el trabajo?",
         input[["internet"]] == "_comms_" ~ "En los últimos 3 meses, ¿qué actividades realizó en Internet relacionadas con la comunicación?",
         input[["internet"]] == "_ocio_" ~ "En los últimos 3 meses, ¿qué actividades realizó en Internet relacionadas con el entretenimiento?",
         input[["internet"]] == "_comercio_" ~ "En los últimos 3 meses, ¿qué actividades vinculadas a las transacciones y/o comercio electrónico realizó a través de Internet?"
      )
      glue::glue("Pregunta: {texto_pregunta}")
   })

   output[["internet_plot_uno"]] <- plotly::renderPlotly({

      eutic %>%
         filterInternetTab() %>%
         generar_data_usos_internet_por_tipo_de_uso(
            group_by_var = input[["internet_graficar_segun"]],
            var_pattern = input[["internet"]],
            var_names = base::names(eutic)
         ) %>%
         plotly_personas_usos_tics(
            group_by_var = input[["internet_graficar_segun"]]
         )

   })

   # Section 2
   output[["internet_texto_pregunta_dos"]] <- shiny::renderText({
      texto_pregunta <- dplyr::case_when(
         input[["internet"]] == "_trabajo_" ~ "En caso de no poder acceder a Internet en una jornada laboral típica, ¿podría usted desempeñar sus tareas con normalidad? (para quienes sí lo utilizan)",
         input[["internet"]] == "_comms_" ~ "¿Con qué frecuencia participa o chequea alguna red social en Internet? (para quienes las utilizan)",
         input[["internet"]] == "_ocio_" ~ "¿En el último mes vio las siguientes señales a través de Internet?",
         input[["internet"]] == "_comercio_" ~ "¿Ud. cuenta con acceso a alguno de los siguientes medios de pago electrónico?"
      )
      glue::glue("Pregunta: {texto_pregunta}")
   })

   output[["internet_plot_dos"]] <- plotly::renderPlotly({

      if (input[["internet"]] == "_trabajo_") {

         eutic %>%
            dplyr::filter(
               usos_internet_laboral == "Sí"
            ) %>%
            base::droplevels() %>%
            filterInternetTab() %>%
            plotly_personas_uso_tic(
               group_var_1 = input[["internet_graficar_segun"]],
               group_var_2 = "usos_internet_laboral_dificultad"
            )

      } else if (input[["internet"]] == "_comms_") {

         eutic %>%
            dplyr::filter(
               usos_internet_comms_redes == "Sí"
            ) %>%
            base::droplevels() %>%
            filterInternetTab() %>%
            plotly_personas_uso_tic(
               group_var_1 = input[["internet_graficar_segun"]],
               group_var_2 = "frecuencia_uso_redes_sociales"
            )

      } else if (input[["internet"]] == "_ocio_") {

         eutic %>%
            filterInternetTab() %>%
            generar_data_usos_internet_por_tipo_de_uso(
               group_by_var = input[["internet_graficar_segun"]],
               var_pattern = "_canales_",
               var_names = base::names(eutic)
            ) %>%
            plotly_personas_usos_tics(
               group_by_var = input[["internet_graficar_segun"]]
            )

      } else if (input[["internet"]] == "_comercio_") {

         eutic %>%
            filterInternetTab() %>%
            generar_data_usos_internet_por_tipo_de_uso(
               group_by_var = input[["internet_graficar_segun"]],
               var_pattern = "_medios_",
               var_names = base::names(eutic)
            ) %>%
            plotly_personas_usos_tics(
               group_by_var = input[["internet_graficar_segun"]]
            )

      }

   })

   # Section 3
   output[["internet_texto_pregunta_tres"]] <- shiny::renderText({
      texto_pregunta <- dplyr::case_when(
         input[["internet"]] == "_comms_" ~ "¿Tiene una cuenta o participa en alguna de las siguientes redes sociales?"
      )
      glue::glue("Pregunta: {texto_pregunta}")
   })

   output[["internet_plot_tres"]] <- plotly::renderPlotly({

      if (input[["internet"]] == "_comms_") {

         eutic %>%
            filterInternetTab() %>%
            generar_data_usos_internet_por_tipo_de_uso(
               group_by_var = input[["internet_graficar_segun"]],
               var_pattern = "_redes_",
               var_names = base::names(eutic)
            ) %>%
            plotly_personas_usos_tics(
               group_by_var = input[["internet_graficar_segun"]]
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