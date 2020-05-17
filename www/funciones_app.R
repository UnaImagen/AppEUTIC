#===========================#
#### FUNCIONES APP EUTIC ####
#===========================#

## Genera plot de hogares que tienen o no una TIC seleccionada
plotly_hogares_tienen <- function(.data, group_var_1, group_var_2) {

   xaxis_title <- dplyr::case_when(
      group_var_1 == "localidad" ~ "Localidad",
      group_var_1 == "ingresos_total" ~ "Nivel de ingresos"
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
            bgcolor = "#E2E2E2",
            orientation = "h",
            yanchor = "bottom",
            xanchor = "left",
            y = -.30
         ),
         hovermode = "x"
      ) %>%
      plotly::config(
         locale = "es",
         displayModeBar = TRUE,
         displaylogo = TRUE,
         modeBarButtonsToRemove = base::c(
            "zoom2d",
            "zoomIn2d",
            "zoomOut2d",
            "select2d",
            "drawclosedpath",
            "lasso2d",
            "pan2d",
            "drawrect",
            "autoScale2d",
            "hoverClosestCartesian",
            "hoverCompareCartesian",
            "toggleSpikelines"
         )
      )

}

## Genera plot de la cantidad de dispositivos en el hogar
plotly_hogares_cantidad_dispositivos <- function(.data, group_var_1, group_var_2, filter_var = group_var_2) {

   xaxis_title <- dplyr::case_when(
      group_var_1 == "localidad" ~ "Localidad",
      group_var_1 == "ingresos_total" ~ "Nivel de ingresos"
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
            bgcolor = "#E2E2E2",
            orientation = "h",
            yanchor = "bottom",
            xanchor = "left",
            y = -.30
         ),
         hovermode = "x"
      ) %>%
      plotly::config(
         locale = "es",
         displayModeBar = TRUE,
         displaylogo = TRUE,
         modeBarButtonsToRemove = base::c(
            "zoom2d",
            "zoomIn2d",
            "zoomOut2d",
            "select2d",
            "drawclosedpath",
            "lasso2d",
            "pan2d",
            "drawrect",
            "autoScale2d",
            "hoverClosestCartesian",
            "hoverCompareCartesian",
            "toggleSpikelines"
         )
      )

}

## Genera data de tipo de conexión
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

## Genera plot de tipo de conexión en el hogar
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
         displayModeBar = TRUE,
         displaylogo = TRUE,
         modeBarButtonsToRemove = base::c(
            "zoom2d",
            "zoomIn2d",
            "zoomOut2d",
            "select2d",
            "drawclosedpath",
            "lasso2d",
            "pan2d",
            "drawrect",
            "autoScale2d",
            "hoverClosestCartesian",
            "hoverCompareCartesian",
            "toggleSpikelines"
         )
      )

}

## Genera plot de uso de tics por personas
plotly_personas_uso_tic <- function(.data, group_var_1, group_var_2, plotly_legend_y = -0.60) {

   colors <- dplyr::if_else(group_var_2 %in% base::c("frecuencia_uso_internet", "frecuencia_uso_internet_celular"), "Accent", "Paired")

   xaxis_title <- dplyr::case_when(
      group_var_1 == "localidad" ~ "Localidad",
      group_var_1 == "ingresos_total" ~ "Nivel del ingresos (del hogar)",
      group_var_1 == "sexo" ~ "Sexo",
      group_var_1 == "nivel_educ" ~ "Nivel educativo"
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
         displayModeBar = TRUE,
         displaylogo = TRUE,
         modeBarButtonsToRemove = base::c(
            "zoom2d",
            "zoomIn2d",
            "zoomOut2d",
            "select2d",
            "drawclosedpath",
            "lasso2d",
            "pan2d",
            "drawrect",
            "autoScale2d",
            "hoverClosestCartesian",
            "hoverCompareCartesian",
            "toggleSpikelines"
         )
      )

}

## Genera plot de usos de celulares
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

## Genera data de usos de internet
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

## Genera plot de usos de tics por personas
plotly_personas_usos_tics <- function(.data, group_by_var, plotly_legend_y = -0.6) {

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
         displayModeBar = TRUE,
         displaylogo = TRUE,
         modeBarButtonsToRemove = base::c(
            "zoom2d",
            "zoomIn2d",
            "zoomOut2d",
            "select2d",
            "drawclosedpath",
            "lasso2d",
            "pan2d",
            "drawrect",
            "autoScale2d",
            "hoverClosestCartesian",
            "hoverCompareCartesian",
            "toggleSpikelines"
         )
      )

}

## Genera data de usos de internet por tipo de uso
generar_data_usos_internet_por_tipo_de_uso <- function(.data, group_by_var, var_pattern, var_names) {

   var_list <- stringr::str_subset(
      string = var_names,
      pattern = var_pattern
   )

   aux_data <- .data %>%
      dplyr::mutate(
         group_by_var = !!rlang::sym(group_by_var),
         pattern_var = !!rlang::sym(var_list[1])
      ) %>%
      dplyr::group_by(
         group_by_var,
         pattern_var
      ) %>%
      dplyr::summarise(
         n = base::sum(peso_hogar, na.rm = TRUE)
      ) %>%
      dplyr::mutate(
         proporcion = n / base::sum(n, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(
         pattern_var == "Sí"
      ) %>%
      dplyr::transmute(
         group_by_var,
         tipo_uso = stringr::str_replace(
            string = var_list[1],
            pattern = base::paste0("usos_internet", var_pattern),
            replacement = ""
         ),
         proporcion
      )

   for (i in 2:base::length(var_list)) {

      aux_data %<>%
         dplyr::bind_rows(
            .data %>%
               dplyr::mutate(
                  group_by_var = !!rlang::sym(group_by_var),
                  pattern_var = !!rlang::sym(var_list[i])
               ) %>%
               dplyr::group_by(
                  group_by_var,
                  pattern_var
               ) %>%
               dplyr::summarise(
                  n = base::sum(peso_hogar, na.rm = TRUE)
               ) %>%
               dplyr::mutate(
                  proporcion = n / base::sum(n, na.rm = TRUE)
               ) %>%
               dplyr::ungroup() %>%
               dplyr::filter(
                  pattern_var == "Sí"
               ) %>%
               dplyr::transmute(
                  group_by_var,
                  tipo_uso = stringr::str_replace(
                     string = var_list[i],
                     pattern = base::paste0("usos_internet", var_pattern),
                     replacement = ""
                  ),
                  proporcion
               )
         )

   }

   aux_data %>%
      dplyr::mutate(
         tipo_uso = dplyr::case_when(
            tipo_uso == "bienes_y_servicios" ~ "Bienes y servicios",
            tipo_uso == "servicios_medicos" ~ "Servicios médicos",
            tipo_uso == "salud" ~ "Salud en general",
            tipo_uso == "estado" ~ "Estado o gobierno",
            tipo_uso == "wikis" ~ "Información en general",
            tipo_uso == "informacion" ~ "Buscar información",
            tipo_uso == "curso_a_distancia" ~ "Curso a distancia",
            tipo_uso == "interactuar_centro_de_estudio" ~ "Inscripciones",
            tipo_uso == "material_docente" ~ "Descargó material docente",
            tipo_uso == "buscar_trabajo" ~ "Buscó empleo",
            tipo_uso == "wfh" ~ "Teletrabajo",
            tipo_uso == "email_laboral" ~ "Responder correo fuera de horario",
            tipo_uso == "email_personal" ~ "Enviar/Recivir correos",
            tipo_uso == "redes" ~ "Utilizar redes sociales",
            tipo_uso == "chat" ~ "Chatear",
            tipo_uso == "llamadas" ~ "Llamadas",
            tipo_uso == "date_app" ~ "Apps para conocer gente",
            tipo_uso == "radio" ~ "Escuchar radio",
            tipo_uso == "tv" ~ "Ver TV",
            tipo_uso == "streaming" ~ "Streaming",
            tipo_uso == "gaming" ~ "Gaming",
            tipo_uso == "software" ~ "Descargar software",
            tipo_uso == "leer" ~ "Leer",
            tipo_uso == "blogging" ~ "Blogging",
            tipo_uso == "web" ~ "Página web propia",
            tipo_uso == "storage" ~ "Almacenar archivos online",
            tipo_uso == "compra" ~ "Compras online",
            tipo_uso == "venta" ~ "Ventas online",
            tipo_uso == "banking" ~ "Banca online",
            tipo_uso == "booking" ~ "Reservas online",
            tipo_uso == "youtube" ~ "YouTube",
            tipo_uso == "netflix" ~ "Netflix",
            tipo_uso == "veratv" ~ "VeraTV",
            tipo_uso == "aire" ~ "Canales de aire",
            tipo_uso == "cable" ~ "Canales de cable",
            tipo_uso == "otro" ~ "Otros",
            tipo_uso == "tarjeta_internacional" ~ "Tj. de crédito internacional",
            tipo_uso == "tarjeta_nacional" ~ "Tj. de crédito nacional",
            tipo_uso == "tarjeta_prepaga" ~ "Tj. prepaga internacional",
            tipo_uso == "tarjeta_debito" ~ "Tj. de débito",
            tipo_uso == "paypal" ~ "Paypal",
            tipo_uso == "antel" ~ "Antel Bits",
            tipo_uso == "otros" ~ "Otros",
            tipo_uso == "facebook" ~ "Facebook",
            tipo_uso == "twitter" ~ "Twitter",
            tipo_uso == "google" ~ "Google+",
            tipo_uso == "instagram" ~ "Instagram",
            tipo_uso == "linkedin" ~ "LinkedIn",
            tipo_uso == "otras" ~ "Otras redes",
            TRUE ~ tipo_uso
         ),
         tipo_uso = forcats::as_factor(tipo_uso)
      )

}

#===============#
#### THE END ####
#===============#