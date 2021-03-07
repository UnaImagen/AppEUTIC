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