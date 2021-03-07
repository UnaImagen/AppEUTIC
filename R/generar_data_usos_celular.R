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