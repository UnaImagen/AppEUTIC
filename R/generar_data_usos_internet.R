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