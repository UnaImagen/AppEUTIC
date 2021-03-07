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