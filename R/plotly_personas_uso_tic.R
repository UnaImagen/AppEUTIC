## Genera plot de uso de tics por personas
plotly_personas_uso_tic <- function(.data, group_var_1, group_var_2) {

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
         n = base::sum(peso_persona, na.rm = TRUE),
         .groups = "drop_last"
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
         hovermode = "x"
      ) %>%
      plotlyLayout() %>%
      plotlyLegend(y = -0.60) %>%
      plotlyConfig()
}