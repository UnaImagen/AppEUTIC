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
         n = base::sum(peso_hogar, na.rm = TRUE),
         .groups = "drop_last"
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
      plotlyConfig()
}