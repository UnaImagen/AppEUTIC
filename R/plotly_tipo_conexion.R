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
            "%{y:0.2%}",
            "<extra></extra>"
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
         hovermode = "x"
      ) %>%
      plotlyLayout() %>%
      plotlyLegend(y = -0.40) %>%
      plotlyConfig()
}