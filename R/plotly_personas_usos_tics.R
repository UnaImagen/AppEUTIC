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
      plotlyConfig()
}