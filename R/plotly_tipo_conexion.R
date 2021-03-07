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