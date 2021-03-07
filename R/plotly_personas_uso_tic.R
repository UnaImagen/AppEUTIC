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