plotlyConfig <- function(.p) {
   .p %>%
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