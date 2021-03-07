plotlyLayout <- function(.p) {
   .p %>%
      plotly::layout(
         paper_bgcolor = "#f5f5f5",
         plot_bgcolor = "#f5f5f5"
      )
}