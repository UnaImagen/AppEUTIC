selectAgeUI <- function(id, var) {
   shiny::sliderInput(
      inputId = id,
      label = "Edad:",
      min = base::min(var, na.rm = TRUE),
      max = base::max(var, na.rm = TRUE),
      value = base::c(
         base::min(var, na.rm = TRUE),
         base::max(var, na.rm = TRUE)
      ),
      step = 1,
      animate = TRUE
   )
}