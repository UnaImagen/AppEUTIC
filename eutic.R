#=============#
#### EUTIC ####
#=============#

library(magrittr)
library(tidyverse)

# Carga datos -------------------------------------------------------------
eutic <- haven::read_sav(
   file = here::here("BASE HOGARES Y PERSONAS EUTIC 2016.sav")
) %>%
   dplyr::rename_all(
      .funs = tolower
   )


# Construye objeto para el App --------------------------------------------
eutic %>%
   # filter(
   #    p23_1 == 3
   # ) %>%
   dplyr::transmute(
      nper = dplyr::row_number(),
      peso_persona = base::as.integer(peso.per),
      peso_hogar = base::as.integer(peso.hog),
      sexo = forcats::as_factor(p12),
      edad = base::as.integer(p13),
      depto = forcats::as_factor(dpto),
      nivel_educ = forcats::as_factor(edudesag_eutic),
      nivel_educ = forcats::fct_relevel(
         .f = nivel_educ,
         "Sin instrucción o Primaria o menos",
         "Secundaria",
         "Terciario"
      ),
      nivel_educ = forcats::fct_recode(
         .f = nivel_educ,
         "Primaria o menos" = "Sin instrucción o Primaria o menos"
      ),

      ## Tenencia en el hogar: desktop
      tiene_desktop = forcats::as_factor(h6_1),
      cantidad_desktop = base::as.integer(h6_1_1),

      ## Tenencia en el hogar: laptop
      tiene_laptop = forcats::as_factor(h6_2),
      cantidad_laptop = base::as.integer(h6_2_1),

      ## Tenencia en el hogar: tablet
      tiene_tablet = forcats::as_factor(h6_3),
      cantidad_tablet = base::as.integer(h6_3_1),

      ## Conexión a internet
      conexion_internet_en_el_hogar = forcats::as_factor(h9),

      ## Uso de celular
      uso_celular_comun = forcats::as_factor(p23),
      uso_smart_phone = forcats::as_factor(p23_1),
      uso_celular = dplyr::case_when(
         uso_celular_comun == "No" & uso_smart_phone == "0" ~ "No utiliza",
         (uso_celular_comun == "Sí" & uso_smart_phone == "No") | (uso_celular_comun == "Sí" & uso_smart_phone == "No sabe") ~ "Utiliza celular",
         uso_celular_comun == "Sí" & uso_smart_phone == "Sí" ~ "Utiliza Smart Phone",
         TRUE ~ NA_character_
      ),

      ## Uso de tablet
      uso_tablet = forcats::as_factor(p26),

      ## Uso de PC
      uso_pc = forcats::as_factor(p27),

      ## Uso Internet
      uso_internet = forcats::as_factor(p37),
      frecuencia_uso_internet = forcats::as_factor(p40),
      frecuencia_uso_internet = forcats::fct_recode(
         .f = frecuencia_uso_internet,
         "Entre una y tres veces al día" = "Diariamente,entre una y tres veces al dia",
         "Cuatro o más veces al día" = "Diariamente,cuatro o mas veces al dia",
         "Al menos una vez a la semana" = "Al menos una vez a la semana pero no todos los días",
         "Al menos una vez al mes" = "Al menos una vez al mes pero no todas las semanas",
         "No recuerda" = "No recuerda o muy irregularmente (no leer)",
         "No utiliza" = "0"
      ),
      frecuencia_uso_internet = forcats::fct_relevel(
         .f = frecuencia_uso_internet,
         "Cuatro o más veces al día",
         "Entre una y tres veces al día",
         "Al menos una vez a la semana",
         "Al menos una vez al mes",
         "Menos de una vez al mes",
         "No utiliza",
         "No recuerda"
      )

   )

eutic %>%
   select(
      id,
      nper,
      pernro,
      edudesag,
      edudesag_eutic
   ) %>%
   tail()
max(eutic$id)

xtabs(~p23 + p23_1, data = eutic, addNA = TRUE)
table(eutic$p40, useNA = "always")
levels(.Last.value$nivel_educ)
table(eutic$edudesag)
x %>%
   group_by(
      uso_smart_phone
   ) %>%
   tally() %>%
   plotly::plot_ly(
   x = ~uso_smart_phone,
   y = ~n,
   type = "bar"
)

#===============#
#### THE END ####
#===============#