#=============#
#### EUTIC ####
#=============#

library(magrittr)

# Carga datos -------------------------------------------------------------
eutic <- haven::read_sav(
   file = here::here("BASE HOGARES Y PERSONAS EUTIC 2016.sav")
) %>%
   dplyr::rename_all(
      .funs = tolower
   )


# Construye objeto para el App --------------------------------------------
eutic %<>%
   dplyr::transmute(
      nper = dplyr::row_number(),
      peso_persona = base::as.integer(peso.per),
      peso_hogar = base::as.integer(peso.hog),
      sexo = forcats::as_factor(p12),
      edad = base::as.integer(p13),
      localidad = forcats::as_factor(dpto),
      localidad = forcats::fct_collapse(
         .f = localidad,
         Montevideo = "Montevideo",
         other_level = "Interior"
      ),
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
      ingresos = forcats::as_factor(quintil_total),
      ingresos = forcats::fct_relabel(
         .f = ingresos,
         .fun = ~stringr::str_c("Q", .)
      ),

      ## Tenencia en el hogar: desktop
      tiene_desktop = forcats::as_factor(h6_1),
      tiene_desktop = forcats::fct_collapse(
         .f = tiene_desktop,
         No = base::c("No", "0")
      ),
      cantidad_desktop = base::as.integer(h6_1_1),

      ## Tenencia en el hogar: laptop
      tiene_laptop = forcats::as_factor(h6_2),
      tiene_laptop = forcats::fct_collapse(
         .f = tiene_laptop,
         No = base::c("No", "0")
      ),
      cantidad_laptop = base::as.integer(h6_2_1),

      ## Tenencia en el hogar: tablet
      tiene_tablet = dplyr::if_else(h6_3 == 1, "Sí", "No"),
      tiene_tablet = forcats::as_factor(tiene_tablet),
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

readr::write_rds(x = eutic, path = "eutic.rds")

detect_desktop <- "Sí"
detect_laptop <- "Sí"
detect_tablet <- "Sí"
detect_condition <- stringr::str_c(detect_desktop, detect_laptop, detect_tablet, sep = ":")

x %>%
   dplyr::transmute(
      localidad,
      ingresos,
      tiene = forcats::fct_cross(tiene_desktop, tiene_laptop, tiene_tablet, sep = ":"),
      tiene = dplyr::if_else(stringr::str_detect(tiene, "Sí"), TRUE, FALSE),
      peso_hogar
   ) %>%
   dplyr::group_by(
      # localidad,
      tiene
   ) %>%
   dplyr::summarise(
      n = base::sum(peso_hogar)
   ) %>%
   dplyr::mutate(
      prop = n / base::sum(n)
   )

#===============#
#### THE END ####
#===============#