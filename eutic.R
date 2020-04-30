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

# EUTIC -------------------------------------------------------------------
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
      ingresos_total = forcats::as_factor(quintil_total),
      ingresos_total = forcats::fct_relabel(
         .f = ingresos_total,
         .fun = ~stringr::str_c("Q", .)
      ),
      ingresos_regional = forcats::as_factor(quintil_regional),
      ingresos_regional = forcats::fct_relabel(
         .f = ingresos_regional,
         .fun = ~stringr::str_c("Q", .)
      ),

      ## Tenencia en el hogar: desktop
      tiene_desktop = base::as.integer(h6_1),
      tiene_desktop = dplyr::if_else(tiene_desktop == 1L, "Sí", "No"),
      cantidad_desktop = dplyr::if_else(tiene_desktop == "No", 0L, base::as.integer(h6_1_1)),
      tiene_desktop = dplyr::if_else(cantidad_desktop == 0L, "No", tiene_desktop),
      tiene_desktop = forcats::as_factor(tiene_desktop),
      cantidad_desktop = forcats::as_factor(cantidad_desktop),
      cantidad_desktop = forcats::fct_lump_n(
         f = cantidad_desktop,
         n = 3,
         w = peso_hogar,
         other_level = "3 o más"
      ),

      ## Tenencia en el hogar: laptop
      tiene_laptop = base::as.integer(h6_2),
      tiene_laptop = dplyr::if_else(tiene_laptop == 1L, "Sí", "No"),
      cantidad_laptop = dplyr::if_else(tiene_laptop == "No", 0L, base::as.integer(h6_2_1)),
      tiene_laptop = dplyr::if_else(cantidad_laptop == 0L, "No", tiene_laptop),
      tiene_laptop = forcats::as_factor(tiene_laptop),
      cantidad_laptop = forcats::as_factor(cantidad_laptop),
      cantidad_laptop = forcats::fct_lump_n(
         f = cantidad_laptop,
         n = 3,
         w = peso_hogar,
         other_level = "3 o más"
      ),

      ## Tenencia en el hogar: tablet
      tiene_tablet = base::as.integer(h6_3),
      tiene_tablet = dplyr::if_else(tiene_tablet == 1L, "Sí", "No"),
      cantidad_tablet = dplyr::if_else(tiene_tablet == "No", 0L, base::as.integer(h6_3_1)),
      tiene_tablet = dplyr::if_else(cantidad_tablet == 0L, "No", tiene_tablet),
      tiene_tablet = forcats::as_factor(tiene_tablet),
      cantidad_tablet = forcats::as_factor(cantidad_tablet),
      cantidad_tablet = forcats::fct_lump_n(
         f = cantidad_tablet,
         n = 3,
         w = peso_hogar,
         other_level = "3 o más"
      ),

      ## Conexión a internet
      tiene_internet = forcats::as_factor(h9),
      banda_ancha_fija = dplyr::if_else(h10_1_2 == 1, "Sí", "No"),
      banda_ancha_fija = forcats::as_factor(banda_ancha_fija),
      banda_ancha_movil = dplyr::if_else(h10_1_4 == 1, "Sí", "No"),
      banda_ancha_movil = forcats::as_factor(banda_ancha_movil),
      otra_conexion = dplyr::if_else(h10_1_1 == 1 | h10_1_3 == 1 | h10_1_5 == 1 | h10_1_6 == 1, "Sí", "No"),
      otra_conexion = forcats::as_factor(otra_conexion),

      ## Uso de celular
      uso_celular_comun = forcats::as_factor(p23),
      uso_smart_phone = forcats::as_factor(p23_1),
      uso_celular = dplyr::case_when(
         uso_celular_comun == "No" & uso_smart_phone == "0" ~ "No utiliza",
         (uso_celular_comun == "Sí" & uso_smart_phone == "No") | (uso_celular_comun == "Sí" & uso_smart_phone == "No sabe") ~ "Utiliza celular",
         uso_celular_comun == "Sí" & uso_smart_phone == "Sí" ~ "Utiliza Smart Phone",
         TRUE ~ NA_character_
      ),
      usos_celular_llamadas = dplyr::if_else(p25_1_1 == 1 | p25_1_2 == 1, "Sí", "No"),
      usos_celular_llamadas = forcats::as_factor(usos_celular_llamadas),
      usos_celular_mensajes = dplyr::if_else(p25_1_3 == 1 | p25_1_15 == 1 | p25_1_16 == 1, "Sí", "No"),
      usos_celular_mensajes = forcats::as_factor(usos_celular_mensajes),
      usos_celular_multimedia_y_redes = dplyr::if_else(p25_1_5 == 1 | p25_1_6 == 1 | p25_1_8 == 1 | p25_1_9 == 1 | p25_1_10 == 1, "Sí", "No"),
      usos_celular_multimedia_y_redes = forcats::as_factor(usos_celular_multimedia_y_redes),
      usos_celular_informacion = dplyr::if_else(p25_1_7 == 1 | p25_1_12 == 1, "Sí", "No"),
      usos_celular_informacion = forcats::as_factor(usos_celular_informacion),
      usos_celular_compras = dplyr::if_else(p25_1_11 == 1 | p25_1_13 == 1 | p25_1_14 == 1, "Sí", "No"),
      usos_celular_compras = forcats::as_factor(usos_celular_compras),

      ## Uso de tablet
      uso_tablet = forcats::as_factor(p26),

      ## Uso de PC
      uso_pc = forcats::as_factor(p27),

      ## Uso Internet
      uso_internet = base::as.integer(p37),
      uso_internet = dplyr::if_else(p40 == 0, 2L, uso_internet),
      uso_internet = dplyr::if_else(uso_internet == 1L, "Sí", "No"),
      uso_internet = forcats::as_factor(uso_internet),

      ## Uso Internet - Frecuencia
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
      ),
      frecuencia_uso_internet_celular = forcats::as_factor(p40_1),
      frecuencia_uso_internet_celular = forcats::fct_recode(
         .f = frecuencia_uso_internet_celular,
         "Entre una y tres veces al día" = "Diariamente,entre una y tres veces al dia",
         "Cuatro o más veces al día" = "Diariamente,cuatro o mas veces al dia",
         "Al menos una vez a la semana" = "Al menos una vez a la semana pero no todos los días",
         "Al menos una vez al mes" = "Al menos una vez al mes pero no todas las semanas",
         "No recuerda" = "No recuerda o muy irregularmente (no leer)",
         "No utiliza" = "0"
      ),
      frecuencia_uso_internet_celular = forcats::fct_relevel(
         .f = frecuencia_uso_internet_celular,
         "Cuatro o más veces al día",
         "Entre una y tres veces al día",
         "Al menos una vez a la semana",
         "Al menos una vez al mes",
         "Menos de una vez al mes",
         "No utiliza",
         "No recuerda"
      ),

      ## Usos Internet - General
      usos_internet_comms = dplyr::if_else(p43_1_1 == 1, "Sí", "No"),
      usos_internet_comms = forcats::as_factor(usos_internet_comms),
      usos_internet_laboral = dplyr::if_else(p43_1_2 == 1, "Sí", "No"),
      usos_internet_laboral = forcats::as_factor(usos_internet_laboral),
      usos_internet_estudio = dplyr::if_else(p43_1_3 == 1, "Sí", "No"),
      usos_internet_estudio = forcats::as_factor(usos_internet_estudio),
      usos_internet_ocio = dplyr::if_else(p43_1_4 == 1, "Sí", "No"),
      usos_internet_ocio = forcats::as_factor(usos_internet_ocio),
      usos_internet_otro = dplyr::if_else(p43_1_5 == 1, "Sí", "No"),
      usos_internet_otro = forcats::as_factor(usos_internet_otro),

      ## Usos Internet - Buscar info
      usos_internet_buscar_info_bienes_y_servicios = dplyr::if_else(p44_1 == 1, "Sí", "No"),
      usos_internet_buscar_info_bienes_y_servicios = forcats::as_factor(usos_internet_buscar_info_bienes_y_servicios),
      usos_internet_buscar_info_servicios_medicos = dplyr::if_else(p44_2 == 1, "Sí", "No"),
      usos_internet_buscar_info_servicios_medicos = forcats::as_factor(usos_internet_buscar_info_servicios_medicos),
      usos_internet_buscar_info_salud = dplyr::if_else(p44_3 == 1, "Sí", "No"),
      usos_internet_buscar_info_salud = forcats::as_factor(usos_internet_buscar_info_salud),
      usos_internet_buscar_info_estado = dplyr::if_else(p44_4 == 1, "Sí", "No"),
      usos_internet_buscar_info_estado = forcats::as_factor(usos_internet_buscar_info_estado),
      usos_internet_buscar_info_wikis = dplyr::if_else(p44_5 == 1, "Sí", "No"),
      usos_internet_buscar_info_wikis = forcats::as_factor(usos_internet_buscar_info_wikis),

      ## Usos Internet - Estudio
      usos_internet_estudio_informacion = dplyr::if_else(p45_1 == 1, "Sí", "No"),
      usos_internet_estudio_informacion = forcats::as_factor(usos_internet_estudio_informacion),
      usos_internet_estudio_curso_a_distancia = dplyr::if_else(p45_2 == 1, "Sí", "No"),
      usos_internet_estudio_curso_a_distancia = forcats::as_factor(usos_internet_estudio_curso_a_distancia),
      usos_internet_estudio_interactuar_centro_de_estudio = dplyr::if_else(p45_3 == 1, "Sí", "No"),
      usos_internet_estudio_interactuar_centro_de_estudio = forcats::as_factor(usos_internet_estudio_interactuar_centro_de_estudio),
      usos_internet_estudio_material_docente = dplyr::if_else(p45_4 == 1, "Sí", "No"),
      usos_internet_estudio_material_docente = forcats::as_factor(usos_internet_estudio_material_docente),

      ## Usos Internet - Trabajo
      usos_internet_trabajo_informacion = dplyr::if_else(p46_1 == 1, "Sí", "No"),
      usos_internet_trabajo_informacion = forcats::as_factor(usos_internet_trabajo_informacion),
      usos_internet_trabajo_buscar_trabajo = dplyr::if_else(p46_2 == 1, "Sí", "No"),
      usos_internet_trabajo_buscar_trabajo = forcats::as_factor(usos_internet_trabajo_buscar_trabajo),
      usos_internet_trabajo_wfh = dplyr::if_else(p46_3 == 1, "Sí", "No"),
      usos_internet_trabajo_wfh = forcats::as_factor(usos_internet_trabajo_wfh),
      usos_internet_trabajo_email_laboral = dplyr::if_else(p46_4 == 1, "Sí", "No"),
      usos_internet_trabajo_email_laboral = forcats::as_factor(usos_internet_trabajo_email_laboral),

      ## Usos Internet - Comms
      usos_internet_comms_email_personal = dplyr::if_else(p47_1_1 == 1, "Sí", "No"),
      usos_internet_comms_email_personal = forcats::as_factor(usos_internet_comms_email_personal),
      usos_internet_comms_redes_sociales = dplyr::if_else(p47_1_2 == 1, "Sí", "No"),
      usos_internet_comms_redes_sociales = forcats::as_factor(usos_internet_comms_redes_sociales),
      usos_internet_comms_chat = dplyr::if_else(p47_1_3 == 1, "Sí", "No"),
      usos_internet_comms_chat = forcats::as_factor(usos_internet_comms_chat),
      usos_internet_comms_llamadas = dplyr::if_else(p47_1_4 == 1, "Sí", "No"),
      usos_internet_comms_llamadas = forcats::as_factor(usos_internet_comms_llamadas),
      usos_internet_comms_date_app = dplyr::if_else(p47_1_5 == 1, "Sí", "No"),
      usos_internet_comms_date_app = forcats::as_factor(usos_internet_comms_date_app),

      ## Usos Internet - Ocio
      usos_internet_ocio_radio = dplyr::if_else(p48_1 == 1, "Sí", "No"),
      usos_internet_ocio_radio = forcats::as_factor(usos_internet_ocio_radio),
      usos_internet_ocio_tv = dplyr::if_else(p48_2 == 1, "Sí", "No"),
      usos_internet_ocio_tv = forcats::as_factor(usos_internet_ocio_tv),
      usos_internet_ocio_streaming = dplyr::if_else(p48_3 == 1, "Sí", "No"),
      usos_internet_ocio_streaming = forcats::as_factor(usos_internet_ocio_streaming),
      usos_internet_ocio_gaming = dplyr::if_else(p48_4 == 1, "Sí", "No"),
      usos_internet_ocio_gaming = forcats::as_factor(usos_internet_ocio_gaming),
      usos_internet_ocio_software = dplyr::if_else(p48_5 == 1, "Sí", "No"),
      usos_internet_ocio_software = forcats::as_factor(usos_internet_ocio_software),
      usos_internet_ocio_leer = dplyr::if_else(p48_6 == 1, "Sí", "No"),
      usos_internet_ocio_leer = forcats::as_factor(usos_internet_ocio_leer),
      usos_internet_ocio_blogging = dplyr::if_else(p48_7 == 1, "Sí", "No"),
      usos_internet_ocio_blogging = forcats::as_factor(usos_internet_ocio_blogging),
      usos_internet_ocio_web = dplyr::if_else(p48_8 == 1, "Sí", "No"),
      usos_internet_ocio_web = forcats::as_factor(usos_internet_ocio_web),
      usos_internet_ocio_storage = dplyr::if_else(p48_9 == 1, "Sí", "No"),
      usos_internet_ocio_storage = forcats::as_factor(usos_internet_ocio_storage),

      ## Usos Internet - Comercio
      usos_internet_comercio_compra = dplyr::if_else(p49_1 == 1 | p49_2 == 1, "Sí", "No"),
      usos_internet_comercio_compra = forcats::as_factor(usos_internet_comercio_compra),
      usos_internet_comercio_venta = dplyr::if_else(p49_3 == 1, "Sí", "No"),
      usos_internet_comercio_venta = forcats::as_factor(usos_internet_comercio_venta),
      usos_internet_comercio_banking = dplyr::if_else(p49_4 == 1 | p49_5 == 1, "Sí", "No"),
      usos_internet_comercio_banking = forcats::as_factor(usos_internet_comercio_banking),
      usos_internet_comercio_booking = dplyr::if_else(p49_6 == 1, "Sí", "No"),
      usos_internet_comercio_booking = forcats::as_factor(usos_internet_comercio_booking),

   )

readr::write_rds(x = eutic, path = "eutic.rds")

#===============#
#### THE END ####
#===============#