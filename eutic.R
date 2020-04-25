#=============#
#### EUTIC ####
#=============#

library(magrittr)

# Genera shape file para mapas en el App ----------------------------------
mapa <- sf::read_sf(here::here("IneShapeFiles/ine_depto.shp"))
mapa <- sf::st_set_crs(mapa, "+proj=utm +zone=21 +south")
mapa <- sf::st_transform(mapa, "+proj=longlat +datum=WGS84")
mapa <- mapa[, base::c(4, 6)]
mapa <- mapa[-16,]
mapa$NOMBRE <- base::c(
   "Montevideo",
   "Artigas",
   "Canelones",
   "Colonia",
   "Durazno",
   "Florida",
   "Lavalleja",
   "Paysandú",
   "Río Negro",
   "Rivera",
   "Rocha",
   "Salto",
   "San José",
   "Soriano",
   "Treinta y Tres",
   "Tacuarembó",
   "Flores",
   "Maldonado",
   "Cerro Largo"
)
base::colnames(mapa) <- base::c("depto", "geometry")

sf::st_write(obj = mapa, dsn = here::here("www/mapa.shp"), append = FALSE)


# Carga datos -------------------------------------------------------------
eutic <- haven::read_sav(
   file = here::here("BASE HOGARES Y PERSONAS EUTIC 2016.sav")
) %>%
   dplyr::rename_all(
      .funs = tolower
   )


# Construye objeto para el App --------------------------------------------
x <- eutic %>%
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

aux_data <- x %>%
   dplyr::group_by(
      depto,
      tiene_desktop
   ) %>%
   dplyr::summarise(
      n = base::sum(peso_hogar)
   ) %>%
   dplyr::mutate(
      prop = n / base::sum(n)
   ) %>%
   dplyr::filter(
      tiene_desktop == "Sí"
   ) %>%
   dplyr::select(
      depto,
      prop
   ) %>%
   dplyr::ungroup() %>%
   dplyr::mutate(
      depto = base::as.character(depto)
   )

pal <- leaflet::colorNumeric(
   palette = "Greens",
   domain = aux_data$prop
   )

mapa$depto == aux_data$depto
length(mapa$depto)
length(aux_data$depto)
mapa_aux <- base::merge(mapa, aux_data, by = "depto")

leaflet::leaflet(
   data = mapa_aux
) %>%
   leaflet::addTiles() %>%
   leaflet::setView(
      lng = -56.1,
      lat =  -32,
      zoom = 7
   ) %>%
   leaflet::addPolygons(
      fillColor = ~pal(prop),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = leaflet::highlightOptions(
         weight = 5,
         color = "#666",
         dashArray = "",
         fillOpacity = 0.7,
         bringToFront = TRUE
      ),
      label = base::paste0(
         depto,
         ": ",
         formattable::comma(
            x = mapa_aux$prop,
            digits = 0L,
            big.mark = ".",
            decimal.mark = ","
         ),
         " dólares"
      ),
      labelOptions = leaflet::labelOptions(
         style = base::list(
            "font-weight" = "normal",
            padding = "3px 8px"
         ),
         textsize = "15px",
         direction = "auto"
      )
   )

#===============#
#### THE END ####
#===============#