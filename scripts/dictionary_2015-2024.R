my_dictionary = function(data) {
  data |>
    mutate(
      # Variables de diseño
      weights = HV005 / 1000000,
      cluster = HV001,
      stratum = HV022,
      
      across(c(HC70, HC71, HC72), ~ replace(.x, .x > 9996, NA)),
      
      # Variable de análisis
      sexo_infante = case_match(HC27, 1 ~ "Varon", 2 ~ "Mujer") |>
        fct_relevel("Varon", "Mujer") |>
        ff_label("Sexo del infante"), 
      
      sexo_jefe = case_match(HV219, 1 ~ "Varon", 2 ~ "Mujer") |>
        fct_relevel("Varon", "Mujer") |>
        ff_label("Sexo del jefe de hogar"), 
      
      talla_edad_oms = HC70 |>
        set_variable_labels(talla_edad_oms = "Talla/edad desviación estándar"), 
      
      peso_edad_oms = HC71 |>
        set_variable_labels(peso_edad_oms = "Peso/edad desviación estándar"), 
      
      peso_talla_oms = HC72  |>
        set_variable_labels(peso_talla_oms = "Peso/talla desviación estándar"), 
      
      dcronica = case_when(
        talla_edad_oms < -200 ~ "1",
        talla_edad_oms >= -200 ~ "0") |>
        factor() |>
        fct_recode("Si" = "1", "No" = "0") |>
        fct_relevel("No", "Si") |>
        ff_label("Desnutrición crónica"), 
      
      wasting = case_when(
        peso_edad_oms < -200 ~ "1",
        peso_edad_oms >= -200 ~ "0") |>
        ff_label("Emaciación"), 
       
      thinness = case_when(
        peso_talla_oms < -200 ~ "1",
        peso_talla_oms >= -200 ~ "0") |>
        ff_label("Delgadez"), 
       
      quintil = factor(HV270) |>
        fct_recode("Q1" = "1", "Q2" = "2", "Q3" = "3", "Q4" = "4", "Q5" = "5") |>
        fct_relevel("Q1", "Q2", "Q3", "Q4", "Q5") |>
        ff_label("Índice de riqueza"), 
       
      region = factor(SHREGION) |>
        fct_recode("Costa" = "1", "Costa" = "2", "Sierra" = "3", "Selva" = "4") |>
        fct_relevel("Costa", "Sierra", "Selva") |>
        ff_label("Dominio geografico"), 
       
      HAj = case_when(
        !is.na(HC53) & !is.na(HV040) & HV040 < 500 ~ HC53 / 10,
        !is.na(HC53) & !is.na(HV040) & HV040 >= 500 ~ HC53 / 10 - ((0.0056384 * HV040 + 0.0000003 * HV040^2) / 10),
        TRUE ~ NA_real_)|>
        ff_label("Hemoglobina Ajustada (g/dL)"), 
       
      anemia = case_when(
        HV103 == 1 & HC1 >= 6 & HC1 <= 23 & HAj > 1 & HAj < 10.5 ~ 1,
        HV103 == 1 & HC1 >= 6 & HC1 <= 23 & HAj >= 10.5 & HAj < 30 ~ 0,
        HV103 == 1 & HC1 >= 24 & HC1 <= 59 & HAj > 1 & HAj < 11 ~ 1,
        HV103 == 1 & HC1 >= 24 & HC1 <= 59 & HAj >= 11 & HAj < 30 ~ 0,
        TRUE ~ NA_real_
      ), 
       
      anemia = factor(anemia) |>
        fct_recode("Si" = "1", "No" = "0") |>
        fct_relevel("No", "Si") |>
        ff_label("Anemia"), 
      
      dbm = case_when(
        dcronica == "Si" & anemia == "Si" ~ 1,
        dcronica == "No" & anemia == "No" ~ 0,
        dcronica == "Si" & anemia == "No" ~ 0,
        dcronica == "No" & anemia == "Si" ~ 0) |>
        factor() |>
        fct_recode("Si" = "1", "No" = "0") |>
        fct_relevel("No", "Si") |>
        ff_label("Doble carga"), 
       
      region_a = case_when(
        # Costa
        HV024 %in% c(20, 14, 13, 2, 15, 11, 4, 18, 23, 7) ~ 1,
        # Sierra
        HV024 %in% c(6, 19, 12, 9, 5, 3, 8, 21) ~ 2,
        # Selva
        HV024 %in% c(16, 25, 17, 10, 1, 22) ~ 3,
        TRUE ~ NA_real_
      ), 
       
      region_a = factor(region_a) |>
        fct_recode("Costa" = "1", "Sierra" = "2", "Selva" = "3") |>
        fct_relevel("Costa", "Sierra", "Selva") |>
        ff_label("Región"), 
       
      edad_meses_cat = case_when(
        HC1 >= 0  & HC1 <= 11 ~ 0,
        HC1 >= 12 & HC1 <= 23 ~ 1,
        HC1 >= 24 & HC1 <= 35 ~ 2,
        HC1 >= 36 ~ 3) |>
        factor() |>
        fct_recode("< 12 meses" = "0", "12 a 23 meses" = "1", "24 a 35 meses" = "2", "36 a 59 meses" = "3") |>
        fct_relevel("< 12 meses", "12 a 23 meses", "24 a 35 meses", "36 a 59 meses") |>
        ff_label("Edad del infante"),
       
      edad_meses_minsa_cat = case_when(
        HC1 >= 6  & HC1 <= 11 ~ 1,
        HC1 >= 12 & HC1 <= 23 ~ 2,
        HC1 >= 24 & HC1 <= 35 ~ 3) |>
        factor() |>
        fct_recode("6 a 11 meses" = "1", "12 a 23 meses" = "2", "24 a 35 meses" = "3") |>
        fct_relevel("6 a 11 meses", "12 a 23 meses", "24 a 35 meses") |>
        ff_label("Edad del infante (MINSA)"), 
       
      altura = case_when(
        HV040 < 500 ~ 1, 
        HV040 >= 500 & HV040 < 2500 ~ 2, 
        HV040 >= 2500 ~ 3) |>
        factor() |>
        fct_recode("<500" = "1", "500–2499" = "2", "≥2500" = "3") |>
        fct_relevel("<500", "500–2499", "≥2500") |>
        ff_label("Altura (m s. n. m.)"), 
       
      residencia = case_match(HV025, 1 ~ "Urbano", 2 ~ "Rural") |>
        fct_relevel("Urbano", "Rural") |>
        ff_label("Área de residencia"), 
       
      educacion_madre = case_when(
        HC61 == "0" ~ "Sin educación", HC61 == "1" ~ "Primaria", 
        HC61 == "2" ~ "Secundaria", HC61 == "3" ~ "Superior",
        TRUE ~ NA) |>
        factor() |>
        fct_relevel("Sin educación", "Primaria", "Secundaria", "Superior") |>
        ff_label("Nivel educativo de la madre"), 
       
      agua_tipo = case_when(
        HV201 %in% c(11, 12, 13, 71) ~ "Segura",
        HV201 %in% c(21, 22, 41, 43, 51, 61, 96) ~ "Insegura") |>
        factor() |>
        fct_relevel("Segura", "Insegura") |>
        ff_label("Agua de consumo humano"), 
       
      agua_tratamiento = case_match(HV237, 0 ~ "No", 1 ~ "Si") |>
        fct_relevel("No", "Si") |>
        ff_label("Tratamiento del agua"), 
       
      agua_disponible = case_match(
        SH42, 0 ~ "No disponible todo el día", 1 ~ "Disponible todo el día") |>
        fct_relevel("No disponible todo el día", "Disponible todo el día") |>
        ff_label("Disponibilidad continua de agua potable"), 
      
    )
}
