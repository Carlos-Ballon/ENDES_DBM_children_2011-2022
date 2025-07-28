my_dictionary = function(data) {
  data |>
    dplyr::mutate(
      # Variables de diseño
      pesos = peso |>
        set_variable_labels(pesos = "Factor de ponderación"),
      cluster = hv001 |>
        set_variable_labels(cluster = "Conglomerado"),
      estrato = hv022 |>
        set_variable_labels(estrato = "Estrato"),
      
      # Variable de análisis
      edad_meses = hc1 |>
        set_variable_labels(edad_meses = "Edad del infante (meses)"), 
      
      edad_meses_cat = case_when(
        hc1 >= 0  & hc1 <= 11  ~ 0,
        hc1 >= 12 & hc1 <= 23  ~ 1,
        hc1 >= 24 & hc1 <= 35  ~ 2,
        hc1 >= 36 & hc1 <= 59  ~ 3,
        TRUE ~ NA_integer_
      ) |> 
        set_value_labels(edad_meses_cat = c(
          "< 12 meses" = 0,
          "12 a 23 meses" = 1,
          "24 a 35 meses" = 2,
          "36 a 59 meses" = 3)
        ) |> 
        set_variable_labels(edad_meses_cat = "Edad del infante"), 
      edad_meses_cat_factor = factor(edad_meses_cat) |>
        fct_recode(
          "< 12 meses" = "0",
          "12 a 23 meses" = "1",
          "24 a 35 meses" = "2",
          "36 a 59 meses" = "3"
        ) |>
        fct_relevel(
          "< 12 meses", "12 a 23 meses", "24 a 35 meses", "36 a 59 meses") |>
        ff_label("Edad del infante"),
      
      edad_meses_minsa_cat = case_when(
        hc1 >= 6  & hc1 <= 11  ~ 1,
        hc1 >= 12 & hc1 <= 23  ~ 2,
        hc1 >= 24 & hc1 <= 35  ~ 3,
        TRUE ~ NA_integer_
      ) |> 
        set_value_labels(edad_meses_minsa_cat = c(
          "6 a 11 meses" = 1,
          "12 a 23 meses" = 2,
          "24 a 35 meses" = 3)
        ) |> 
        set_variable_labels(edad_meses_minsa_cat = "Edad del infante (MINSA)"),
      
      edad_meses_minsa_cat_factor = factor(edad_meses_minsa_cat) |>
        fct_recode(
          "6 a 11 meses" = "1",
          "12 a 23 meses" = "2",
          "24 a 35 meses" = "3"
        ) |>
        fct_relevel("6 a 11 meses", "12 a 23 meses", "24 a 35 meses") |>
        ff_label("Edad del infante (MINSA)"),
      
      sexo_child = hc27 |>
        set_value_labels(sexo_child = c("Masculino" = 1, "Femenino" = 2)) |>
        set_variable_labels(sexo_child = "Sexo del infante"), 
      sexo_child_factor = factor(hc27) |>
        fct_recode("Masculino" = "1", "Femenino" = "2") |>
        fct_relevel("Masculino", "Femenino") |>
        ff_label("Sexo del infante"),
      
      talla_edad = hc5 / 100 |> 
        set_variable_labels(talla_edad = "Talla/edad desviación estándar"),
      
      talla_edad_oms = hc70 / 100 |> 
        set_variable_labels(talla_edad_oms = "Talla/edad desviación estándar (OMS)"),
      
      peso_edad = hc8 / 100 |> 
        set_variable_labels(peso_edad = "Peso/edad desviación estándar"),
      
      peso_edad_oms = hc71 / 100 |> 
        set_variable_labels(peso_edad_oms = "Peso/edad desviación estándar (OMS)"),
      
      peso_talla = hc11 / 100 |> 
        set_variable_labels(peso_talla = "Peso/talla desviación estándar"),
      
      peso_talla_oms = hc72 / 100 |> 
        set_variable_labels(peso_talla_oms = "Peso/talla desviación estándar (OMS)"),
      
      imc_infante = hc73 /100 |> 
        set_variable_labels(imc_infant = "Índice de masa corporal (OMS)"),
      
      stunting = case_when(
        talla_edad_oms < -2 ~ "Si",
        talla_edad_oms >= -2 ~ "No"
      ) |>
        fct_relevel("No", "Si") |>
        ff_label("Retraso del crecimiento"), # Desnutrición crónica (talla baja para la edad)
      
      wasting = case_when(
        peso_talla_oms < -2 ~ "Si",
        peso_talla_oms >= -2 ~ "No"
      ) |>
        fct_relevel("No", "Si") |>
        ff_label("Emaciación"), # Desnutrición aguda (peso bajo para la talla)
      
      thinness = case_when(
        peso_edad_oms < -2 ~ "Si",
        peso_edad_oms >= -2 ~ "No"
      ) |>
        fct_relevel("No", "Si") |>
        ff_label("Delgadez infantil"), # Desnutrición crónica o aguda (Bajo peso para la edad)
      
      exceso_peso = case_when(
        peso_talla_oms >= 2 | peso_edad_oms >= 2 | imc_infante >= 1 ~ "Si",
        peso_talla_oms < 2 & peso_edad_oms < 2 & imc_infante < 1 ~ "No"
      ) |>
        fct_relevel("No", "Si") |>
        ff_label("Exceso de peso infantil"), 
      
      anemia_child_levels = hc57 |>
        set_value_labels(anemia_child_levels = c(
          "Grave" = 1,
          "Moderada" = 2,
          "Leve" = 3,
          "Sin anemia" = 4)
        ) |> 
        set_variable_labels(anemia_child = "Anemia"),
      anemia_child_levels_factor = factor(hc57) |>
        fct_recode(
          "Grave" = "1",
          "Moderada" = "2",
          "Leve" = "3",
          "Sin anemia" = "4"
        ) |>
        fct_relevel("Sin anemia", "Leve", "Moderada", "Grave") |>
        ff_label("Anemia"), 
      
      anemia_child = case_when(
        hc57 %in% c(1, 2, 3) ~ 1,
        hc57 == 4 ~ 0
      ) |> 
        set_value_labels(anemia_child = c("No" = 0, "Si" = 1)) |>
        set_variable_labels(anemia_child = "Anemia"),
      anemia_child_factor = factor(anemia_child) |>
        fct_recode("Si" = "1", "No" = "0") |>
        fct_relevel("No", "Si") |>
        ff_label("Anemia"),
      
      DBM_anemia_stunting = case_when(
        anemia_child == 1 & stunting == "Si" ~ 1,
        anemia_child == 0 & stunting == "No" ~ 0,
        anemia_child == 1 & stunting == "No" ~ 0,
        anemia_child == 0 & stunting == "Si" ~ 0,
        TRUE ~ NA_real_
      ) |> 
        set_value_labels(DBM_anemia_stunting = c("Si" = 1, "No" = 0)) |>
        set_variable_labels(DBM_anemia_stunting = "Doble carga de malnutrición"),
      DBM_anemia_stunting_factor = factor(DBM_anemia_stunting) |>
        fct_recode("Si" = "1", "No" = "0") |>
        fct_relevel("No", "Si") |>
        ff_label("Doble carga de malnutrición"), # Anemia y retraso en el crecimiento
      
      DBM_anemia_desnutricion = case_when(
        desnutricion == 1 & hc57 %in% c(1, 2, 3) ~ 1,
        desnutricion == 0 & hc57 == 4 ~ 0,
        TRUE ~ NA_real_
      ) |>
        set_value_labels(DBM_anemia_desnutricion = c("Si" = 1, "No" = 0)) |>
        set_variable_labels(DBM_anemia_desnutricion = "Anemia y desnutrición infantil"),
      DBM_anemia_desnutricion_factor = factor(DBM_anemia_desnutricion) |>
        fct_recode("Si" = "1", "No" = "0") |>
        fct_relevel("No", "Si") |>
        ff_label("Anemia y desnutrición infantil"),
      
      DBM_anemia_wasting = case_when(
        anemia_child == 1 & wasting == "Si" ~ 1,
        anemia_child == 0 & wasting == "No" ~ 0
      ) |>
        set_value_labels(DBM_anemia_wasting = c("Si" = 1, "No" = 0)) |>
        set_variable_labels(DBM_anemia_wasting = "Anemia y emaciación infantil"),
      DBM_anemia_wasting_factor = factor(DBM_anemia_wasting) |>
        fct_recode("Si" = "1", "No" = "0") |>
        fct_relevel("No", "Si") |>
        ff_label("Anemia y emaciación infantil"),
      
      DBM_anemia_thinness = case_when(
        anemia_child == 1 & thinness == "Si" ~ 1,
        anemia_child == 0 & thinness == "No" ~ 0
      ) |>
        set_value_labels(DBM_anemia_thinness = c("Si" = 1, "No" = 0)) |>
        set_variable_labels(DBM_anemia_thinness = "Anemia y delgadez infantil"),
      DBM_anemia_thinness_factor = factor(DBM_anemia_thinness) |>
        fct_recode("Si" = "1", "No" = "0") |>
        fct_relevel("No", "Si") |>
        ff_label("Anemia y delgadez infantil"),
      
      DBM_anemia_overweight = case_when(
        anemia_child == 1 & exceso_peso == "Si" ~ 1,
        anemia_child == 0 & exceso_peso == "No" ~ 0
      ) |>
        set_value_labels(DBM_anemia_overweight = c("Si" = 1, "No" = 0)) |>
        set_variable_labels(DBM_anemia_overweight = "Anemia y exceso de peso infantil"),
      DBM_anemia_overweight_factor = factor(DBM_anemia_overweight) |>
        fct_recode("Si" = "1", "No" = "0") |>
        fct_relevel("No", "Si") |>
        ff_label("Anemia y exceso de peso infantil"),
      
      DBM_full = case_when(
        anemia_child == 1 & (stunting == "Si" | wasting == "Si" | thinness == "Si") ~ 1,
        anemia_child == 0 & (stunting == "No" & wasting == "No" & thinness == "No") ~ 0
      ) |> 
        set_value_labels(DBM_original = c("Si" = 1, "No" = 0)) |>
        set_variable_labels(DBM_original = "Doble carga de malnutrición"),
      DBM_full_factor = factor(DBM_full) |>
        fct_recode("Si" = "1", "No" = "0") |>
        fct_relevel("No", "Si") |>
        ff_label("Doble carga de malnutrición"),
      
      education = hc61 |>
        set_value_labels(education = c(
          "Sin educación" = 0,
          "Primaria" = 1,
          "Secundaria" = 2,
          "Superior" = 3)
        ) |>
        set_variable_labels(education = "Nivel educativo de la madre"), 
      education_factor = factor(hc61) |>
        fct_recode(
          "Sin educación" = "0",
          "Primaria" = "1",
          "Secundaria" = "2",
          "Superior" = "3"
        ) |>
        fct_relevel("Sin educación", "Primaria", "Secundaria", "Superior") |>
        ff_label("Nivel educativo de la madre"), 
      year = factor(id1) |>
        set_variable_labels(year = "Año de la encuesta"),
      
      area_residencia = hv025 |>
        set_value_labels(area_residencia = c("Urbano" = 1, "Rural" = 2)) |>
        set_variable_labels(area_residencia = "Área de residencia"),
      area_residencia_factor = factor(hv025) |> 
        fct_recode("Urbano" = "1", "Rural" = "2") |>
        fct_relevel("Urbano", "Rural") |>
        ff_label("Área de residencia"),
      
      region = hv024 |>
        set_value_labels(
          region = c(
            "AMAZONAS" = 1, "ANCASH" = 2, "APURIMAC" = 3, "AREQUIPA" = 4,
            "AYACUCHO" = 5, "CAJAMARCA" = 6, "CALLAO" = 7, "CUSCO" = 8,
            "HUANCAVELICA" = 9, "HUANUCO" = 10, "ICA" = 11, "JUNIN" = 12,
            "LA LIBERTAD" = 13, "LAMBAYEQUE" = 14, "LIMA" = 15, "LORETO" = 16,
            "MADRE DE DIOS" = 17, "MOQUEGUA" = 18, "PASCO" = 19, "PIURA" = 20,
            "PUNO" = 21, "SAN MARTIN" = 22, "TACNA" = 23, "TUMBES" = 24,
            "UCAYALI" = 25)
        ) |>
        set_variable_labels(region = "Región"),
      region_factor = factor(hv024) |>
        fct_recode(
          "AMAZONAS" = "1", "ANCASH" = "2", "APURIMAC" = "3", "AREQUIPA" = "4",
          "AYACUCHO" = "5", "CAJAMARCA" = "6", "CALLAO" = "7", "CUSCO" = "8",
          "HUANCAVELICA" = "9", "HUANUCO" = "10", "ICA" = "11", "JUNIN" = "12",
          "LA LIBERTAD" = "13", "LAMBAYEQUE" = "14", "LIMA" = "15", "LORETO" = "16",
          "MADRE DE DIOS" = "17", "MOQUEGUA" = "18", "PASCO" = "19", "PIURA" = "20",
          "PUNO" = "21", "SAN MARTIN" = "22", "TACNA" = "23", "TUMBES" = "24",
          "UCAYALI" = "25") |>
        fct_relevel(
          "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO", "CAJAMARCA",
          "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA", "JUNIN",
          "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE DIOS",
          "MOQUEGUA", "PASCO", "PIURA", "PUNO", "SAN MARTIN", "TACNA",
          "TUMBES", "UCAYALI") |>
        ff_label("Región")
    )
}
