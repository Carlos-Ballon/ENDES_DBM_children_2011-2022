my_dictionary_factors = function(data) {
  data |>
    mutate(
      # Variable de análisis
      sexo_infante = factor(sexo_infante) |>
        fct_recode("Masculino" = "1", "Femenino" = "2") |>
        fct_relevel("Masculino", "Femenino") |>
        ff_label("Sexo del niño"), 
      
      sexo_jefe = factor(sexo_jefe) |>
        fct_recode("Masculino" = "1", "Femenino" = "2") |>
        fct_relevel("Masculino", "Femenino") |>
        ff_label("Sexo del jefe de hogar"),
      
      dcronica = factor(dcronica) |>
        fct_recode("Si" = "2", "No" = "1") |>
        fct_relevel("No", "Si") |>
        ff_label("Desnutrición crónica"), 
      
      wasting = factor(wasting) |>
        fct_recode("Si" = "1", "No" = "0") |>
        fct_relevel("No", "Si") |>
        ff_label("Emaciación"), 
      
      thinness = factor(thinness) |>
        fct_recode("Si" = "1", "No" = "0") |>
        fct_relevel("No", "Si") |>
        ff_label("Delgadez"),
      
      quintil = factor(quintil) |>
        fct_recode("Muy pobre" = "1", "Pobre" = "2", "Medio" = "3","Rico" = "4", "Muy Rico" = "5" ) |>
        fct_relevel("Muy Rico", "Rico", "Medio", "Pobre", "Muy pobre") |>
        ff_label("Índice de riqueza"), 
      
      region = factor(region) |>
        fct_recode("Costa" = "1", "Sierra" = "2", "Selva" = "3") |>
        fct_relevel("Costa", "Sierra", "Selva") |>
        ff_label("Dominio geografico"), 
      
      anemia = factor(anemia) |>
        fct_recode("Si" = "2", "No" = "1") |>
        fct_relevel("No", "Si") |>
        ff_label("Anemia"), 
      
      dbm = factor(dbm) |>
        fct_recode("Con doble carga" = "2", "Sin doble carga" = "1") |>
        fct_relevel("Sin doble carga", "Con doble carga") |>
        ff_label("Doble carga de malnutrición"), 
      
      region_a = factor(region_a) |>
        fct_recode("Costa" = "1", "Sierra" = "2", "Selva" = "3") |>
        fct_relevel("Costa", "Sierra", "Selva") |>
        ff_label("Región"), 
      
      edad_meses_minsa_cat = factor(edad_meses_minsa_cat) |>
        fct_recode("6 a 11 meses" = "1", "12 a 23 meses" = "2", "24 a 35 meses" = "3") |>
        fct_relevel("6 a 11 meses", "12 a 23 meses", "24 a 35 meses") |>
        ff_label("Edad del niño"), 
      
      altura = factor(altura) |>
        fct_recode("<500" = "1", "500–2499" = "2", "≥2500" = "3") |>
        fct_relevel("<500", "500–2499", "≥2500") |>
        ff_label("Altitud del hogar (m s. n. m.)"), 
      
      residencia = factor(residencia) |>
        fct_recode("Urbano" = "1", "Rural" = "2") |>
        fct_relevel("Urbano", "Rural") |>
        ff_label("Área de residencia"), 
      
      educacion_madre = factor(educacion_madre) |>
        fct_recode("Sin educación" = "1","Primaria" = "2", "Secundaria" = "3", "Superior" = "4") |>
        fct_relevel("Sin educación", "Primaria", "Secundaria", "Superior") |>
        ff_label("Nivel educativo de la madre"), 
      
      agua_tipo = factor(agua_tipo) |> 
        fct_recode("Segura" = "1", "Insegura" = "2") |>
        fct_relevel("Segura", "Insegura") |>
        ff_label("Agua de consumo humano"), 
      
      agua_tratamiento = factor(agua_tratamiento) |>
        fct_recode("No" = "1", "Si" = "2") |>
        fct_relevel("Si", "No") |>
        ff_label("Tratamiento del agua"), 
      
      agua_disponible = factor(agua_disponible) |>
        fct_recode("No disponible todo el día" = "1", "Disponible todo el día" = "2") |>
        fct_relevel("Disponible todo el día", "No disponible todo el día") |>
        ff_label("Disponibilidad continua de agua potable"),
      
      departamento = factor(HV024) |>
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
        ff_label("Departamento"),
      
      year = factor(year)
    )
}
