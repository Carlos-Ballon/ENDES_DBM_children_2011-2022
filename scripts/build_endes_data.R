build_endes_data <- function(rech0, rech1, rech4, rech6, rech23, year) {
  # Vivienda + Hogar
  vivieda_hogar <- rech23 |>
    dplyr::left_join(rech0, by = "HHID", suffix = c("", ""))
  
  # Miembros del hogar + anemia
  hogar_salud_anemia <- rech4 |>
    dplyr::rename(HVIDX = IDXH4) |>
    dplyr::left_join(rech1, by = c("HHID", "HVIDX"), suffix = c("", "")) |>
    dplyr::rename(HC0 = HVIDX) |>
    dplyr::left_join(rech6, by = c("HHID", "HC0"), suffix = c("", "")) |>
    dplyr::mutate(rech6 = 1) # Marca de unión con rech6
  
  # Unión final: Individual + Hogar
  data <- hogar_salud_anemia |>
    dplyr::left_join(vivieda_hogar, by = "HHID", suffix = c("", "")) |>
    dplyr::mutate(year = year)
  
  return(data)
}
