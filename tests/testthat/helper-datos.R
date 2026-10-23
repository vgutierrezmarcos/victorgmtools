# Datos sintéticos compartidos por los tests (sin red)

fechas_mensuales <- function(desde = "2022-01-01", hasta = "2024-12-01") {
  seq(as.Date(desde), as.Date(hasta), by = "month")
}

# Serie mensual con dos flujos y dos grupos; los valores crecen de forma determinista
datos_flujos <- function() {
  fechas <- fechas_mensuales()
  df <- expand.grid(fecha = fechas, flujo = c("E", "I"), nombres = c("A", "B"), stringsAsFactors = FALSE)
  df$valores <- 100 + seq_len(nrow(df))
  df
}

# Datos de comercio por producto para los índices de precios
datos_productos <- function() {
  set.seed(123)
  fechas <- fechas_mensuales()
  dp <- expand.grid(fecha = fechas, flujo = c("E", "I"), taric = c("0101", "0202", "0303"), stringsAsFactors = FALSE)
  dp$peso <- round(stats::runif(nrow(dp), 500, 1500))
  dp$valor_estadistico <- round(dp$peso * stats::runif(nrow(dp), 2, 4) * (1 + seq_len(nrow(dp)) / nrow(dp)))
  dp
}
