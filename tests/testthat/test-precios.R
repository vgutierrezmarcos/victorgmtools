test_that("get_precios_ponderados con año de referencia devuelve índices y RRI", {
  dp <- datos_productos()
  pp <- get_precios_ponderados(dp, .año_referencia = 2023)
  expect_setequal(names(pp), c("fecha", "precios_ponderados_export", "precios_ponderados_import", "rri"))
  expect_equal(nrow(pp), length(fechas_mensuales()))
  expect_equal(pp$rri, pp$precios_ponderados_export / pp$precios_ponderados_import)
  expect_true(all(pp$precios_ponderados_export > 0))
})

test_that("get_precios_ponderados con cesta móvil descarta los primeros meses", {
  dp <- datos_productos()
  pp <- get_precios_ponderados(dp)
  expect_equal(nrow(pp), length(fechas_mensuales()) - 11)
})

test_that("get_precios_ponderados con un solo flujo avisa de que no hay RRI", {
  dp <- datos_productos()
  expect_warning(pp <- get_precios_ponderados(dp[dp$flujo == "E", ], .año_referencia = 2023), "RRI")
  expect_false("rri" %in% names(pp))
})

test_that("el método SQL coincide con el método en R", {
  skip_if_not_installed("duckdb")
  dp <- datos_productos()
  pr <- get_precios_ponderados(dp, .año_referencia = 2023)
  ps <- get_precios_ponderados(dp, .año_referencia = 2023, .metodo = "SQL")
  ps <- ps[order(ps$fecha), ]
  expect_equal(ps$precios_ponderados_export, pr$precios_ponderados_export, tolerance = 1e-8)
})

test_that("get_flujos_reales devuelve el volumen y lo indexa al año base", {
  dp <- datos_productos()
  fr <- get_flujos_reales(dp, .año_referencia = 2023, .año_base = 2023)
  expect_setequal(names(fr), c("fecha", "flujo", "valor_estadistico", "precios_ponderados", "flujos_reales"))
  expect_false(any(is.na(fr$flujos_reales)))
  base <- fr[fr$fecha == as.Date("2023-01-01"), ]
  expect_equal(base$flujos_reales, c(100, 100))

  fr2 <- get_flujos_reales(dp, .año_referencia = 2023)
  expect_equal(fr2$flujos_reales, fr2$valor_estadistico / fr2$precios_ponderados)
})

test_that("get_descomposicion_tv reparte la tasa nominal entre precio y volumen", {
  dp <- datos_productos()
  dt <- dp |>
    get_flujos_reales(.año_referencia = 2023) |>
    get_descomposicion_tv(.meses_atras = 12)
  expect_true(all(c("tv_nominal", "contribucion_precios", "contribucion_volumen") %in% names(dt)))
  expect_gt(nrow(dt), 0)
  expect_equal(dt$contribucion_precios + dt$contribucion_volumen, dt$tv_nominal)
})

test_that("los gráficos de precios devuelven ggplot en modo estático y girafe en interactivo", {
  dp <- datos_productos()
  pp <- get_precios_ponderados(dp, .año_referencia = 2023)
  fr <- get_flujos_reales(dp, .año_referencia = 2023)
  dt <- get_descomposicion_tv(fr)

  expect_s3_class(get_rri_plt(pp, .estatico = TRUE), "ggplot")
  expect_s3_class(get_rri_plt(pp, .estatico = TRUE, .representar_indices = FALSE), "ggplot")
  expect_s3_class(get_flujos_reales_plt(fr, .estatico = TRUE), "ggplot")
  expect_s3_class(get_descomposicion_tv_plt(dt, .estatico = TRUE, .flujo_seleccionado = "E"), "ggplot")
  expect_s3_class(get_precio_producto_plt(dp, "0101", .estatico = TRUE), "ggplot")
  expect_s3_class(get_rri_plt(pp, .estatico = FALSE), "girafe")
  expect_error(get_precio_producto_plt(dp, "9999", .estatico = TRUE), "No hay observaciones")
})
