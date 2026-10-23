paletas <- c("paleta_paises", "paleta_exportaciones_importaciones", "paleta_energeticos",
             "paleta_subsectores_energeticos_sec", "paleta_subsectores_energeticos_cuci",
             "paleta_efecto_precio_volumen", "paleta_balanza_de_pagos", "paleta_sectores_sec",
             "paleta_sectores_cuci", "paleta_secciones_taric", "paleta_actividades_economicas",
             "paleta_modos_suministro", "paleta_ccaa", "paleta_chokepoints", "paleta_puertos",
             "paleta_rutas_flete", "paleta_signo_victorgm", "tokens_victorgm")

test_that("las paletas devuelven colores hexadecimales con nombres únicos", {
  for (p in paletas) {
    v <- get(p, envir = asNamespace("victorgmtools"))()
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", v)), info = p)
    expect_false(any(duplicated(names(v))), info = p)
    expect_true(all(nzchar(names(v))), info = p)
  }
})

test_that("las paletas semánticas usan los colores de la web", {
  tk <- tokens_victorgm()
  expect_equal(unname(paleta_exportaciones_importaciones()[["Exportaciones"]]), "#5F2987")
  expect_equal(paleta_exportaciones_importaciones()[["E"]], paleta_exportaciones_importaciones()[["Exportaciones"]])
  expect_equal(paleta_paises()[["España"]], paleta_paises()[["ESP"]])
  expect_equal(unname(paleta_signo_victorgm()[["sube"]]), unname(tk[["exito"]]))
  expect_equal(tokens_victorgm(.oscuro = TRUE)[["fondo"]], "#16131a")
})

test_that("las rampas devuelven el número de colores pedido", {
  expect_length(colores_victorgm_categoricos(), 30)
  expect_length(colores_victorgm_categoricos(5), 5)
  expect_length(colores_victorgm_categoricos(40), 40)
  expect_length(paleta_secuencial_victorgm(4), 4)
  expect_length(paleta_divergente_victorgm(9), 9)
  expect_equal(paleta_sectores_sec()[["5"]], paleta_sectores_sec()[["Bienes de equipo"]])
  expect_equal(paleta_sectores_sec()[["01"]], paleta_sectores_sec()[["Alimentación, bebidas y tabaco"]])
  expect_equal(paleta_ccaa()[["13"]], paleta_ccaa()[["Madrid"]])
})
