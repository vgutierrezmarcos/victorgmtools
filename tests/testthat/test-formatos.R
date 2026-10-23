test_that("los formatos numéricos usan punto de miles y coma decimal", {
  expect_equal(es_number(1234567.891, accuracy = 0.1), "1.234.567,9")
  expect_equal(fmt_mill(1234567890), "1.234,6 M€")
  expect_equal(fmt_pct(12.34), "12,3 %")
  expect_equal(fmt_pct(c(5.3, -2.1), signo = TRUE), c("+5,3 %", "-2,1 %"))
  expect_equal(fmt_ton(1234567), "1.234,6 t")
  etiqueta <- es_label_number(suffix = " %", accuracy = 0.1)
  expect_equal(etiqueta(1500.5), "1.500,5 %")
})

test_that("fraccion_txt y veces_txt traducen cifras a palabras", {
  expect_equal(fraccion_txt(0.48), "casi la mitad")
  expect_equal(fraccion_txt(0.27), "algo más de una cuarta parte")
  expect_equal(fraccion_txt(0.5), "la mitad")
  expect_equal(fraccion_txt(c(0.02, 0.98)), c("menos de una décima parte", "casi la totalidad"))
  expect_true(is.na(fraccion_txt(NA)))

  expect_equal(veces_txt(3.1), "se triplica")
  expect_equal(veces_txt(2.3), "se multiplica por más de dos")
  expect_equal(veces_txt(1.3), "aumenta en casi un tercio")
  expect_equal(veces_txt(0.5), "se reduce a la mitad")
  expect_equal(veces_txt(0.7, plural = TRUE), "caen en casi un tercio")
  expect_equal(veces_txt(7), "se multiplica por 7")
})

test_that("filas_leyenda equilibra las filas", {
  expect_equal(filas_leyenda(4), 1L)
  expect_equal(filas_leyenda(5), 2L)
  expect_equal(filas_leyenda(6, max_col = 2), 3L)
  expect_equal(filas_leyenda(8), 2L)
  expect_equal(filas_leyenda(0), 1L)
})
