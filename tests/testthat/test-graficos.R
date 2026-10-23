grafico_base <- function() {
  df <- datos_flujos()
  df <- df[df$flujo == "E", ]
  ggplot2::ggplot(df, ggplot2::aes(fecha, valores, colour = nombres)) + ggplot2::geom_line()
}

test_that("graficos_estilo_victorgm devuelve ggplot o girafe según .estatico", {
  p <- grafico_base()
  expect_s3_class(graficos_estilo_victorgm(p, .estatico = TRUE, .title = "T", .caption = "F"), "ggplot")
  expect_s3_class(graficos_estilo_victorgm(p, .estatico = TRUE, .oscuro = TRUE, .title = "T"), "ggplot")
  expect_s3_class(graficos_estilo_victorgm(p, .estatico = FALSE), "girafe")
})

test_that("tema_victorgm admite el tema oscuro", {
  expect_s3_class(tema_victorgm(), "theme")
  expect_s3_class(tema_victorgm(.oscuro = TRUE), "theme")
})

test_that("eje_y_derecha solo actúa sobre series temporales", {
  p <- grafico_base()
  pd <- eje_y_derecha(p)
  expect_equal(pd$scales$get_scales("y")$position, "right")
  barras <- ggplot2::ggplot(data.frame(x = c("a", "b"), y = 1:2), ggplot2::aes(x, y)) + ggplot2::geom_col()
  expect_null(eje_y_derecha(barras)$scales$get_scales("y"))
})

test_that("igualar_escalas amplía los límites a la unión de rangos", {
  df1 <- data.frame(x = 1:5, y = 1:5)
  df2 <- data.frame(x = 1:5, y = 10:14)
  p1 <- ggplot2::ggplot(df1, ggplot2::aes(x, y)) + ggplot2::geom_line()
  p2 <- ggplot2::ggplot(df2, ggplot2::aes(x, y)) + ggplot2::geom_line()
  pp <- igualar_escalas(p1, p2)
  l1 <- ggplot2::ggplot_build(pp[[1]])$layout$panel_scales_y[[1]]$get_limits()
  l2 <- ggplot2::ggplot_build(pp[[2]])$layout$panel_scales_y[[1]]$get_limits()
  expect_equal(l1, c(1, 14))
  expect_equal(l2, c(1, 14))
})

test_that("las piezas de informe devuelven html o el ggplot según el formato", {
  p <- grafico_base() + ggplot2::labs(title = "Título del gráfico")
  iniciar_informe()
  # Fuera de un documento HTML, figura() devuelve el ggplot limpio
  f <- figura(p, rotulo = "R", fuente = "INE")
  expect_s3_class(f, "ggplot")
  expect_null(ggplot2::get_labs(f)$title)

  k <- kpis(kpi("Exportaciones", "1", "+2 %", signo = 2), kpi("Importaciones", "1", "-2 %", signo = -2))
  expect_s3_class(k, "shiny.tag")
  html <- as.character(k)
  expect_match(html, "sube")
  expect_match(html, "baja")

  r <- resumen(bloque("A", "Clave", "Texto", seccion = "sec-a"))
  expect_match(as.character(r), "una-columna")
  expect_match(as.character(r), "href=\"#sec-a\"")

  t <- tabla(gt::gt(head(datos_flujos())), rotulo = "T", fuente = "INE")
  expect_s3_class(t, "gt_tbl")

  expect_true(file.exists(css_informe_victorgm(.como = "ruta")))
  expect_s3_class(css_informe_victorgm(), "shiny.tag")
})

test_that("revisar_orden_geoms detecta capas desordenadas", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    "p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +",
    "  ggplot2::geom_point() +",
    "  ggplot2::geom_line()",
    "q <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +",
    "  ggplot2::geom_col() +",
    "  ggplot2::geom_hline(yintercept = 0) +",
    "  ggiraph::geom_line_interactive()"
  ), tmp)
  res <- revisar_orden_geoms(tmp, .silencioso = TRUE)
  expect_equal(nrow(res), 1)
  expect_equal(res$linea, 1)
  expect_match(res$mensaje, "geom_line va despu")
})
