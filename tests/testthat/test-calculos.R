test_that("get_saldo añade el saldo como flujo", {
  df <- datos_flujos()
  s <- get_saldo(df)
  expect_setequal(unique(s$flujo), c("E", "I", "Saldo"))
  fila <- s[s$fecha == as.Date("2022-01-01") & s$nombres == "A", ]
  expect_equal(fila$valores[fila$flujo == "Saldo"],
               fila$valores[fila$flujo == "E"] - fila$valores[fila$flujo == "I"])
})

test_that("get_saldo ignora otros flujos y avisa con exportaciones negativas", {
  df <- datos_flujos()
  extra <- df[1, ]
  extra$flujo <- "Otro"
  s <- get_saldo(rbind(df, extra))
  expect_false("Otro" %in% s$flujo)

  df_neg <- df
  df_neg$valores[df_neg$flujo == "E"] <- -1
  expect_warning(get_saldo(df_neg), "exportaciones negativos")
})

test_that("get_cobertura calcula exportaciones sobre importaciones en tanto por cien", {
  df <- datos_flujos()
  cob <- get_cobertura(df)
  expect_setequal(unique(cob$flujo), c("E", "I", "Cobertura"))
  fila <- cob[cob$fecha == as.Date("2022-01-01") & cob$nombres == "A", ]
  expect_equal(fila$valores[fila$flujo == "Cobertura"],
               100 * fila$valores[fila$flujo == "E"] / fila$valores[fila$flujo == "I"])
  cob1 <- get_cobertura(df, .en_tanto_por_uno = TRUE)
  expect_equal(max(cob1$valores[cob1$flujo == "Cobertura"]), max(cob$valores[cob$flujo == "Cobertura"]) / 100)
})

test_that("get_tasa_variacion calcula la tasa interanual por grupo y flujo", {
  df <- datos_flujos()
  tv <- get_tasa_variacion(df, .periodos_atras = 12, .col_flujo = "flujo")
  expect_true("tv_12_meses" %in% names(tv))
  serie <- df[df$flujo == "E" & df$nombres == "A", ]
  esperado <- 100 * (serie$valores[13] / serie$valores[1] - 1)
  obtenido <- tv$tv_12_meses[tv$flujo == "E" & tv$nombres == "A" & tv$fecha == serie$fecha[13]]
  expect_equal(obtenido, esperado)
  expect_true(all(is.na(tv$tv_12_meses[tv$fecha < as.Date("2023-01-01")])))
})

test_that("get_variacion y get_acumulado devuelven las columnas esperadas", {
  df <- datos_flujos()
  v <- get_variacion(df, .periodos_atras = 1, .col_flujo = "flujo")
  expect_true("variacion_1_meses" %in% names(v))
  serie <- v[v$flujo == "E" & v$nombres == "A", ]
  expect_equal(serie$variacion_1_meses[2], serie$valores[2] - serie$valores[1])

  ac <- get_acumulado(df, .periodos_atras = 12, .col_flujo = "flujo")
  expect_true("acumulado_12_meses" %in% names(ac))
  serie <- ac[ac$flujo == "I" & ac$nombres == "B", ]
  serie <- serie[order(serie$fecha), ]
  expect_equal(serie$acumulado_12_meses[12], sum(serie$valores[1:12]))
})

test_that("get_acumulado_desde_1_enero acumula dentro de cada año", {
  df <- datos_flujos()
  ac <- get_acumulado_desde_1_enero(df, .col_flujo = "flujo")
  expect_true("acumulado_desde_1_enero" %in% names(ac))
  serie <- ac[ac$flujo == "E" & ac$nombres == "A", ]
  serie <- serie[order(serie$fecha), ]
  expect_equal(serie$acumulado_desde_1_enero[3], sum(serie$valores[1:3]))
  expect_equal(serie$acumulado_desde_1_enero[13], serie$valores[13])
})

test_that("get_peso reparte el 100 % entre los grupos de cada fecha y flujo", {
  df <- datos_flujos()
  p <- get_peso(df)
  expect_true("peso" %in% names(p))
  sumas <- tapply(p$peso, paste(p$fecha, p$flujo), sum)
  expect_true(all(abs(sumas - 100) < 1e-8))
})

test_that("get_contribucion suma la tasa de variación del total", {
  df <- datos_flujos()
  ct <- get_contribucion(df, .periodos_atras = 12)
  expect_true("contribucion_12_meses" %in% names(ct))
  ultima <- ct[ct$fecha == max(ct$fecha) & ct$flujo == "E", ]
  totales <- tapply(df$valores[df$flujo == "E"], df$fecha[df$flujo == "E"], sum)
  tv_total <- 100 * (totales[[length(totales)]] / totales[[length(totales) - 12]] - 1)
  expect_equal(sum(ultima$contribucion_12_meses), tv_total, tolerance = 1e-6)
})

test_that("get_media_movil devuelve la media de los últimos periodos", {
  df <- datos_flujos()
  mm <- get_media_movil(df, .periodos = 3, .col_flujo = "flujo")
  expect_true("mm_3_meses" %in% names(mm))
  serie <- mm[mm$flujo == "E" & mm$nombres == "A", ]
  serie <- serie[order(serie$fecha), ]
  expect_equal(serie$mm_3_meses[3], mean(serie$valores[1:3]))
})

test_that("get_agregado agrupa en bloques de n periodos", {
  df <- datos_flujos()
  ag <- get_agregado(df, .periodos = 3, .col_flujo = "flujo")
  expect_equal(nrow(ag), nrow(df) / 3)
  serie <- df[df$flujo == "E" & df$nombres == "A", ]
  primero <- ag[ag$flujo == "E" & ag$nombres == "A", ]
  primero <- primero[order(primero$fecha), ]
  expect_equal(primero$valores[1], sum(serie$valores[1:3]))
  expect_equal(primero$fecha[1], serie$fecha[3])

  media <- get_agregado(df, .periodos = 12, .transformacion = "media", .alineacion = "natural", .col_flujo = "flujo")
  expect_equal(nrow(media), 2 * 2 * 3)
  expect_error(get_agregado(df, .transformacion = "otra"), "no reconocida")
})

test_that("get_contribucion_acumulada reparte el crecimiento del acumulado", {
  df <- datos_flujos()
  ca <- get_contribucion_acumulada(df, .años_atras_contribucion = 2)
  expect_setequal(names(ca), c("flujo", "nombres", "fecha_inicial", "fecha_final",
                               "acumulado_inicial", "acumulado_final", "contribucion"))
  e <- ca[ca$flujo == "E", ]
  total_inicial <- sum(e$acumulado_inicial)
  total_final <- sum(e$acumulado_final)
  expect_equal(sum(e$contribucion), 100 * (total_final / total_inicial - 1))
  expect_error(get_contribucion_acumulada(df, .años_atras_contribucion = 5), "fecha inicial")
})
