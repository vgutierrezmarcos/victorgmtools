# Precios ponderados----
#' Calcula índices de precios. Devuelve un dataframe con la evolución de los índices y (en su caso) de la relación real de intercambio (RRI).
#' @param .datos_df Data frame que contiene los datos de importación y exportación sobre los que se quiere calcular el índice. La idea es que incluya `.col_fecha`, `.col_flujo`, `.col_taric`, `.col_valor_estadistico`, `.col_peso` y opcionalmente, `.col_nombres`.
#' @param .año_referencia Número. Año para tomar como referencia y hacer la cesta de bienes. En su defecto (i.e. si `NULL`), se realizarán las ponderaciones con una cesta móvil que evoluciona en base a las observaciones de los últimos `.meses_atras_acumulado` meses. Por defecto, `NULL`.
#' @param .meses_atras_acumulado Número. Número de meses sobre los que se calcula el acumulado para obtener las cestas. Por defecto, `12`.
#' @param .variable_utilizada_cesta Cadena de caracteres. Permite `"valor_estadistico"` o `"peso"`. Por defecto, `"valor_estadistico"`.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas (en formato fecha). Por defecto, `"fecha"`.
#' @param .col_flujo Cadena de caracteres. Nombre de la columna del dataframe que contiene el tipo de flujo. Por defecto, `"flujo"`.
#' @param .col_taric Cadena de caracteres. Nombre de la columna del dataframe que contiene el codigo TARIC de cada producto. Por defecto, `"taric"`.
#' @param .col_valor_estadistico Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores estadísticos. Por defecto, `"valor_estadistico"`.
#' @param .col_peso Cadena de caracteres. Nombre de la columna del dataframe que contiene el peso. Es importante que estén definidos en la misma unidad. Por defecto, `"peso"`.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres sobre los que se quiere agrupar para calcular los índices. Por ejemplo, si se quiere calcular el índice de precios para cada provincia se podrá especificar aquí. Admite varias columnas (p.ej. `c("provincia", "pais")`). Por defecto, `NULL`.
#' @param .export Nombre que denota las exportaciones en la columna `.col_flujo`. Por defecto, `"E"`.
#' @param .export Nombre que denota las exportaciones en la columna `.col_flujo`. Por defecto, `"E"`.
#' @param .import Nombre que denota las importaciones en la columna `.col_flujo`. Por defecto, `"I"`.
#' @param .get_rri Valor lógico. En caso de `get_rri = TRUE`, devuelve una columna adicional con la RRI (i.e.cociente del índice de precios de exportación y de importación). Por defecto, `TRUE`.
#' @param .metodo Si se establece `.metodo = "SQL"` usará SQL para una mayor eficiencia. Deseable cuando `.datos_df` es grande. Si se establece en `NULL` realizará los cálculos usando únicamente R base y el tidyverse. Por defecto, `NULL`.
#' @export
get_precios_ponderados <- function(.datos_df,
                                   .año_referencia = NULL,
                                   .meses_atras_acumulado = 12,
                                   .variable_utilizada_cesta = "valor_estadistico",
                                   .col_fecha = "fecha",
                                   .col_flujo = "flujo",
                                   .col_taric = "taric",
                                   .col_valor_estadistico = "valor_estadistico",
                                   .col_peso = "peso",
                                   .col_nombres = NULL,
                                   .flujo_seleccionado = NULL,
                                   .export = "E",
                                   .import = "I",
                                   .get_rri = TRUE,
                                   .remove_na = TRUE,
                                   .metodo = NULL) {

  # Validaciones iniciales (comunes para ambos métodos)
  stopifnot(
    "El input debe ser un data frame." = is.data.frame(.datos_df),
    "No existe la columna señalada para la fecha." = .col_fecha %in% names(.datos_df),
    "No existe la columna señalada para los valores." = .col_valor_estadistico %in% names(.datos_df),
    "No existe la columna señalada para el peso." = .col_peso %in% names(.datos_df),
    "No existe la columna señalada para el código TARIC." = .col_taric %in% names(.datos_df),
    "No existe la columna señalada para el flujo." = .col_flujo %in% names(.datos_df)
  )

  if (!inherits(.datos_df[[.col_fecha]], "Date")) {
    warning("Se ha convertido la columna de fechas a formato fecha.")
    .datos_df[[.col_fecha]] <- as.Date(.datos_df[[.col_fecha]])
  }

  .datos_df <- .datos_df[!is.na(.datos_df[[.col_fecha]]),]

  min_date <- min(.datos_df[[.col_fecha]], na.rm = TRUE)
  max_date <- max(.datos_df[[.col_fecha]], na.rm = TRUE)

  if (is.infinite(min_date) || is.infinite(max_date)) {
    stop("Las fechas no tienen un formato válido.")
  }

  # MÉTODO SQL
  if (!is.null(.metodo) && .metodo == "SQL") {

    # Verificar que duckdb esté disponible
    if (!requireNamespace("duckdb", quietly = TRUE)) {
      stop("El paquete duckdb es necesario para usar el método SQL. Instálalo con: install.packages('duckdb')")
    }

    # Crear conexión temporal en memoria
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    # Escribir datos a tabla temporal
    DBI::dbWriteTable(con, "datos_temp", .datos_df, overwrite = TRUE)

    # Determinar variable de cesta
    variable_cesta <- ifelse(.variable_utilizada_cesta == "valor_estadistico",
                             .col_valor_estadistico,
                             .col_peso)

    # Construir nombres de columnas para GROUP BY
    group_cols <- c(.col_fecha, .col_taric, .col_flujo)
    if (!is.null(.col_nombres)) {
      group_cols <- c(group_cols, .col_nombres)
    }
    group_by_clause <- paste(group_cols, collapse = ", ")

    # PASO 1: Agregar datos iniciales y calcular precios
    query_step1 <- paste0("
      CREATE TEMP TABLE datos_agregados AS
      SELECT
        ", group_by_clause, ",
        SUM(", .col_valor_estadistico, ") as valor_total,
        SUM(", .col_peso, ") as peso_total,
        CASE
          WHEN SUM(", .col_peso, ") > 0
          THEN SUM(", .col_valor_estadistico, ") / SUM(", .col_peso, ")
          ELSE 0
        END as precio
      FROM datos_temp
      GROUP BY ", group_by_clause
    )

    DBI::dbExecute(con, query_step1)

    # PASO 2: Crear secuencia de fechas y completar series temporales
    date_seq <- seq.Date(from = min_date, to = max_date, by = "month")
    date_df <- data.frame(fecha_completa = date_seq)
    DBI::dbWriteTable(con, "fechas_completas", date_df, overwrite = TRUE)

    unique_series_query <- paste0("
      CREATE TEMP TABLE series_unicas AS
      SELECT DISTINCT ", paste(setdiff(group_cols, .col_fecha), collapse = ", "), "
      FROM datos_agregados
    ")
    DBI::dbExecute(con, unique_series_query)

    complete_series_query <- paste0("
      CREATE TEMP TABLE datos_completos AS
      SELECT
        fc.fecha_completa as ", .col_fecha, ",
        su.*,
        COALESCE(da.valor_total, 0) as valor_total,
        COALESCE(da.peso_total, 0) as peso_total,
        COALESCE(da.precio, 0) as precio
      FROM fechas_completas fc
      CROSS JOIN series_unicas su
      LEFT JOIN datos_agregados da ON ",
                                    paste(c(paste0("fc.fecha_completa = da.", .col_fecha),
                                            paste0("su.", setdiff(group_cols, .col_fecha), " = da.", setdiff(group_cols, .col_fecha))),
                                          collapse = " AND ")
    )
    DBI::dbExecute(con, complete_series_query)

    # PASO 3: Calcular ponderaciones
    if (!is.null(.año_referencia)) {
      # Ponderación con año de referencia
      ponderacion_query <- paste0("
        CREATE TEMP TABLE ponderaciones AS
        SELECT
          ", paste(setdiff(group_cols, .col_fecha), collapse = ", "), ",
          SUM(", ifelse(variable_cesta == .col_valor_estadistico, "valor_total", "peso_total"), ") as valor_ponderacion
        FROM datos_completos
        WHERE strftime('%Y', ", .col_fecha, ") = '", .año_referencia, "'
        GROUP BY ", paste(setdiff(group_cols, .col_fecha), collapse = ", ")
      )
      DBI::dbExecute(con, ponderacion_query)

      # Calcular total de cesta
      DBI::dbExecute(con, "
        CREATE TEMP TABLE total_cesta AS
        SELECT SUM(valor_ponderacion) as total_cesta
        FROM ponderaciones
      ")

      final_query <- paste0("
        SELECT
          dc.*,
          p.valor_ponderacion,
          tc.total_cesta,
          CASE
            WHEN tc.total_cesta > 0
            THEN (p.valor_ponderacion * 1.0) / tc.total_cesta
            ELSE 0
          END as ponderacion,
          CASE
            WHEN tc.total_cesta > 0
            THEN dc.precio * ((p.valor_ponderacion * 1.0) / tc.total_cesta)
            ELSE 0
          END as precio_por_ponderacion
        FROM datos_completos dc
        LEFT JOIN ponderaciones p ON ",
                            paste(paste0("dc.", setdiff(group_cols, .col_fecha), " = p.", setdiff(group_cols, .col_fecha)),
                                  collapse = " AND "), "
        CROSS JOIN total_cesta tc
      ")

      datos_finales <- DBI::dbGetQuery(con, final_query)

      # Agregar precios ponderados
      group_final_cols <- c(.col_fecha, .col_flujo)
      if (!is.null(.col_nombres)) group_final_cols <- c(group_final_cols, .col_nombres)

      precios_ponderados_df <- datos_finales |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_final_cols))) |>
        dplyr::summarise(precios_ponderados = sum(precio_por_ponderacion, na.rm = TRUE),
                         .groups = "drop")

    } else {
      # Para ponderación móvil, usar método híbrido (SQL + R)
      datos_completos <- DBI::dbGetQuery(con, "SELECT * FROM datos_completos")

      # Ajustar nombre de columna para get_acumulado
      col_valores_sql <- ifelse(variable_cesta == .col_valor_estadistico, "valor_total", "peso_total")

      # Aplicar get_acumulado (mantener en R)
      datos_completos <- datos_completos |>
        victorgmtools::get_acumulado(.periodos_atras = .meses_atras_acumulado,
                                     .frecuencia = "mensual",
                                     .col_valores = col_valores_sql,
                                     .col_nombres = c(.col_nombres, .col_taric),
                                     .col_flujo = .col_flujo,
                                     .col_fecha = .col_fecha)

      acumulado_x_meses <- paste0("acumulado_", .meses_atras_acumulado, "_meses")

      precios_ponderados_df <- datos_completos |>
        dplyr::rename(valor_ponderacion_acumulado = !!rlang::sym(acumulado_x_meses)) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha)),
                        dplyr::across(dplyr::all_of(.col_flujo)),
                        dplyr::across(dplyr::all_of(.col_nombres))) |>
        dplyr::mutate(total_cesta = sum(!!rlang::sym(col_valores_sql), na.rm = TRUE)) |>
        dplyr::ungroup() |>
        victorgmtools::get_acumulado(.periodos_atras = .meses_atras_acumulado,
                                     .frecuencia = "mensual",
                                     .col_valores = "total_cesta",
                                     .col_nombres = .col_nombres,
                                     .col_flujo = .col_flujo,
                                     .col_fecha = .col_fecha) |>
        dplyr::rename(total_cesta_acumulado = !!rlang::sym(acumulado_x_meses)) |>
        dplyr::select(-total_cesta) |>
        dplyr::mutate(ponderacion = valor_ponderacion_acumulado / total_cesta_acumulado,
                      precio_por_ponderacion = precio * ponderacion) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha)),
                        dplyr::across(dplyr::all_of(.col_flujo)),
                        dplyr::across(dplyr::all_of(.col_nombres))) |>
        dplyr::summarise(precios_ponderados = sum(precio_por_ponderacion, na.rm = TRUE),
                         .groups = "drop")
    }

  } else {

    # MÉTODO ORIGINAL (R puro)
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha)),
                      dplyr::across(dplyr::all_of(.col_taric)),
                      dplyr::across(dplyr::all_of(.col_nombres)),
                      dplyr::across(dplyr::all_of(.col_flujo))) |>
      dplyr::summarise(!!rlang::sym(.col_valor_estadistico) := sum(!!rlang::sym(.col_valor_estadistico), na.rm = TRUE),
                       !!rlang::sym(.col_peso) := sum(!!rlang::sym(.col_peso), na.rm = TRUE)) |>
      dplyr::ungroup()

    date_seq <- seq.Date(from = min_date,
                         to = max_date,
                         by = "month")

    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_taric)), dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(setNames(list(0, 0), c(.col_valor_estadistico, .col_peso))) |>
      dplyr::ungroup()

    if (.variable_utilizada_cesta == "valor_estadistico"){
      .variable_utilizada_cesta <- .col_valor_estadistico
    } else if (.variable_utilizada_cesta == "peso"){
      .variable_utilizada_cesta  <- .col_peso
    }

    if(!is.null(.año_referencia)){
      ponderacion_referencia <-
        .datos_df |>
        dplyr::mutate(año = as.Date(!!rlang::sym(.col_fecha)) |> lubridate::year()) |>
        dplyr::filter(año == .año_referencia) |>
        dplyr::select(-año) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_taric)),
                        dplyr::across(dplyr::all_of(.col_nombres)),
                        dplyr::across(dplyr::all_of(.col_flujo))) |>
        dplyr::reframe(valor_ponderacion := sum(!!rlang::sym(.variable_utilizada_cesta), na.rm = TRUE)) |>
        dplyr::mutate(total_cesta = sum(valor_ponderacion, na.rm = TRUE),
                      ponderacion = valor_ponderacion / total_cesta)

      .datos_df <-
        .datos_df |>
        dplyr::mutate(precio = !!rlang::sym(.col_valor_estadistico) / !!rlang::sym(.col_peso)) |>
        dplyr::left_join(ponderacion_referencia) |>
        dplyr::mutate(precio_por_ponderacion = precio * ponderacion)

    } else{
      acumulado_x_meses <- paste0("acumulado_", .meses_atras_acumulado, "_meses")

      .datos_df <-
        .datos_df |>
        dplyr::mutate(precio = !!rlang::sym(.col_valor_estadistico) / !!rlang::sym(.col_peso)) |>
        victorgmtools::get_acumulado(.periodos_atras = .meses_atras_acumulado,
                                     .frecuencia = "mensual",
                                     .col_valores = .variable_utilizada_cesta,
                                     .col_nombres = c(.col_nombres, .col_taric),
                                     .col_flujo = .col_flujo,
                                     .col_fecha = .col_fecha) |>
        dplyr::rename(valor_ponderacion_acumulado = !!rlang::sym(acumulado_x_meses)) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha)),
                        dplyr::across(dplyr::all_of(.col_flujo)),
                        dplyr::across(dplyr::all_of(.col_nombres))) |>
        dplyr::mutate(total_cesta = sum(!!rlang::sym(.variable_utilizada_cesta), na.rm = TRUE)) |>
        dplyr::ungroup() |>
        victorgmtools::get_acumulado(.periodos_atras = .meses_atras_acumulado,
                                     .frecuencia = "mensual",
                                     .col_valores = "total_cesta",
                                     .col_nombres = .col_nombres,
                                     .col_flujo = .col_flujo,
                                     .col_fecha = .col_fecha) |>
        dplyr::rename(total_cesta_acumulado = !!rlang::sym(acumulado_x_meses)) |>
        dplyr::select(-total_cesta) |>
        dplyr::mutate(ponderacion = valor_ponderacion_acumulado / total_cesta_acumulado) |>
        dplyr::mutate(precio_por_ponderacion = precio * ponderacion)
    }

    precios_ponderados_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha)),
                      dplyr::across(dplyr::all_of(.col_flujo)),
                      dplyr::across(dplyr::all_of(.col_nombres))) |>
      dplyr::summarise(precios_ponderados = sum(precio_por_ponderacion, na.rm = TRUE)) |>
      dplyr::ungroup()
  }

  # PROCESAMIENTO FINAL (común para ambos métodos)
  if(is.null(.flujo_seleccionado)){
    precios_ponderados_df <-
      precios_ponderados_df |>
      tidyr::pivot_wider(names_from = !!rlang::sym(.col_flujo), values_from = precios_ponderados) |>
      dplyr::rename(precios_ponderados_export = !!rlang::sym(.export),
                    precios_ponderados_import = !!rlang::sym(.import))

    if(.get_rri == TRUE){
      precios_ponderados_df <-
        precios_ponderados_df |>
        dplyr::mutate(rri = precios_ponderados_export / precios_ponderados_import)
    }

    if(.remove_na == TRUE){
      precios_ponderados_df <-
        precios_ponderados_df |>
        dplyr::filter(precios_ponderados_export != 0,
                      precios_ponderados_import != 0)
    }
  }else{
    precios_ponderados_df <-
      precios_ponderados_df |>
      dplyr::filter(!!rlang::sym(.col_flujo) == .flujo_seleccionado)

    if(.remove_na == TRUE){
      precios_ponderados_df <-
        precios_ponderados_df |>
        dplyr::filter(precios_ponderados != 0)
    }
  }

  return(precios_ponderados_df)
}

# Precios ponderados optimizado con duckplyr ----
#' Calcula índices de precios (versión optimizada).
#' @param .datos_df Data frame que contiene los datos de importación y exportación.
#' @param .año_referencia Número. Año para tomar como referencia.
#' @param .meses_atras_acumulado Número. Número de meses sobre los que se calcula el acumulado.
#' @param .variable_utilizada_cesta Cadena de caracteres. Permite "valor_estadistico" o "peso".
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas.
#' @param .col_flujo Cadena de caracteres. Nombre de la columna del dataframe que contiene el tipo de flujo.
#' @param .col_taric Cadena de caracteres. Nombre de la columna del dataframe que contiene el codigo TARIC.
#' @param .col_valor_estadistico Cadena de caracteres. Nombre de la columna de valores estadísticos.
#' @param .col_peso Cadena de caracteres. Nombre de la columna del peso.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna de nombres para agrupación.
#' @param .flujo_seleccionado Cadena de caracteres. Filtro específico de flujo.
#' @param .export Nombre que denota las exportaciones. Por defecto "E".
#' @param .import Nombre que denota las importaciones. Por defecto "I".
#' @param .get_rri Valor lógico. Si TRUE, calcula RRI.
#' @param .remove_na Valor lógico. Si TRUE, elimina valores NA.
#' @param .use_duckplyr Valor lógico. Si TRUE, usa duckplyr para optimización.
#' @export
get_precios_ponderados_duckplyr <- function(.datos_df,
                                   .año_referencia = NULL,
                                   .meses_atras_acumulado = 12,
                                   .variable_utilizada_cesta = "valor_estadistico",
                                   .col_fecha = "fecha",
                                   .col_flujo = "flujo",
                                   .col_taric = "taric",
                                   .col_valor_estadistico = "valor_estadistico",
                                   .col_peso = "peso",
                                   .col_nombres = NULL,
                                   .flujo_seleccionado = NULL,
                                   .export = "E",
                                   .import = "I",
                                   .get_rri = TRUE,
                                   .remove_na = TRUE,
                                   .use_duckplyr = TRUE) {

  # Validaciones iniciales
  stopifnot(
    "El input debe ser un data frame." = is.data.frame(.datos_df),
    "No existe la columna señalada para la fecha." = .col_fecha %in% names(.datos_df),
    "No existe la columna señalada para los valores." = .col_valor_estadistico %in% names(.datos_df),
    "No existe la columna señalada para el peso." = .col_peso %in% names(.datos_df),
    "No existe la columna señalada para el código TARIC." = .col_taric %in% names(.datos_df),
    "No existe la columna señalada para el flujo." = .col_flujo %in% names(.datos_df)
  )

  # Activar duckplyr si está disponible y solicitado
  if (.use_duckplyr && requireNamespace("duckplyr", quietly = TRUE)) {
    .datos_df <- duckplyr::as_duckplyr_df(.datos_df)
  }

  # Conversión y limpieza de fechas optimizada
  if (!inherits(.datos_df[[.col_fecha]], "Date")) {
    warning("Se ha convertido la columna de fechas a formato fecha.")
    .datos_df[[.col_fecha]] <- as.Date(.datos_df[[.col_fecha]])
  }

  .datos_df <- .datos_df |> dplyr::filter(!is.na(!!rlang::sym(.col_fecha)))

  # Calcular rango de fechas
  date_range <- .datos_df |>
    dplyr::summarise(
      min_date = min(!!rlang::sym(.col_fecha), na.rm = TRUE),
      max_date = max(!!rlang::sym(.col_fecha), na.rm = TRUE)
    ) |>
    dplyr::collect()

  if (any(is.infinite(c(date_range$min_date, date_range$max_date)))) {
    stop("Las fechas no tienen un formato válido.")
  }

  # Agregación inicial optimizada
  group_cols <- c(.col_fecha, .col_taric, .col_flujo, .col_nombres)
  group_cols <- group_cols[!is.null(group_cols)]

  .datos_df <- .datos_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      !!rlang::sym(.col_valor_estadistico) := sum(!!rlang::sym(.col_valor_estadistico), na.rm = TRUE),
      !!rlang::sym(.col_peso) := sum(!!rlang::sym(.col_peso), na.rm = TRUE),
      .groups = "drop"
    )

  # Completar series temporales
  date_seq <- seq.Date(from = date_range$min_date, to = date_range$max_date, by = "month")

  series_cols <- setdiff(group_cols, .col_fecha)
  .datos_df <- .datos_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(series_cols))) |>
    tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
    tidyr::replace_na(setNames(list(0, 0), c(.col_valor_estadistico, .col_peso))) |>
    dplyr::ungroup()

  # Seleccionar variable de cesta
  if (.variable_utilizada_cesta == "valor_estadistico") {
    .variable_utilizada_cesta <- .col_valor_estadistico
  } else if (.variable_utilizada_cesta == "peso") {
    .variable_utilizada_cesta <- .col_peso
  }

  # Calcular precios
  .datos_df <- .datos_df |>
    dplyr::mutate(precio = dplyr::case_when(
      !!rlang::sym(.col_peso) > 0 ~ !!rlang::sym(.col_valor_estadistico) / !!rlang::sym(.col_peso),
      TRUE ~ 0
    ))

  # Calcular ponderaciones
  if (!is.null(.año_referencia)) {
    # Ponderación con año de referencia (optimizada)
    ponderacion_referencia <- .datos_df |>
      dplyr::filter(lubridate::year(!!rlang::sym(.col_fecha)) == .año_referencia) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(.col_taric, .col_nombres, .col_flujo)))) |>
      dplyr::summarise(valor_ponderacion = sum(!!rlang::sym(.variable_utilizada_cesta), na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(
        total_cesta = sum(valor_ponderacion, na.rm = TRUE),
        ponderacion = valor_ponderacion / total_cesta
      )

    .datos_df <- .datos_df |>
      dplyr::left_join(ponderacion_referencia,
                       by = c(.col_taric, .col_nombres, .col_flujo)[!is.null(c(.col_taric, .col_nombres, .col_flujo))]) |>
      dplyr::mutate(precio_por_ponderacion = precio * ponderacion)

  } else {
    # Ponderación móvil optimizada usando funciones de ventana
    .datos_df <- .datos_df |>
      dplyr::arrange(dplyr::across(dplyr::all_of(c(.col_nombres, .col_taric, .col_flujo, .col_fecha)))) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(.col_nombres, .col_taric, .col_flujo)))) |>
      dplyr::mutate(
        valor_ponderacion_acumulado = slider::slide_dbl(
          !!rlang::sym(.variable_utilizada_cesta),
          sum,
          .before = .meses_atras_acumulado - 1,
          .complete = FALSE
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(.col_fecha, .col_flujo, .col_nombres)))) |>
      dplyr::mutate(total_cesta = sum(!!rlang::sym(.variable_utilizada_cesta), na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(.col_nombres, .col_flujo)))) |>
      dplyr::mutate(
        total_cesta_acumulado = slider::slide_dbl(
          total_cesta,
          sum,
          .before = .meses_atras_acumulado - 1,
          .complete = FALSE
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        ponderacion = dplyr::case_when(
          total_cesta_acumulado > 0 ~ valor_ponderacion_acumulado / total_cesta_acumulado,
          TRUE ~ 0
        ),
        precio_por_ponderacion = precio * ponderacion
      )
  }

  # Agregación final optimizada
  final_group_cols <- c(.col_fecha, .col_flujo, .col_nombres)
  final_group_cols <- final_group_cols[!is.null(final_group_cols)]

  precios_ponderados_df <- .datos_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(final_group_cols))) |>
    dplyr::summarise(precios_ponderados = sum(precio_por_ponderacion, na.rm = TRUE), .groups = "drop")

  # Procesamiento final
  if (is.null(.flujo_seleccionado)) {
    precios_ponderados_df <- precios_ponderados_df |>
      tidyr::pivot_wider(names_from = !!rlang::sym(.col_flujo), values_from = precios_ponderados) |>
      dplyr::rename(
        precios_ponderados_export = !!rlang::sym(.export),
        precios_ponderados_import = !!rlang::sym(.import)
      )

    if (.get_rri) {
      precios_ponderados_df <- precios_ponderados_df |>
        dplyr::mutate(rri = precios_ponderados_export / precios_ponderados_import)
    }

    if (.remove_na) {
      precios_ponderados_df <- precios_ponderados_df |>
        dplyr::filter(
          !is.na(precios_ponderados_export) & precios_ponderados_export != 0,
          !is.na(precios_ponderados_import) & precios_ponderados_import != 0
        )
    }
  } else {
    precios_ponderados_df <- precios_ponderados_df |>
      dplyr::filter(!!rlang::sym(.col_flujo) == .flujo_seleccionado)

    if (.remove_na) {
      precios_ponderados_df <- precios_ponderados_df |>
        dplyr::filter(!is.na(precios_ponderados) & precios_ponderados != 0)
    }
  }

  # Convertir de vuelta si era duckplyr
  if (.use_duckplyr && inherits(precios_ponderados_df, "duckplyr_df")) {
    precios_ponderados_df <- dplyr::collect(precios_ponderados_df)
  }

  return(precios_ponderados_df)
}

# Flujos en términos reales----
#' Calcula los flujos en términos reales. Devuelve un dataframe con la evolución de los índices de precios y del valor de las exportaciones en términos reales.
#' @param .datos_df Data frame que contiene los datos de importación y exportación sobre los que se quiere calcular el índice. La idea es que incluya `.col_fecha`, `.col_flujo`, `.col_taric`, `.col_valor_estadistico`, `.col_peso` y opcionalmente, `.col_nombres`. Si se ha obtenido a través de la función `get_precios_ponderados`, lo detecta y se ahorra el cálculo.
#' @param .año_referencia Número. Año para tomar como referencia y hacer la cesta de bienes. En su defecto (i.e. si `NULL`), se realizarán las ponderaciones con una cesta móvil que evoluciona en base a las observaciones de los últimos `.meses_atras_acumulado` meses. Por defecto, `NULL`.
#' @param .año_base Número. Año para tomar como base (toma la primera observación del año en caso de que los datos no sean anuales (i.e. enero si son mensuales)) y establecer índice 100 a los flujos reales. En su defecto (i.e. si `NULL`), no se calcula índice sino que `flujos_reales` refleja los `kilogramos` de la cesta compuesta ficticia. Por defecto, `NULL`.
#' @param .meses_atras_acumulado Número. Número de meses sobre los que se calcula el acumulado para obtener las cestas. Por defecto, `12`.
#' @param .variable_utilizada_cesta Cadena de caracteres. Permite `"valor_estadistico"` o `"peso"`. Por defecto, `"valor_estadistico"`.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas (en formato fecha). Por defecto, `"fecha"`.
#' @param .col_flujo Cadena de caracteres. Nombre de la columna del dataframe que contiene el tipo de flujo. Por defecto, `"flujo"`.
#' @param .col_taric Cadena de caracteres. Nombre de la columna del dataframe que contiene el codigo TARIC de cada producto. Por defecto, `"taric"`.
#' @param .col_valor_estadistico Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores estadísticos. Por defecto, `"valor_estadistico"`.
#' @param .col_peso Cadena de caracteres. Nombre de la columna del dataframe que contiene el peso. Es importante que estén definidos en la misma unidad. Por defecto, `"peso"`.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres sobre los que se quiere agrupar para calcular los índices. Por ejemplo, si se quiere calcular el índice de precios para cada provincia se podrá especificar aquí. Admite varias columnas (p.ej. `c("provincia", "pais")`). Por defecto, `NULL`.
#' @param .export Nombre que denota las exportaciones en la columna `.col_flujo`. Por defecto, `"E"`.
#' @param .import Nombre que denota las importaciones en la columna `.col_flujo`. Por defecto, `"I"`.
#' @param .metodo Si se establece `.metodo = "SQL"` usará SQL para una mayor eficiencia. Deseable cuando `.datos_df` es grande. Si se establece en `NULL` realizará los cálculos usando únicamente R base y el tidyverse. Por defecto, `"SQL"`.
#' @export
get_flujos_reales <- function(.datos_df,
                              .año_referencia = NULL,
                              .año_base = NULL,
                              .meses_atras_acumulado = 12,
                              .variable_utilizada_cesta = "valor_estadistico",
                              .col_fecha = "fecha",
                              .col_flujo = "flujo",
                              .col_taric = "taric",
                              .col_valor_estadistico = "valor_estadistico",
                              .col_peso = "peso",
                              .col_nombres = NULL,
                              .export = "E",
                              .import = "I",
                              .remove_na = TRUE,
                              .metodo = NULL) {

  # MÉTODO SQL
  if (.metodo == "SQL") {

    # Verificar que duckdb esté disponible
    if (!requireNamespace("duckdb", quietly = TRUE)) {
      stop("El paquete duckdb es necesario para usar el método SQL. Instálalo con: install.packages('duckdb')")
    }

    # Crear conexión temporal en memoria
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    # Verificar si ya tenemos precios ponderados
    if("precios_ponderados_export" %in% colnames(.datos_df) && "precios_ponderados_import" %in% colnames(.datos_df)){
      if("rri" %in% colnames(.datos_df)){
        precios_ponderados_df <- .datos_df |> dplyr::select(-rri)
      } else {
        precios_ponderados_df <- .datos_df
      }
    } else {
      # Calcular precios ponderados usando método SQL
      if (.variable_utilizada_cesta == "valor_estadistico"){
        .variable_utilizada_cesta <- .col_valor_estadistico
      } else if (.variable_utilizada_cesta == "peso"){
        .variable_utilizada_cesta  <- .col_peso
      }

      precios_ponderados_df <-
        .datos_df |>
        victorgmtools::get_precios_ponderados(.año_referencia = .año_referencia,
                                              .meses_atras_acumulado = .meses_atras_acumulado,
                                              .variable_utilizada_cesta = .variable_utilizada_cesta,
                                              .col_fecha = .col_fecha,
                                              .col_flujo = .col_flujo,
                                              .col_valor_estadistico = .col_valor_estadistico,
                                              .col_taric = .col_taric,
                                              .col_nombres = .col_nombres,
                                              .export = .export,
                                              .import = .import,
                                              .get_rri = FALSE,
                                              .remove_na = FALSE,
                                              .metodo = "SQL")  # Usar SQL también aquí
    }

    # Escribir datos a tablas temporales
    DBI::dbWriteTable(con, "precios_ponderados", precios_ponderados_df, overwrite = TRUE)
    DBI::dbWriteTable(con, "datos_originales", .datos_df, overwrite = TRUE)

    # PASO 1: Pivotar precios ponderados y agregar valor estadístico en SQL
    pivot_query <- paste0("
      CREATE TEMP TABLE precios_pivotados AS
      SELECT
        ", .col_fecha, ",
        ", ifelse(!is.null(.col_nombres), paste0(.col_nombres, ","), ""), "
        'E' as flujo,
        precios_ponderados_export as precios_ponderados
      FROM precios_ponderados
      WHERE precios_ponderados_export IS NOT NULL

      UNION ALL

      SELECT
        ", .col_fecha, ",
        ", ifelse(!is.null(.col_nombres), paste0(.col_nombres, ","), ""), "
        'I' as flujo,
        precios_ponderados_import as precios_ponderados
      FROM precios_ponderados
      WHERE precios_ponderados_import IS NOT NULL
    ")

    DBI::dbExecute(con, pivot_query)

    # PASO 2: Agregar valores estadísticos
    group_cols_valores <- c(.col_fecha, .col_flujo)
    if (!is.null(.col_nombres)) group_cols_valores <- c(group_cols_valores, .col_nombres)

    valor_estadistico_query <- paste0("
      CREATE TEMP TABLE valores_agregados AS
      SELECT
        ", paste(group_cols_valores, collapse = ", "), ",
        SUM(", .col_valor_estadistico, ") as valor_estadistico
      FROM datos_originales
      GROUP BY ", paste(group_cols_valores, collapse = ", ")
    )

    DBI::dbExecute(con, valor_estadistico_query)

    # PASO 3: Unir y calcular flujos reales
    join_conditions <- paste(paste0("pp.", group_cols_valores, " = va.", group_cols_valores), collapse = " AND ")

    flujos_reales_query <- paste0("
      SELECT
        pp.*,
        va.valor_estadistico,
        CASE
          WHEN pp.precios_ponderados != 0
          THEN va.valor_estadistico / pp.precios_ponderados
          ELSE NULL
        END as flujos_reales
      FROM precios_pivotados pp
      LEFT JOIN valores_agregados va ON ", join_conditions
    )

    if (.remove_na == TRUE) {
      flujos_reales_query <- paste0(flujos_reales_query, "
        WHERE pp.precios_ponderados != 0
      ")
    }

    flujos_reales_df <- DBI::dbGetQuery(con, flujos_reales_query)

    # PASO 4: Aplicar año base si está especificado
    if (!is.null(.año_base)) {
      # Escribir flujos reales a tabla temporal
      DBI::dbWriteTable(con, "flujos_reales", flujos_reales_df, overwrite = TRUE)

      # Calcular ponderación base
      group_cols_base <- c(.col_flujo)
      if (!is.null(.col_nombres)) group_cols_base <- c(group_cols_base, .col_nombres)

      ponderacion_base_query <- paste0("
        CREATE TEMP TABLE ponderacion_base AS
        SELECT
          ", paste(group_cols_base, collapse = ", "), ",
          SUM(flujos_reales) as valor_enero_base
        FROM flujos_reales
        WHERE strftime('%Y', ", .col_fecha, ") = '", .año_base, "'
          AND ", .col_fecha, " = (
            SELECT MIN(", .col_fecha, ")
            FROM flujos_reales
            WHERE strftime('%Y', ", .col_fecha, ") = '", .año_base, "'
          )
        GROUP BY ", paste(group_cols_base, collapse = ", ")
      )

      DBI::dbExecute(con, ponderacion_base_query)

      # Aplicar normalización
      join_base_conditions <- paste(paste0("fr.", group_cols_base, " = pb.", group_cols_base), collapse = " AND ")

      flujos_normalizados_query <- paste0("
        SELECT
          fr.", .col_fecha, ",
          fr.flujo,
          ", ifelse(!is.null(.col_nombres), paste0("fr.", .col_nombres, ","), ""), "
          fr.valor_estadistico,
          fr.precios_ponderados,
          CASE
            WHEN pb.valor_enero_base != 0
            THEN (fr.flujos_reales / pb.valor_enero_base) * 100
            ELSE fr.flujos_reales
          END as flujos_reales
        FROM flujos_reales fr
        LEFT JOIN ponderacion_base pb ON ", join_base_conditions
      )

      flujos_reales_df <- DBI::dbGetQuery(con, flujos_normalizados_query)
    }

  } else {

    # MÉTODO ORIGINAL (R puro)
    if("precios_ponderados_export" %in% colnames(.datos_df) && "precios_ponderados_import" %in% colnames(.datos_df)){
      if("rri" %in% colnames(.datos_df)){
        precios_ponderados_df <-
          .datos_df |>
          dplyr::select(-rri)
      }else{
        precios_ponderados_df <-
          .datos_df
      }
    }else{
      if (.variable_utilizada_cesta == "valor_estadistico"){
        .variable_utilizada_cesta <- .col_valor_estadistico
      } else if (.variable_utilizada_cesta == "peso"){
        .variable_utilizada_cesta  <- .col_peso
      }

      precios_ponderados_df <-
        .datos_df |>
        victorgmtools::get_precios_ponderados(.año_referencia = .año_referencia,
                                              .meses_atras_acumulado = .meses_atras_acumulado,
                                              .variable_utilizada_cesta = .variable_utilizada_cesta,
                                              .col_fecha = .col_fecha,
                                              .col_flujo = .col_flujo,
                                              .col_valor_estadistico = .col_valor_estadistico,
                                              .col_taric = .col_taric,
                                              .col_nombres = .col_nombres,
                                              .export = .export,
                                              .import = .import,
                                              .get_rri = FALSE,
                                              .remove_na = FALSE)
    }

    precios_ponderados_df <-
      precios_ponderados_df |>
      tidyr::pivot_longer(c(precios_ponderados_export, precios_ponderados_import), names_to = "flujo", values_to = "precios_ponderados") |>
      dplyr::mutate(flujo = dplyr::case_when(flujo == "precios_ponderados_export" ~ "E",
                                             flujo == "precios_ponderados_import" ~ "I",
                                             TRUE ~ NA))

    valor_estadistico_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha)),
                      dplyr::across(dplyr::all_of(.col_flujo)),
                      dplyr::across(dplyr::all_of(.col_nombres))) |>
      dplyr::summarise(valor_estadistico = sum(valor_estadistico, na.rm = TRUE)) |>
      dplyr::ungroup()

    flujos_reales_df <-
      merge(precios_ponderados_df, valor_estadistico_df) |>
      dplyr::mutate(flujos_reales = valor_estadistico / precios_ponderados) |>
      dplyr::relocate(c(!!rlang::sym(.col_fecha), !!rlang::sym(.col_flujo), valor_estadistico, precios_ponderados, flujos_reales)) |>
      dplyr::relocate(c(valor_estadistico, precios_ponderados, flujos_reales),
                      .after = last_col())

    if(.remove_na == TRUE){
      flujos_reales_df <-
        flujos_reales_df |>
        dplyr::filter(precios_ponderados != 0)
    }

    if(!is.null(.año_base)){
      ponderacion_base <-
        flujos_reales_df |>
        dplyr::mutate(año = as.Date(!!rlang::sym(.col_fecha)) |> lubridate::year()) |>
        dplyr::filter(año == .año_base,
                      fecha == min(fecha, na.rm = TRUE)) |>
        dplyr::select(-año) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)),
                        dplyr::across(dplyr::all_of(.col_flujo))) |>
        dplyr::reframe(valor_enero_base = sum(flujos_reales, na.rm = TRUE))

      flujos_reales_df <-
        flujos_reales_df |>
        dplyr::left_join(ponderacion_base) |>
        dplyr::mutate(flujos_reales = (flujos_reales / valor_enero_base) * 1e2) |>
        dplyr::select(-valor_enero_base)
    }
  }

  # Asegurar orden correcto de columnas (común para ambos métodos)
  final_cols <- c(.col_fecha, .col_flujo)
  if (!is.null(.col_nombres)) final_cols <- c(final_cols, .col_nombres)
  final_cols <- c(final_cols, "valor_estadistico", "precios_ponderados", "flujos_reales")

  # Seleccionar solo las columnas que existen
  existing_cols <- intersect(final_cols, names(flujos_reales_df))
  flujos_reales_df <- flujos_reales_df[, existing_cols, drop = FALSE]

  return(flujos_reales_df)
}

# Flujos reales optimizado con duckplyr ----
#' Calcula los flujos en términos reales (versión optimizada).
#' @param .datos_df Data frame que contiene los datos de importación y exportación.
#' @param .año_referencia Número. Año para tomar como referencia.
#' @param .año_base Número. Año para tomar como base.
#' @param .meses_atras_acumulado Número. Número de meses para acumulado.
#' @param .variable_utilizada_cesta Cadena de caracteres. Variable para cesta.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna de fechas.
#' @param .col_flujo Cadena de caracteres. Nombre de la columna de flujo.
#' @param .col_taric Cadena de caracteres. Nombre de la columna TARIC.
#' @param .col_valor_estadistico Cadena de caracteres. Nombre de la columna de valores.
#' @param .col_peso Cadena de caracteres. Nombre de la columna de peso.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna de nombres.
#' @param .export Nombre que denota exportaciones.
#' @param .import Nombre que denota importaciones.
#' @param .remove_na Valor lógico. Si TRUE, elimina NAs.
#' @param .use_duckplyr Valor lógico. Si TRUE, usa duckplyr.
#' @export
get_flujos_reales_duckplyr <- function(.datos_df,
                              .año_referencia = NULL,
                              .año_base = NULL,
                              .meses_atras_acumulado = 12,
                              .variable_utilizada_cesta = "valor_estadistico",
                              .col_fecha = "fecha",
                              .col_flujo = "flujo",
                              .col_taric = "taric",
                              .col_valor_estadistico = "valor_estadistico",
                              .col_peso = "peso",
                              .col_nombres = NULL,
                              .export = "E",
                              .import = "I",
                              .remove_na = TRUE,
                              .use_duckplyr = TRUE) {

  # Activar duckplyr si está disponible y solicitado
  if (.use_duckplyr && requireNamespace("duckplyr", quietly = TRUE)) {
    .datos_df <- duckplyr::as_duckplyr_df(.datos_df)
  }

  # Verificar si ya tenemos precios ponderados
  has_precios_ponderados <- all(c("precios_ponderados_export", "precios_ponderados_import") %in% colnames(.datos_df))

  if (has_precios_ponderados) {
    if ("rri" %in% colnames(.datos_df)) {
      precios_ponderados_df <- .datos_df |> dplyr::select(-rri)
    } else {
      precios_ponderados_df <- .datos_df
    }
  } else {
    # Calcular precios ponderados usando la función optimizada
    if (.variable_utilizada_cesta == "valor_estadistico") {
      .variable_utilizada_cesta <- .col_valor_estadistico
    } else if (.variable_utilizada_cesta == "peso") {
      .variable_utilizada_cesta <- .col_peso
    }

    precios_ponderados_df <- get_precios_ponderados(
      .datos_df,
      .año_referencia = .año_referencia,
      .meses_atras_acumulado = .meses_atras_acumulado,
      .variable_utilizada_cesta = .variable_utilizada_cesta,
      .col_fecha = .col_fecha,
      .col_flujo = .col_flujo,
      .col_valor_estadistico = .col_valor_estadistico,
      .col_taric = .col_taric,
      .col_nombres = .col_nombres,
      .export = .export,
      .import = .import,
      .get_rri = FALSE,
      .remove_na = FALSE,
      .use_duckplyr = .use_duckplyr
    )
  }

  # Asegurar que precios_ponderados_df esté en duckplyr si es necesario
  if (.use_duckplyr && requireNamespace("duckplyr", quietly = TRUE) && !inherits(precios_ponderados_df, "duckplyr_df")) {
    precios_ponderados_df <- duckplyr::as_duckplyr_df(precios_ponderados_df)
  }

  # Pivotar precios ponderados de manera optimizada
  precios_pivotados <- precios_ponderados_df |>
    tidyr::pivot_longer(
      cols = c(precios_ponderados_export, precios_ponderados_import),
      names_to = "flujo_tipo",
      values_to = "precios_ponderados"
    ) |>
    dplyr::mutate(
      flujo = dplyr::case_when(
        flujo_tipo == "precios_ponderados_export" ~ .export,
        flujo_tipo == "precios_ponderados_import" ~ .import,
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(-flujo_tipo) |>
    dplyr::filter(!is.na(!!rlang::sym(.col_flujo)), !is.na(precios_ponderados))

  # Calcular valores estadísticos agregados de manera optimizada
  if (.use_duckplyr && inherits(.datos_df, "duckplyr_df")) {
    datos_originales <- .datos_df
  } else if (.use_duckplyr && requireNamespace("duckplyr", quietly = TRUE)) {
    datos_originales <- duckplyr::as_duckplyr_df(.datos_df)
  } else {
    datos_originales <- .datos_df
  }

  group_cols_valores <- c(.col_fecha, .col_flujo, .col_nombres)
  group_cols_valores <- group_cols_valores[!is.null(group_cols_valores)]

  valor_estadistico_df <- datos_originales |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols_valores))) |>
    dplyr::summarise(valor_estadistico = sum(!!rlang::sym(.col_valor_estadistico), na.rm = TRUE), .groups = "drop")

  # Unir y calcular flujos reales de manera optimizada
  join_cols <- intersect(names(precios_pivotados), names(valor_estadistico_df))
  join_cols <- setdiff(join_cols, c("precios_ponderados", "valor_estadistico"))

  flujos_reales_df <- precios_pivotados |>
    dplyr::left_join(valor_estadistico_df, by = join_cols) |>
    dplyr::mutate(
      flujos_reales = dplyr::case_when(
        !is.na(precios_ponderados) & precios_ponderados != 0 ~ valor_estadistico / precios_ponderados,
        TRUE ~ NA_real_
      )
    )

  # Aplicar filtros de NA si es necesario
  if (.remove_na) {
    flujos_reales_df <- flujos_reales_df |>
      dplyr::filter(
        !is.na(precios_ponderados) & precios_ponderados != 0,
        !is.na(flujos_reales)
      )
  }

  # Aplicar año base si está especificado
  if (!is.null(.año_base)) {
    # Calcular ponderación base de manera optimizada
    group_cols_base <- c(.col_flujo, .col_nombres)
    group_cols_base <- group_cols_base[!is.null(group_cols_base)]

    ponderacion_base <- flujos_reales_df |>
      dplyr::filter(lubridate::year(!!rlang::sym(.col_fecha)) == .año_base) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols_base))) |>
      dplyr::slice_min(!!rlang::sym(.col_fecha), n = 1) |>  # Primera fecha del año base
      dplyr::summarise(valor_enero_base = sum(flujos_reales, na.rm = TRUE), .groups = "drop")

    # Aplicar normalización
    flujos_reales_df <- flujos_reales_df |>
      dplyr::left_join(ponderacion_base, by = intersect(names(flujos_reales_df), names(ponderacion_base))) |>
      dplyr::mutate(
        flujos_reales = dplyr::case_when(
          !is.na(valor_enero_base) & valor_enero_base != 0 ~ (flujos_reales / valor_enero_base) * 100,
          TRUE ~ flujos_reales
        )
      ) |>
      dplyr::select(-valor_enero_base)
  }

  # Convertir de vuelta si era duckplyr
  if (.use_duckplyr && inherits(flujos_reales_df, "duckplyr_df")) {
    flujos_reales_df <- dplyr::collect(flujos_reales_df)
  }

  return(flujos_reales_df)
}

# Descomposición de la tasa de variación----
#' Partiendo de un dataframe con valor nominal, precios ponderados y flujos reales, devuelve un dataframe con la tasa de variación en términos nominales y la contribución del precio y del volumen.
#' @param .datos_df Data frame que contiene los datos de importación y exportación sobre los que se quiere calcular las tasas de variación y las contribuciones. La idea es que incluya `.col_fecha`, `.col_flujo`, `.col_valor_estadistico`, `.col_precios_ponderados`, `.col_flujos_reales` y opcionalmente, `.col_nombres`.
#' @param .meses_atras Número. Número de meses sobre los que se quiere calcular la tasa de variación. Por defecto, `12`.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas (en formato fecha). Por defecto, `"fecha"`.
#' @param .col_flujo Cadena de caracteres. Nombre de la columna del dataframe que contiene el tipo de flujo. Por defecto, `"flujo"`.
#' @param .col_valor_estadistico Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores estadísticos. Por defecto, `"valor_estadistico"`.
#' @param .col_precios_ponderados Cadena de caracteres. Nombre de la columna del dataframe que contiene los precios ponderados. Por defecto, `"precios_ponderados"`.
#' @param .col_flujos_reales Cadena de caracteres. Nombre de la columna del dataframe que contiene los flujos reales. Por defecto, `"flujos_reales"`.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres sobre los que se quiere agrupar para calcular los índices. Por ejemplo, si se quiere calcular el índice de precios para cada provincia se podrá especificar aquí. Admite varias columnas (p.ej. `c("provincia", "pais")`). Por defecto, `NULL`.
#' @export
get_descomposicion_tv <- function(.datos_df,
                                  .meses_atras = 12,
                                  .col_fecha = "fecha",
                                  .col_flujo = "flujo",
                                  .col_valor_estadistico = "valor_estadistico",
                                  .col_precios_ponderados = "precios_ponderados",
                                  .col_flujos_reales = "flujos_reales",
                                  .col_nombres = NULL,
                                  .remove_na = TRUE) {
  descomposicion_tv_df <-
    .datos_df |>
    victorgmtools::get_tasa_variacion(.periodos_atras = .meses_atras,
                                      .col_valores = .col_valor_estadistico,
                                      .col_nombres = .col_nombres,
                                      .col_fecha = .col_fecha,
                                      .col_flujo = .col_flujo) |>
    dplyr::rename_with(~ "tv_nominal", dplyr::matches("^tv_.*_meses$")) |>
    victorgmtools::get_tasa_variacion(.periodos_atras = .meses_atras,
                                      .col_valores = .col_precios_ponderados,
                                      .col_nombres = .col_nombres,
                                      .col_fecha = .col_fecha,
                                      .col_flujo = .col_flujo) |>
    dplyr::rename_with(~ "tv_precios", dplyr::matches("^tv_.*_meses$")) |>
    victorgmtools::get_tasa_variacion(.periodos_atras = .meses_atras,
                                      .col_valores = .col_flujos_reales,
                                      .col_nombres = .col_nombres,
                                      .col_fecha = .col_fecha,
                                      .col_flujo = .col_flujo) |>
    dplyr::rename_with(~ "tv_volumen", dplyr::matches("^tv_.*_meses$")) |>
    dplyr::mutate(contribucion_precios = (tv_precios / (tv_precios + tv_volumen)) * tv_nominal,
                  contribucion_volumen = (tv_volumen / (tv_precios + tv_volumen)) * tv_nominal) |>
    dplyr::relocate(c(!!rlang::sym(.col_fecha), !!rlang::sym(.col_flujo))) |>
    dplyr::relocate(c(tv_nominal, contribucion_precios, contribucion_volumen, tv_precios, tv_volumen),
                    .after = last_col())

  if(.remove_na == TRUE){
    descomposicion_tv_df <-
      descomposicion_tv_df |>
      dplyr::filter(!is.na(tv_nominal),
                    !is.nan(contribucion_precios),
                    !is.nan(contribucion_volumen))
  }

  return(descomposicion_tv_df)
}

# Gráfico de la evolución del precio de un producto----
#' Permite generar un gráfico para analizar la evolución del precio de un producto.
#' @param .datos_df Data frame que contiene los datos de importación y exportación. La idea es que incluya `.col_fecha`, `.col_flujo`, `.col_taric`, `.col_valor_estadistico`, `.col_peso` y opcionalmente, `.col_nombres`.
#' @param .taric_seleccionado Cadena de caracteres. Código TARIC del producto del cual se quiere representar la evolución de precios.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas (en formato fecha). Por defecto, `"fecha"`.
#' @param .col_flujo Cadena de caracteres. Nombre de la columna del dataframe que contiene el tipo de flujo. Por defecto, `"flujo"`.
#' @param .col_taric Cadena de caracteres. Nombre de la columna del dataframe que contiene el codigo TARIC de cada producto. Por defecto, `"taric"`.
#' @param .col_valor_estadistico Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores estadísticos. Por defecto, `"valor_estadistico"`.
#' @param .col_peso Cadena de caracteres. Nombre de la columna del dataframe que contiene el peso. Es importante que estén definidos en la misma unidad. Por defecto, `"peso"`.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres para hacer distintos gráficos por categorías. Por ejemplo, si se quiere representar la evolución de precios para cada provincia se podrá especificar aquí. Por defecto, `NULL`.
#' @param .nombres_por_facets Valor lógico. Permite establecer si se quiere agregar por nombres antes de calcular el índice de precios (en caso de `FALSE`) o si se quiere representar un gráfico para cada valor de la columna `.col_nombres` (en caso de `TRUE`). Por defecto, `FALSE`.
#' @param .flujo_seleccionado Cadena de caracteres. Permite filtrar por flujo, estableciendo por ejemplo, `.flujo_seleccionado = "E"`. Por defecto, `NULL`, que devuelve exportaciones e importaciones.
#' @param .export Nombre que denota las exportaciones en la columna `.col_flujo`. Por defecto, `"E"`.
#' @param .import Nombre que denota las importaciones en la columna `.col_flujo`. Por defecto, `"I"`.
#' @param .estatico_toggle Valor lógico. En caso de `TRUE` el gráfico no será interactivo. Por defecto, `FALSE`.
#' @param .fuente_letra Cadena de caracteres. Fuente tipográfica a utilizar en el gráfico. Por defecto, utiliza la fuente: `"Segoe UI"`. Otras opciones comunes: `"Arial"`, `"Times"`, `"Courier"`, `"Helvetica"`, etc.
#' @export
get_precio_producto_plt <- function(.datos_df,
                                    .taric_seleccionado,
                                    .col_fecha = "fecha",
                                    .col_flujo = "flujo",
                                    .col_taric = "taric",
                                    .col_valor_estadistico = "valor_estadistico",
                                    .col_peso = "peso",
                                    .col_nombres = NULL,
                                    .nombres_por_facets = FALSE,
                                    .export = "E",
                                    .import = "I",
                                    .flujo_seleccionado = NULL,
                                    .accuracy = 0.01,
                                    .legend_position = "bottom",
                                    .title = NULL,
                                    .title_size = 18,
                                    .subtitle = NULL,
                                    .subtitle_size = 14,
                                    .caption = NULL,
                                    .caption_size = 10,
                                    .estatico_toggle = FALSE,
                                    .fuente_letra = "Segoe UI") {
    if(!is.null(.flujo_seleccionado)) {
      .datos_df <-
        .datos_df |>
        dplyr::filter(.data[[.col_flujo]] == .flujo_seleccionado)
    }

    if(is.null(.col_nombres) && .nombres_por_facets == FALSE) {
      .datos_df <-
        .datos_df |>
        dplyr::group_by(!!rlang::sym(.col_fecha), !!rlang::sym(.col_flujo), !!rlang::sym(.col_taric)) |>
        dplyr::summarise(!!rlang::sym(.col_valor_estadistico) := sum(!!rlang::sym(.col_valor_estadistico), na.rm = TRUE),
                         !!rlang::sym(.col_peso) := sum(!!rlang::sym(.col_peso), na.rm = TRUE)) |>
        dplyr::ungroup()
    } else if(!is.null(.col_nombres)){
      .datos_df <-
        .datos_df |>
        dplyr::group_by(!!rlang::sym(.col_fecha), !!rlang::sym(.col_nombres), !!rlang::sym(.col_flujo), !!rlang::sym(.col_taric)) |>
        dplyr::summarise(!!rlang::sym(.col_valor_estadistico) := sum(!!rlang::sym(.col_valor_estadistico), na.rm = TRUE),
                         !!rlang::sym(.col_peso) := sum(!!rlang::sym(.col_peso), na.rm = TRUE)) |>
        dplyr::ungroup()
    }

    .datos_df <-
      .datos_df |>
      dplyr::filter(.data[[.col_taric]] == .taric_seleccionado) |>
      dplyr::mutate(precio = !!rlang::sym(.col_valor_estadistico) / !!rlang::sym(.col_peso))

    precio_producto_plt <-
      ggplot2::ggplot(data = .datos_df) +
      ggplot2::geom_hline(yintercept = 0)


    if(is.null(.col_nombres)){
      precio_producto_plt <-
        precio_producto_plt +
        ggplot2::geom_line(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                  y = precio,
                                                  color = .data[[.col_flujo]])) +
        ggiraph::geom_point_interactive(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                               y = precio,
                                                               color = .data[[.col_flujo]],
                                                               tooltip = paste0("Fecha: ", scales::date_format(format = "%m/%Y")(.data[[.col_fecha]]), "\n",
                                                                                "Flujo: ", .data[[.col_flujo]], "\n",
                                                                                "Precio: ", scales::number_format(accuracy = .accuracy, suffix = " €/kg", big.mark = ".", decimal.mark = ",")(precio))))
    } else{
      precio_producto_plt <-
        precio_producto_plt +
        ggplot2::geom_line(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                  y = precio,
                                                  color = .data[[.col_flujo]],
                                                  group = .data[[.col_nombres]])) +
        ggiraph::geom_point_interactive(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                               y = precio,
                                                               color = .data[[.col_flujo]],
                                                               tooltip = paste0("Fecha: ", scales::date_format(format = "%m/%Y")(.data[[.col_fecha]]), "\n",
                                                                                "Flujo: ", .data[[.col_flujo]], "\n",
                                                                                "Precio: ", scales::number_format(accuracy = .accuracy, suffix = " €/kg", big.mark = ".", decimal.mark = ",")(precio), "\n",
                                                                                .data[[.col_nombres]])))
    }


    if(!is.null(.col_nombres) && .nombres_por_facets == TRUE) {
      precio_producto_plt <-
        precio_producto_plt +
        ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(.col_nombres)))
    }

    precio_producto_plt <-
      precio_producto_plt |>
      victorgmtools::graficos_estilo_victorgm(
        .suffix = " €/kg",
        .title = .title,
        .title_size = .title_size,
        .subtitle = .subtitle,
        .subtitle_size = .subtitle_size,
        .caption = .caption,
        .caption_size = .caption_size,
        .legend_position = .legend_position,
        .fuente_letra = .fuente_letra)

    if(.estatico_toggle == FALSE){
      precio_producto_plt |>
        victorgmtools::to_giraph()
    } else {
      precio_producto_plt
    }
  }

# Gráfico de la evolución del precio de un producto----
#' Permite generar un gráfico para analizar la evolución del precio de un producto.
#' @param .datos_df Data frame que contiene los datos de importación y exportación. La idea es que incluya `.col_fecha`, `.col_flujo`, `.col_taric`, `.col_valor_estadistico`, `.col_peso` y opcionalmente, `.col_nombres`. En caso de que el dataframe provenga de la función `get_precios_ponderados` se ahorrará los cálculos intermedios.
#' @param .año_referencia Número. Año para tomar como referencia y hacer la cesta de bienes. En su defecto (i.e. si `NULL`), se realizarán las ponderaciones con una cesta móvil que evoluciona en base a las observaciones de los últimos `.meses_atras_acumulado` meses. Por defecto, `NULL`.
#' @param .meses_atras_acumulado Número. Número de meses sobre los que se calcula el acumulado para obtener las cestas. Por defecto, `12`.
#' @param .variable_utilizada_cesta Cadena de caracteres. Permite `"valor_estadistico"` o `"peso"`. Por defecto, `"valor_estadistico"`.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas (en formato fecha). Por defecto, `"fecha"`.
#' @param .col_flujo Cadena de caracteres. Nombre de la columna del dataframe que contiene el tipo de flujo. Por defecto, `"flujo"`.
#' @param .col_taric Cadena de caracteres. Nombre de la columna del dataframe que contiene el codigo TARIC de cada producto. Por defecto, `"taric"`.
#' @param .col_valor_estadistico Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores estadísticos. Por defecto, `"valor_estadistico"`.
#' @param .col_peso Cadena de caracteres. Nombre de la columna del dataframe que contiene el peso. Es importante que estén definidos en la misma unidad. Por defecto, `"peso"`.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres para hacer distintos gráficos por categorías. Por ejemplo, si se quiere representar la evolución de precios para cada provincia se podrá especificar aquí. Por defecto, `NULL`.
#' @param .nombres_por_facets Valor lógico. Permite establecer si se quiere agregar por nombres antes de calcular el índice de precios (en caso de `FALSE`) o si se quiere representar un gráfico para cada valor de la columna `.col_nombres` (en caso de `TRUE`). Por defecto, `FALSE`.
#' @param .flujo_seleccionado Cadena de caracteres. Permite filtrar por flujo, estableciendo por ejemplo, `.flujo_seleccionado = "E"`. Por defecto, `NULL`, que devuelve exportaciones e importaciones.
#' @param .export Nombre que denota las exportaciones en la columna `.col_flujo`. Por defecto, `"E"`.
#' @param .import Nombre que denota las importaciones en la columna `.col_flujo`. Por defecto, `"I"`.
#' @param .estatico_toggle Valor lógico. En caso de `TRUE` el gráfico no será interactivo. Por defecto, `FALSE`.
#' @param .representar_indices Valor lógico. En caso de `TRUE` el gráfico representa índices en base 100 para el primer período del gráfico, representando 3 índices: RRI, precio de exportaciones y precio de importaciones. En caso de `FALSE` representa únicamente la evolución de la RRI (sin calcular índices). Por defecto, `TRUE`.
#' @param .fuente_letra Cadena de caracteres. Fuente tipográfica a utilizar en el gráfico. Por defecto, utiliza la fuente: `"Segoe UI"`. Otras opciones comunes: `"Arial"`, `"Times"`, `"Courier"`, `"Helvetica"`, etc.
#' @export
get_rri_plt <- function(.datos_df,
                        .año_referencia = NULL,
                        .meses_atras_acumulado = 12,
                        .variable_utilizada_cesta = "valor_estadistico",
                        .col_fecha = "fecha",
                        .col_flujo = "flujo",
                        .col_taric = "taric",
                        .col_valor_estadistico = "valor_estadistico",
                        .col_peso = "peso",
                        .col_nombres = NULL,
                        .nombres_por_facets = FALSE,
                        .export = "E",
                        .import = "I",
                        .flujo_seleccionado = NULL,
                        .accuracy = 0.01,
                        .legend_position = "bottom",
                        .title = NULL,
                        .title_size = 18,
                        .subtitle = NULL,
                        .subtitle_size = 14,
                        .caption = NULL,
                        .caption_size = 10,
                        .estatico_toggle = FALSE,
                        .representar_indices = TRUE,
                        .fuente_letra = "Segoe UI") {
  if("precios_ponderados_export" %in% colnames(.datos_df) && "precios_ponderados_import" %in% colnames(.datos_df)){
    if("rri" %in% colnames(.datos_df)){
      .datos_df <-
        .datos_df |>
        dplyr::select(-rri)
    }else{
      .datos_df <-
        .datos_df
    }
  }else{
    if(!is.null(.col_nombres) && .nombres_por_facets == FALSE) {
      .datos_df <-
        .datos_df |>
        dplyr::group_by(!!rlang::sym(.col_fecha), !!rlang::sym(.col_flujo), !!rlang::sym(.col_nombres), !!rlang::sym(.col_taric)) |>
        dplyr::summarise(!!rlang::sym(.col_valor_estadistico) := sum(!!rlang::sym(.col_valor_estadistico), na.rm = TRUE),
                         !!rlang::sym(.col_peso) := sum(!!rlang::sym(.col_peso), na.rm = TRUE)) |>
        dplyr::ungroup()
    }

    .datos_df <-
      .datos_df |>
      victorgmtools::get_precios_ponderados(.año_referencia = .año_referencia,
                                            .meses_atras_acumulado = .meses_atras_acumulado,
                                            .variable_utilizada_cesta = .variable_utilizada_cesta,
                                            .col_fecha = .col_fecha,
                                            .col_flujo = .col_flujo,
                                            .col_taric = .col_taric,
                                            .col_valor_estadistico = .col_valor_estadistico,
                                            .col_peso = .col_peso,
                                            .col_nombres = .col_nombres,
                                            .export = .export,
                                            .import = .import,
                                            .get_rri = TRUE,
                                            .remove_na = TRUE)
  }

  .datos_df <-
    .datos_df |>
    dplyr::arrange(!!rlang::sym(.col_fecha)) |>
    dplyr::mutate(indice_rri = rri / dplyr::first(rri) * 100) |>
    dplyr::mutate(indice_precios_ponderados_export = precios_ponderados_export / dplyr::first(precios_ponderados_export) * 100) |>
    dplyr::mutate(indice_precios_ponderados_import = precios_ponderados_import / dplyr::first(precios_ponderados_import) * 100)

  if(.representar_indices == TRUE){
    rri_plt <-
      ggplot2::ggplot(data = .datos_df) +
      ggplot2::geom_hline(yintercept = 100) +
      ggplot2::geom_line(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                y = indice_rri,
                                                color = "Índice de la Relación Real de Intercambio")) +
      ggplot2::geom_line(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                y = indice_precios_ponderados_export,
                                                color = "Índice de precios de exportación")) +
      ggplot2::geom_line(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                y = indice_precios_ponderados_import,
                                                color = "Índice de precios de importación")) +
      ggiraph::geom_point_interactive(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                             y = indice_rri,
                                                             color = "Índice de la Relación Real de Intercambio",
                                                             tooltip = paste0("Fecha: ", scales::date_format(format = "%m/%Y")(.data[[.col_fecha]]), "\n",
                                                                              "RRI: ", scales::number_format(accuracy = .accuracy, suffix = "", big.mark = ".", decimal.mark = ",")(rri), "\n",
                                                                              "Índice RRI (base 100 en el primer periodo del gráfico): ", scales::number_format(accuracy = .accuracy, suffix = "", big.mark = ".", decimal.mark = ",")(indice_rri)))) +
      ggiraph::geom_point_interactive(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                             y = indice_precios_ponderados_export,
                                                             color = "Índice de precios de exportación",
                                                             tooltip = paste0("Fecha: ", scales::date_format(format = "%m/%Y")(.data[[.col_fecha]]), "\n",
                                                                              "Precio ponderado de las exportaciones: ", scales::number_format(accuracy = .accuracy, suffix = "", big.mark = ".", decimal.mark = ",")(precios_ponderados_export), "\n",
                                                                              "Índice precios exportación (base 100 en el primer periodo del gráfico): ", scales::number_format(accuracy = .accuracy, suffix = "", big.mark = ".", decimal.mark = ",")(indice_precios_ponderados_export)))) +
      ggiraph::geom_point_interactive(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                             y = indice_precios_ponderados_import,
                                                             color = "Índice de precios de importación",
                                                             tooltip = paste0("Fecha: ", scales::date_format(format = "%m/%Y")(.data[[.col_fecha]]), "\n",
                                                                              "Precio ponderado de las importaciones: ", scales::number_format(accuracy = .accuracy, suffix = "", big.mark = ".", decimal.mark = ",")(precios_ponderados_import), "\n",
                                                                              "Índice precios importación (base 100 en el primer periodo del gráfico): ", scales::number_format(accuracy = .accuracy, suffix = "", big.mark = ".", decimal.mark = ",")(indice_precios_ponderados_import)))) +
      ggplot2::scale_color_manual(values = c("Índice de la Relación Real de Intercambio" = "#2C3E50FF",
                                             "Índice de precios de importación" = "#18BC9CFF",
                                             "Índice de precios de exportación" = "#E31A1CFF"))
  } else{
    rri_plt <-
      ggplot2::ggplot(data = .datos_df) +
      ggplot2::geom_hline(yintercept = 1) +
      ggplot2::geom_line(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                y = rri,
                                                color = "Relación Real de Intercambio")) +
      ggiraph::geom_point_interactive(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                             y = rri,
                                                             color = "Relación Real de Intercambio",
                                                             tooltip = paste0("Fecha: ", scales::date_format(format = "%m/%Y")(.data[[.col_fecha]]), "\n",
                                                                              "RRI: ", scales::number_format(accuracy = .accuracy, suffix = "", big.mark = ".", decimal.mark = ",")(rri), "\n",
                                                                              "Índice RRI (base 100 en el primer periodo del gráfico): ", scales::number_format(accuracy = .accuracy, suffix = "", big.mark = ".", decimal.mark = ",")(indice_rri)))) +
      ggplot2::scale_color_manual(values = c("Relación Real de Intercambio" = "#2C3E50FF"))
  }

  if(!is.null(.col_nombres) && .nombres_por_facets == TRUE) {
    rri_plt <-
      rri_plt +
      ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(.col_nombres)))
  }

  rri_plt <-
    rri_plt |>
    victorgmtools::graficos_estilo_victorgm(
      .title = .title,
      .title_size = .title_size,
      .subtitle = .subtitle,
      .subtitle_size = .subtitle_size,
      .caption = .caption,
      .caption_size = .caption_size,
      .legend_position = .legend_position,
      .fuente_letra = .fuente_letra
    )

  if(.estatico_toggle == FALSE){
    rri_plt |>
      victorgmtools::to_giraph()
  } else {
    rri_plt
  }
}

# Gráfico de la evolución de los flujos en términos reales----
#' Permite generar un gráfico para analizar la evolución del precio de un producto.
#' @param .datos_df Data frame que contiene los datos de importación y exportación. La idea es que incluya `.col_fecha`, `.col_flujo`, `.col_taric`, `.col_valor_estadistico`, `.col_peso` y opcionalmente, `.col_nombres`.
#' @param .año_referencia Número. Año para tomar como referencia y hacer la cesta de bienes. En su defecto (i.e. si `NULL`), se realizarán las ponderaciones con una cesta móvil que evoluciona en base a las observaciones de los últimos `.meses_atras_acumulado` meses. Por defecto, `NULL`.
#' @param .año_base Número. Año para tomar como base (toma la primera observación del año en caso de que los datos no sean anuales (i.e. enero si son mensuales)) y establecer índice 100 a los flujos reales. En su defecto (i.e. si `NULL`), no se calcula índice sino que `flujos_reales` refleja los `kilogramos` de la cesta compuesta ficticia. Por defecto, `NULL`.
#' @param .meses_atras_acumulado Número. Número de meses sobre los que se calcula el acumulado para obtener las cestas. Por defecto, `12`.
#' @param .variable_utilizada_cesta Cadena de caracteres. Permite `"valor_estadistico"` o `"peso"`. Por defecto, `"valor_estadistico"`.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas (en formato fecha). Por defecto, `"fecha"`.
#' @param .col_flujo Cadena de caracteres. Nombre de la columna del dataframe que contiene el tipo de flujo. Por defecto, `"flujo"`.
#' @param .col_taric Cadena de caracteres. Nombre de la columna del dataframe que contiene el codigo TARIC de cada producto. Por defecto, `"taric"`.
#' @param .col_valor_estadistico Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores estadísticos. Por defecto, `"valor_estadistico"`.
#' @param .col_peso Cadena de caracteres. Nombre de la columna del dataframe que contiene el peso. Es importante que estén definidos en la misma unidad. Por defecto, `"peso"`.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres para hacer distintos gráficos por categorías. Por ejemplo, si se quiere representar la evolución de precios para cada provincia se podrá especificar aquí. Por defecto, `NULL`.
#' @param .nombres_por_facets Valor lógico. Permite establecer si se quiere agregar por nombres antes de calcular el índice de precios (en caso de `FALSE`) o si se quiere representar un gráfico para cada valor de la columna `.col_nombres` (en caso de `TRUE`). Por defecto, `FALSE`.
#' @param .flujo_seleccionado Cadena de caracteres. Permite filtrar por flujo, estableciendo por ejemplo, `.flujo_seleccionado = "E"`. Por defecto, `NULL`, que devuelve exportaciones e importaciones.
#' @param .export Nombre que denota las exportaciones en la columna `.col_flujo`. Por defecto, `"E"`.
#' @param .import Nombre que denota las importaciones en la columna `.col_flujo`. Por defecto, `"I"`.
#' @param .estatico_toggle Valor lógico. En caso de `TRUE` el gráfico no será interactivo. Por defecto, `FALSE`.
#' @param .representar_indices Valor lógico. En caso de `TRUE` el gráfico representa índices en base 100 para el primer período del gráfico, representando 3 índices: RRI, precio de exportaciones y precio de importaciones. En caso de `FALSE` representa únicamente la evolución de la RRI (sin calcular índices). Por defecto, `TRUE`.
#' @param .fuente_letra Cadena de caracteres. Fuente tipográfica a utilizar en el gráfico. Por defecto, utiliza la fuente: `"Segoe UI"`. Otras opciones comunes: `"Arial"`, `"Times"`, `"Courier"`, `"Helvetica"`, etc.
#' @export
get_flujos_reales_plt <- function(.datos_df,
                                  .año_referencia = NULL,
                                  .año_base = NULL,
                                  .meses_atras_acumulado = 12,
                                  .variable_utilizada_cesta = "valor_estadistico",
                                  .col_fecha = "fecha",
                                  .col_flujo = "flujo",
                                  .col_taric = "taric",
                                  .col_valor_estadistico = "valor_estadistico",
                                  .col_peso = "peso",
                                  .col_nombres = NULL,
                                  .nombres_por_facets = FALSE,
                                  .export = "E",
                                  .import = "I",
                                  .flujo_seleccionado = NULL,
                                  .accuracy = 0.01,
                                  .legend_position = "bottom",
                                  .title = NULL,
                                  .title_size = 18,
                                  .subtitle = NULL,
                                  .subtitle_size = 14,
                                  .caption = NULL,
                                  .caption_size = 10,
                                  .estatico_toggle = FALSE,
                                  .representar_indices = TRUE,
                                  .fuente_letra = "Segoe UI") {

  if("flujos_reales" %in% colnames(.datos_df)){
    .datos_df <-
      .datos_df
  } else if("precios_ponderados_export" %in% colnames(.datos_df) && "precios_ponderados_import" %in% colnames(.datos_df)){
    if("rri" %in% colnames(.datos_df)){
      .datos_df <-
        .datos_df |>
        dplyr::select(-rri) |>
        victorgmtools::get_flujos_reales(.año_referencia= .año_referencia,
                                         .año_base = .año_base,
                                         .meses_atras_acumulado = .meses_atras_acumulado,
                                         .variable_utilizada_cesta = .variable_utilizada_cesta,
                                         .col_fecha = .col_fecha,
                                         .col_flujo = .col_flujo,
                                         .col_taric = .col_taric,
                                         .col_valor_estadistico = .col_valor_estadistico,
                                         .col_peso = .col_peso,
                                         .col_nombres = .col_nombres,
                                         .export = .export,
                                         .import = .import,
                                         .remove_na = TRUE)
    }else{
      .datos_df <-
        .datos_df |>
        victorgmtools::get_flujos_reales(.año_referencia= .año_referencia,
                                         .año_base = .año_base,
                                         .meses_atras_acumulado = .meses_atras_acumulado,
                                         .variable_utilizada_cesta = .variable_utilizada_cesta,
                                         .col_fecha = .col_fecha,
                                         .col_flujo = .col_flujo,
                                         .col_taric = .col_taric,
                                         .col_valor_estadistico = .col_valor_estadistico,
                                         .col_peso = .col_peso,
                                         .col_nombres = .col_nombres,
                                         .export = .export,
                                         .import = .import,
                                         .remove_na = TRUE)
    }
  } else{
    if(!is.null(.col_nombres) && .nombres_por_facets == FALSE) {
      .datos_df <-
        .datos_df |>
        dplyr::group_by(!!rlang::sym(.col_fecha), !!rlang::sym(.col_flujo), !!rlang::sym(.col_nombres), !!rlang::sym(.col_taric)) |>
        dplyr::summarise(!!rlang::sym(.col_valor_estadistico) := sum(!!rlang::sym(.col_valor_estadistico), na.rm = TRUE),
                         !!rlang::sym(.col_peso) := sum(!!rlang::sym(.col_peso), na.rm = TRUE)) |>
        dplyr::ungroup()
    }

    .datos_df <-
      .datos_df |>
      victorgmtools::get_flujos_reales(.año_referencia= .año_referencia,
                                       .meses_atras_acumulado = .meses_atras_acumulado,
                                       .variable_utilizada_cesta = .variable_utilizada_cesta,
                                       .col_fecha = .col_fecha,
                                       .col_flujo = .col_flujo,
                                       .col_taric = .col_taric,
                                       .col_valor_estadistico = .col_valor_estadistico,
                                       .col_peso = .col_peso,
                                       .col_nombres = .col_nombres,
                                       .export = .export,
                                       .import = .import,
                                       .remove_na = TRUE)
  }

  .datos_df <-
    .datos_df |>
    dplyr::arrange(!!rlang::sym(.col_fecha)) |>
    tidyr::pivot_wider(names_from = !!rlang::sym(.col_flujo), values_from = c(precios_ponderados, valor_estadistico, flujos_reales)) |>
    dplyr::mutate(indice_flujos_reales_E = flujos_reales_E / dplyr::first(flujos_reales_E) * 100) |>
    dplyr::mutate(indice_flujos_reales_I = flujos_reales_I / dplyr::first(flujos_reales_I) * 100)

  if(.representar_indices == TRUE){
    flujos_reales_plt <-
      ggplot2::ggplot(data = .datos_df) +
      ggplot2::geom_hline(yintercept = 100) +
      ggplot2::geom_line(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                y = indice_flujos_reales_E,
                                                color = "Índice de exportación en volumen")) +
      ggplot2::geom_line(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                y = indice_flujos_reales_I,
                                                color = "Índice de importación en volumen")) +
      ggiraph::geom_point_interactive(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                             y = indice_flujos_reales_E,
                                                             color = "Índice de exportación en volumen",
                                                             tooltip = paste0("Fecha: ", scales::date_format(format = "%m/%Y")(.data[[.col_fecha]]), "\n",
                                                                              "Kilogramos exportados de la cesta de exportación: ", scales::number_format(accuracy = .accuracy, suffix = " kg", big.mark = ".", decimal.mark = ",")(flujos_reales_E), "\n",
                                                                              "Índice exportación en términos reales (base 100 en el primer periodo del gráfico): ", scales::number_format(accuracy = .accuracy, suffix = "", big.mark = ".", decimal.mark = ",")(indice_flujos_reales_E)))) +
      ggiraph::geom_point_interactive(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                             y = indice_flujos_reales_I,
                                                             color = "Índice de importación en volumen",
                                                             tooltip = paste0("Fecha: ", scales::date_format(format = "%m/%Y")(.data[[.col_fecha]]), "\n",
                                                                              "Kilogramos importados de la cesta de importación: ", scales::number_format(accuracy = .accuracy, suffix = " kg", big.mark = ".", decimal.mark = ",")(flujos_reales_I), "\n",
                                                                              "Índice importación en términos reales (base 100 en el primer periodo del gráfico): ", scales::number_format(accuracy = .accuracy, suffix = "", big.mark = ".", decimal.mark = ",")(indice_flujos_reales_I)))) +
      ggplot2::scale_color_manual(values = c("Índice de importación en volumen" = "#18BC9CFF",
                                             "Índice de exportación en volumen" = "#E31A1CFF"))
  } else{
    flujos_reales_plt <-
      ggplot2::ggplot(data = .datos_df) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::geom_line(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                y = flujos_reales_E,
                                                color = "Exportación en volumen")) +
      ggplot2::geom_line(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                y = flujos_reales_I,
                                                color = "Importación en volumen")) +
      ggiraph::geom_point_interactive(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                             y = flujos_reales_E,
                                                             color = "Exportación en volumen",
                                                             tooltip = paste0("Fecha: ", scales::date_format(format = "%m/%Y")(.data[[.col_fecha]]), "\n",
                                                                              "Kilogramos exportados de la cesta de exportación: ", scales::number_format(accuracy = .accuracy, suffix = " kg", big.mark = ".", decimal.mark = ",")(flujos_reales_E), "\n",
                                                                              "Índice exportación en términos reales (base 100 en el primer periodo del gráfico): ", scales::number_format(accuracy = .accuracy, suffix = "", big.mark = ".", decimal.mark = ",")(indice_flujos_reales_E)))) +
      ggiraph::geom_point_interactive(mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                             y = flujos_reales_I,
                                                             color = "Importación en volumen",
                                                             tooltip = paste0("Fecha: ", scales::date_format(format = "%m/%Y")(.data[[.col_fecha]]), "\n",
                                                                              "Kilogramos importados de la cesta de importación: ", scales::number_format(accuracy = .accuracy, suffix = " kg", big.mark = ".", decimal.mark = ",")(flujos_reales_I), "\n",
                                                                              "Índice importación en términos reales (base 100 en el primer periodo del gráfico): ", scales::number_format(accuracy = .accuracy, suffix = "", big.mark = ".", decimal.mark = ",")(indice_flujos_reales_I)))) +
      ggplot2::scale_color_manual(values = c("Importación en volumen" = "#18BC9CFF",
                                             "Exportación en volumen" = "#E31A1CFF"))
  }

  if(!is.null(.col_nombres) && .nombres_por_facets == TRUE) {
    flujos_reales_plt <-
      flujos_reales_plt +
      ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(.col_nombres)))
  }

  flujos_reales_plt <-
    flujos_reales_plt |>
    victorgmtools::graficos_estilo_victorgm(
      .title = .title,
      .title_size = .title_size,
      .subtitle = .subtitle,
      .subtitle_size = .subtitle_size,
      .caption = .caption,
      .caption_size = .caption_size,
      .legend_position = .legend_position,
      .fuente_letra = .fuente_letra
    )

  if(.estatico_toggle == FALSE){
    flujos_reales_plt |>
      victorgmtools::to_giraph()
  } else {
    flujos_reales_plt
  }
}

# Gráfico de la descomposición de la tasa de variación----
#' Partiendo de un dataframe como el generado con `victorgmtools::get_descomposicion_tv`, permite generar un gráfico para analizar la descomposición de la tasa de variación nominal.
#' @param .datos_df Data frame que contiene los datos de importación y exportación sobre los que se quiere calcular las tasas de variación y las contribuciones. La idea es que incluya `.col_fecha`, `.col_flujo`, `.col_valor_estadistico`, `.col_precios_ponderados`, `.col_flujos_reales` y opcionalmente, `.col_nombres`.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas (en formato fecha). Por defecto, `"fecha"`.
#' @param .col_flujo Cadena de caracteres. Nombre de la columna del dataframe que contiene el tipo de flujo. Por defecto, `"flujo"`.
#' @param .col_tv_nominal Cadena de caracteres. Nombre de la columna del dataframe que contiene las tasas de variación en términos nominales. Por defecto, `tv_nominal`.
#' @param .col_contribucion_precios Cadena de caracteres. Nombre de la columna del dataframe que contiene la contribución de los precios a la tasa de variación. Por defecto, `contribucion_precios`.
#' @param .col_contribucion_volumen Cadena de caracteres. Nombre de la columna del dataframe que contiene la contribución del peso a la tasa de variación. Por defecto, `contribucion_volumen`.
#' @param .col_nombres Opcional (sólo en caso de que .datos_df contenga esta columna). Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres sobre los que se quiere agrupar para calcular las contribuciones a las tasas de variación. Por defecto, `NULL`.
#' @param .flujo_seleccionado Cadena de caracteres. Permite filtrar por flujo, estableciendo por ejemplo, `.flujo_seleccionado = "E"`. Por defecto, `NULL`, que devuelve ambos gráficos.
#' @param .remove_na Valor lógico. Permite eliminar los primero periodos del dataframe en los que las tasas de variación y las contribuciones son `NA` o `NaN`. Por defecto, `TRUE`.
#' @param .quitar_valores_extremos Valor lógico. En caso de `TRUE` elimina valores extremos de `.col_contribucion_precios` y de `.col_contribucion_volumen` para que el gráfico siga siendo informativo. Por defecto, `TRUE`.
#' @param .estatico_toggle Valor lógico. En caso de `TRUE` el gráfico no será interactivo. Por defecto, `FALSE`.
#' @param .fuente_letra Cadena de caracteres. Fuente tipográfica a utilizar en el gráfico. Por defecto, utiliza la fuente: `"Segoe UI"`. Otras opciones comunes: `"Arial"`, `"Times"`, `"Courier"`, `"Helvetica"`, etc.
#' @export
get_descomposicion_tv_plt <- function(.datos_df,
                                      .col_fecha = "fecha",
                                      .col_flujo = "flujo",
                                      .col_tv_nominal = "tv_nominal",
                                      .col_contribucion_precios = "contribucion_precios",
                                      .col_contribucion_volumen = "contribucion_volumen",
                                      .col_nombres = NULL,
                                      .flujo_seleccionado = NULL,
                                      .remove_na = TRUE,
                                      .accuracy = 0.01,
                                      .quitar_valores_extremos = TRUE,
                                      .desviaciones_extremos = 2,
                                      .legend_position = "bottom",
                                      .title = NULL,
                                      .title_size = 18,
                                      .subtitle = NULL,
                                      .subtitle_size = 14,
                                      .caption = NULL,
                                      .caption_size = 10,
                                      .estatico_toggle = FALSE,
                                      .fuente_letra = "Segoe UI") {
  if(.remove_na == TRUE){
    .datos_df <-
      .datos_df |>
      dplyr::filter(!is.na(.col_tv_nominal),
                    !is.nan(.col_contribucion_precios),
                    !is.nan(.col_contribucion_volumen))
  }

  if(!is.null(.flujo_seleccionado)){
    .datos_df <-
      .datos_df |>
      dplyr::filter(.data[[.col_flujo]] == .flujo_seleccionado)
  }

  # Eliminar valores extremos si está activado
  if(.quitar_valores_extremos) {
    # Función para detectar outliers por grupo
    detectar_outliers <- function(data, col_precios, col_volumen, n_sd = .desviaciones_extremos) {
      data |>
        dplyr::group_by(dplyr::across(dplyr::any_of(c(.col_flujo, .col_nombres)))) |>
        dplyr::mutate(
          # Calcular estadísticas por grupo
          media_precios = mean(.data[[col_precios]], na.rm = TRUE),
          sd_precios = sd(.data[[col_precios]], na.rm = TRUE),
          media_volumen = mean(.data[[col_volumen]], na.rm = TRUE),
          sd_volumen = sd(.data[[col_volumen]], na.rm = TRUE),

          # Detectar outliers (valores que se alejan más de n_sd desviaciones estándar)
          outlier_precios = abs(.data[[col_precios]] - media_precios) > (n_sd * sd_precios),
          outlier_volumen = abs(.data[[col_volumen]] - media_volumen) > (n_sd * sd_volumen),

          # Si cualquiera de las dos es outlier, marcar ambas para eliminación
          es_outlier = outlier_precios | outlier_volumen
        ) |>
        dplyr::ungroup() |>
        dplyr::select(-media_precios, -sd_precios, -media_volumen, -sd_volumen,
                      -outlier_precios, -outlier_volumen)
    }

    # Aplicar detección de outliers
    .datos_df <- detectar_outliers(.datos_df, .col_contribucion_precios, .col_contribucion_volumen)

    # Crear versión para el gráfico eliminando outliers de las contribuciones
    .datos_df_grafico <- .datos_df |>
      dplyr::mutate(
        !!.col_contribucion_precios := ifelse(es_outlier, NA, .data[[.col_contribucion_precios]]),
        !!.col_contribucion_volumen := ifelse(es_outlier, NA, .data[[.col_contribucion_volumen]])
      )

    # Mostrar información sobre outliers eliminados
    n_outliers <- sum(.datos_df$es_outlier, na.rm = TRUE)
    if(n_outliers > 0) {
      message(paste("Se eliminaron", n_outliers, "observaciones con valores extremos en las contribuciones"))

      # Mostrar fechas de outliers para información
      fechas_outliers <- .datos_df |>
        dplyr::filter(es_outlier) |>
        dplyr::pull(.data[[.col_fecha]]) |>
        format("%Y-%m") |>
        unique()

      if(length(fechas_outliers) <= 5) {
        message(paste("Fechas afectadas:", paste(fechas_outliers, collapse = ", ")))
      } else {
        message(paste("Fechas afectadas:", paste(head(fechas_outliers, 3), collapse = ", "), "... y", length(fechas_outliers) - 3, "más"))
      }
    }

    # Usar datos sin outliers para el gráfico, pero mantener tv_nominal original
    .datos_df <- .datos_df_grafico
  }

  .datos_df_largo <-
    .datos_df |>
    tidyr::pivot_longer(
      cols = c(.col_contribucion_precios, .col_contribucion_volumen),
      names_to = "tipo_contribucion",
      values_to = "valor"
    )

  .datos_df_largo$tipo_contribucion_label <-
    dplyr::case_when(
      .datos_df_largo$tipo_contribucion == .col_contribucion_precios ~ "Contribución del precio",
      .datos_df_largo$tipo_contribucion == .col_contribucion_volumen ~ "Contribución del volumen"
      )

  # .width <-
  #   0.85 * nrow(.datos_df_largo |> dplyr::distinct(.col_fecha)) / 2

  descomposicion_tv_plt <-
    ggplot2::ggplot() +
    ggiraph::geom_col_interactive(data = .datos_df_largo,
                                  mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                         y = valor,
                                                         fill = tipo_contribucion_label,
                                                         tooltip = paste0("Fecha: ", scales::date_format(format = "%m/%Y")(.data[[.col_fecha]]), "\n",
                                                                          tipo_contribucion_label, ": ",
                                                                          scales::number_format(accuracy = .accuracy, suffix = " p.p.", big.mark = ".", decimal.mark = ",")(valor))),
                                  # width = .width,
                                  position = "stack") +
    ggplot2::geom_hline(yintercept = 0) +
    ggiraph::geom_point_interactive(data = .datos_df,
                                    mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                                           y = .data[[.col_tv_nominal]],
                                                           color = "Tasa de variación",
                                                           tooltip = paste0("Fecha: ", scales::date_format(format = "%m/%Y")(.data[[.col_fecha]]), "\n",
                                                                            "Tasa de variación nominal: ",
                                                                            scales::number_format(accuracy = .accuracy, suffix = " %", big.mark = ".", decimal.mark = ",")(.data[[.col_tv_nominal]])))) +
    ggplot2::geom_line(data = .datos_df,
                       mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                              y = .data[[.col_tv_nominal]],
                                              color = "Tasa de variación"))

  if(is.null(.flujo_seleccionado)) {
    descomposicion_tv_plt <-
      descomposicion_tv_plt +
      ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(.col_flujo)))
  }

  if(!is.null(.col_nombres)) {
    descomposicion_tv_plt <-
      descomposicion_tv_plt +
      ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(.col_nombres)))
  }

  descomposicion_tv_plt <-
    descomposicion_tv_plt |>
    victorgmtools::graficos_estilo_victorgm(
      .suffix = " %",
      .title = .title,
      .title_size = .title_size,
      .subtitle = .subtitle,
      .subtitle_size = .subtitle_size,
      .caption = .caption,
      .caption_size = .caption_size,
      .legend_position = .legend_position,
      .fuente_letra = .fuente_letra) +
    ggplot2::scale_fill_manual(values = c("Contribución del precio" = scales::alpha("#18BC9CFF", 0.7),
                                          "Contribución del volumen" = scales::alpha("#E31A1CFF", 0.7))) +
    ggplot2::scale_color_manual(values = c("Tasa de variación" = "darkblue")) #+
    # ggplot2::guides(
    #   fill = ggplot2::guide_legend(override.aes = list(shape = 19, linetype = 0, size = 3)),
    #   color = ggplot2::guide_legend(override.aes = list(shape = 19, size = 3))
    # )

  if(.estatico_toggle == FALSE){
    descomposicion_tv_plt |>
      victorgmtools::to_giraph()
  } else {
    descomposicion_tv_plt
  }
}
