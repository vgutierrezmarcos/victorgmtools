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
#' @param .flujo_seleccionado Cadena de caracteres. Si se especifica, calcula el índice solo para este flujo ("E" o "I"). Por defecto, `NULL` (calcula para ambos flujos).
#' @param .export Nombre que denota las exportaciones en la columna `.col_flujo`. Por defecto, `"E"`.
#' @param .import Nombre que denota las importaciones en la columna `.col_flujo`. Por defecto, `"I"`.
#' @param .get_rri Valor lógico. En caso de `get_rri = TRUE`, devuelve una columna adicional con la RRI (i.e.cociente del índice de precios de exportación y de importación). Por defecto, `TRUE`.
#' @param .remove_na Valor lógico. Si TRUE, elimina filas con valores NA en el resultado. Por defecto, `TRUE`.
#' @param .metodo Si se establece `.metodo = "SQL"` usará SQL (duckdb) para una mayor eficiencia. Deseable cuando `.datos_df` es grande. Si se establece en `NULL` realizará los cálculos usando únicamente R base y el tidyverse. Por defecto, `NULL`.
#' @return Un tibble con columnas: fecha, flujo, indice_precios, y opcionalmente rri (si .get_rri = TRUE). Si se especifica .col_nombres, también incluye esas columnas de agrupación.
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
          dc.", .col_fecha, ",
          dc.", .col_flujo, ",
          ", if (!is.null(.col_nombres)) paste0("dc.", paste(.col_nombres, collapse = paste0(", dc.")), ",") else "", "
          SUM(dc.precio * (p.valor_ponderacion / tc.total_cesta)) as precios_ponderados
        FROM datos_completos dc
        JOIN ponderaciones p ON ",
                            paste(paste0("dc.", setdiff(group_cols, .col_fecha), " = p.", setdiff(group_cols, .col_fecha)),
                                  collapse = " AND "), "
        CROSS JOIN total_cesta tc
        GROUP BY dc.", .col_fecha, ", dc.", .col_flujo,
                            if (!is.null(.col_nombres)) paste0(", dc.", paste(.col_nombres, collapse = paste0(", dc."))) else ""
      )

      precios_ponderados_df <- DBI::dbGetQuery(con, final_query)
      precios_ponderados_df[[.col_fecha]] <- as.Date(precios_ponderados_df[[.col_fecha]])

    } else {
      # Ponderación móvil - convertir a R después de preparar datos
      datos_completos <- DBI::dbGetQuery(con, "SELECT * FROM datos_completos")
      datos_completos[[.col_fecha]] <- as.Date(datos_completos[[.col_fecha]])

      # Determinar la columna de valores para el cálculo
      col_valores_sql <- ifelse(variable_cesta == .col_valor_estadistico, "valor_total", "peso_total")

      datos_completos <-
        datos_completos |>
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
      tidyr::replace_na(stats::setNames(list(0, 0), c(.col_valor_estadistico, .col_peso))) |>
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
        dplyr::left_join(ponderacion_referencia, by = c(.col_taric, .col_nombres, .col_flujo)) |>
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

    # NUEVA LÓGICA: Detectar qué flujos existen
    flujos_existentes <- unique(precios_ponderados_df[[.col_flujo]])
    tiene_export <- .export %in% flujos_existentes
    tiene_import <- .import %in% flujos_existentes

    # Realizar pivot_wider
    precios_ponderados_df <-
      precios_ponderados_df |>
      tidyr::pivot_wider(names_from = !!rlang::sym(.col_flujo), values_from = precios_ponderados)

    # Renombrar solo las columnas que existen
    if(tiene_export && tiene_import){
      # Caso normal: existen ambos flujos
      precios_ponderados_df <-
        precios_ponderados_df |>
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

    } else if(tiene_export){
      # Solo existen exportaciones
      precios_ponderados_df <-
        precios_ponderados_df |>
        dplyr::rename(precios_ponderados_export = !!rlang::sym(.export))

      if(.get_rri == TRUE){
        warning("No se puede calcular la RRI porque solo existen datos de exportaciones.")
      }

      if(.remove_na == TRUE){
        precios_ponderados_df <-
          precios_ponderados_df |>
          dplyr::filter(precios_ponderados_export != 0)
      }

    } else if(tiene_import){
      # Solo existen importaciones
      precios_ponderados_df <-
        precios_ponderados_df |>
        dplyr::rename(precios_ponderados_import = !!rlang::sym(.import))

      if(.get_rri == TRUE){
        warning("No se puede calcular la RRI porque solo existen datos de importaciones.")
      }

      if(.remove_na == TRUE){
        precios_ponderados_df <-
          precios_ponderados_df |>
          dplyr::filter(precios_ponderados_import != 0)
      }

    } else {
      # No hay datos de ninguno de los flujos esperados
      warning("No se encontraron datos para los flujos especificados (.export y .import).")
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
#' @param .remove_na Valor lógico. Si `TRUE` (por defecto), elimina las observaciones con precio ponderado nulo.
#' @param .metodo Si se establece `.metodo = "SQL"` usará SQL para una mayor eficiencia. Deseable cuando `.datos_df` es grande. Si se establece en `NULL` realizará los cálculos usando únicamente R base y el tidyverse. Por defecto, `NULL`.
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
  if (!is.null(.metodo) && .metodo == "SQL") {

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
      dplyr::summarise(valor_estadistico = sum(!!rlang::sym(.col_valor_estadistico), na.rm = TRUE), .groups = "drop") |>
      dplyr::ungroup()

    flujos_reales_df <-
      merge(precios_ponderados_df, valor_estadistico_df) |>
      dplyr::mutate(flujos_reales = valor_estadistico / precios_ponderados) |>
      dplyr::relocate(c(!!rlang::sym(.col_fecha), !!rlang::sym(.col_flujo), valor_estadistico, precios_ponderados, flujos_reales)) |>
      dplyr::relocate(c(valor_estadistico, precios_ponderados, flujos_reales),
                      .after = dplyr::last_col())

    if(.remove_na == TRUE){
      flujos_reales_df <-
        flujos_reales_df |>
        dplyr::filter(precios_ponderados != 0)
    }

    if(!is.null(.año_base)){
      ponderacion_base <-
        flujos_reales_df |>
        dplyr::mutate(año = as.Date(!!rlang::sym(.col_fecha)) |> lubridate::year()) |>
        dplyr::filter(año == .año_base) |>
        dplyr::filter(!!rlang::sym(.col_fecha) == min(!!rlang::sym(.col_fecha), na.rm = TRUE)) |>
        dplyr::select(-año) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)),
                        dplyr::across(dplyr::all_of(.col_flujo))) |>
        dplyr::reframe(valor_enero_base = sum(flujos_reales, na.rm = TRUE))

      flujos_reales_df <-
        flujos_reales_df |>
        dplyr::left_join(ponderacion_base, by = c(.col_nombres, .col_flujo)) |>
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
#' @param .remove_na Valor lógico. Si `TRUE` (por defecto), elimina las observaciones con tasa nominal `NA` o contribuciones no calculables.
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
                    .after = dplyr::last_col())

  if(.remove_na == TRUE){
    descomposicion_tv_df <-
      descomposicion_tv_df |>
      dplyr::filter(!is.na(tv_nominal),
                    !is.nan(contribucion_precios),
                    !is.nan(contribucion_volumen))
  }

  return(descomposicion_tv_df)
}


# Gráfico de la evolución del precio de un producto ----
#' Gráfico de la evolución del precio unitario de un producto
#'
#' Representa el precio unitario (valor estadístico / peso) de un producto a lo largo del tiempo,
#' distinguiendo exportaciones e importaciones. Si se indica `.col_nombres` puede desagregarse por
#' grupos (por ejemplo países o provincias), en una misma gráfica o en facetas.
#'
#' @inheritParams get_precios_ponderados
#' @param .taric_seleccionado Cadena de caracteres. Código del producto (según `.col_taric`) que se quiere representar.
#' @param .nombres_por_facets Valor lógico. Si `TRUE` y se ha indicado `.col_nombres`, cada grupo va en una faceta. Por defecto, `FALSE`.
#' @param .accuracy Número. Precisión con la que se muestran los precios en los tooltips. Por defecto, `0.01`.
#' @param .legend_position Posición de la leyenda. Por defecto, `"bottom"`.
#' @param .title,.subtitle,.caption Título, subtítulo y pie del gráfico.
#' @param .title_size,.subtitle_size,.caption_size Tamaños de título, subtítulo y pie.
#' @param .estatico Valor lógico. `TRUE` devuelve un `ggplot` estático; `FALSE` (por defecto) un gráfico interactivo `ggiraph`.
#' @param .fuente_letra Cadena de caracteres. Fuente del gráfico. Por defecto, `"Source Sans 3"`.
#' @param .paleta_utilizada Vector de colores con nombre para los flujos. Por defecto, `paleta_exportaciones_importaciones()`.
#' @param ... Otros argumentos que se pasan a [graficos_estilo_victorgm()] (por ejemplo `.logo_path`, `.width`, `.guardar_en`).
#' @return Un objeto `ggplot` o `girafe`.
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
                                    .estatico = FALSE,
                                    .fuente_letra = "Source Sans 3",
                                    .paleta_utilizada = NULL,
                                    ...) {
  if (!is.null(.flujo_seleccionado)) {
    .datos_df <- .datos_df |>
      dplyr::filter(.data[[.col_flujo]] == .flujo_seleccionado)
  }

  cols_grupo <- c(.col_fecha, .col_flujo, .col_taric)
  if (!is.null(.col_nombres)) cols_grupo <- c(cols_grupo, .col_nombres)

  .datos_df <- .datos_df |>
    dplyr::filter(.data[[.col_taric]] == .taric_seleccionado) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(cols_grupo))) |>
    dplyr::summarise(
      !!rlang::sym(.col_valor_estadistico) := sum(!!rlang::sym(.col_valor_estadistico), na.rm = TRUE),
      !!rlang::sym(.col_peso) := sum(!!rlang::sym(.col_peso), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(precio = !!rlang::sym(.col_valor_estadistico) / !!rlang::sym(.col_peso))

  if (nrow(.datos_df) == 0) {
    stop("No hay observaciones para el producto '", .taric_seleccionado, "'.")
  }

  fmt_precio <- scales::number_format(accuracy = .accuracy, suffix = " €/kg", big.mark = ".", decimal.mark = ",")
  grupo_linea <- if (is.null(.col_nombres)) .col_flujo else .col_nombres

  precio_producto_plt <-
    ggplot2::ggplot(data = .datos_df,
                    mapping = ggplot2::aes(x = .data[[.col_fecha]],
                                           y = precio,
                                           color = .data[[.col_flujo]])) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_line(mapping = ggplot2::aes(group = interaction(.data[[.col_flujo]], .data[[grupo_linea]]))) +
    ggiraph::geom_point_interactive(
      mapping = ggplot2::aes(
        data_id = .data[[.col_flujo]],
        tooltip = paste0(
          "Fecha: ", format(.data[[.col_fecha]], "%m/%Y"), "\n",
          "Flujo: ", .data[[.col_flujo]], "\n",
          if (!is.null(.col_nombres)) paste0(.data[[.col_nombres]], "\n") else "",
          "Precio: ", fmt_precio(precio)
        )
      ),
      size = 1
    )

  if (!is.null(.col_nombres) && isTRUE(.nombres_por_facets)) {
    precio_producto_plt <- precio_producto_plt +
      ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(.col_nombres)))
  }

  if (is.null(.paleta_utilizada)) {
    .paleta_utilizada <- paleta_exportaciones_importaciones()
    names(.paleta_utilizada)[names(.paleta_utilizada) == "E"] <- .export
    names(.paleta_utilizada)[names(.paleta_utilizada) == "I"] <- .import
  }

  precio_producto_plt |>
    graficos_estilo_victorgm(
      .suffix = " €/kg",
      .accuracy = .accuracy,
      .title = .title,
      .title_size = .title_size,
      .subtitle = .subtitle,
      .subtitle_size = .subtitle_size,
      .caption = .caption,
      .caption_size = .caption_size,
      .legend_position = .legend_position,
      .fuente_letra = .fuente_letra,
      .paleta_utilizada = .paleta_utilizada,
      .estatico = .estatico,
      ...
    )
}

# Gráfico de la relación real de intercambio ----
#' Gráfico de la relación real de intercambio (RRI)
#'
#' Representa la evolución de los índices de precios de exportación e importación y de su cociente,
#' la relación real de intercambio. Admite tanto el dataframe original (calcula los índices con
#' [get_precios_ponderados()]) como uno que ya contenga `precios_ponderados_export` y
#' `precios_ponderados_import`.
#'
#' @inheritParams get_precio_producto_plt
#' @inheritParams get_precios_ponderados
#' @param .representar_indices Valor lógico. Si `TRUE` (por defecto) representa los tres índices con base 100 en el primer periodo; si `FALSE`, solo la RRI en niveles.
#' @param .paleta_utilizada Vector de colores con nombre para las tres series (`"Índice de la Relación Real de Intercambio"`, `"Índice de precios de exportación"`, `"Índice de precios de importación"`) o para `"Relación Real de Intercambio"`.
#' @return Un objeto `ggplot` o `girafe`.
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
                        .estatico = FALSE,
                        .representar_indices = TRUE,
                        .fuente_letra = "Source Sans 3",
                        .paleta_utilizada = NULL,
                        ...) {
  tiene_precios <- all(c("precios_ponderados_export", "precios_ponderados_import") %in% colnames(.datos_df))

  if (tiene_precios) {
    if (!"rri" %in% colnames(.datos_df)) {
      .datos_df <- .datos_df |>
        dplyr::mutate(rri = precios_ponderados_export / precios_ponderados_import)
    }
  } else {
    .datos_df <- .datos_df |>
      get_precios_ponderados(.año_referencia = .año_referencia,
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

  .datos_df <- .datos_df |>
    dplyr::arrange(!!rlang::sym(.col_fecha)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres))) |>
    dplyr::mutate(
      indice_rri = rri / dplyr::first(rri) * 100,
      indice_precios_ponderados_export = precios_ponderados_export / dplyr::first(precios_ponderados_export) * 100,
      indice_precios_ponderados_import = precios_ponderados_import / dplyr::first(precios_ponderados_import) * 100
    ) |>
    dplyr::ungroup()

  fmt <- scales::number_format(accuracy = .accuracy, big.mark = ".", decimal.mark = ",")
  fecha_txt <- function(x) format(x, "%m/%Y")

  if (isTRUE(.representar_indices)) {
    nombres_series <- c(rri = "Índice de la Relación Real de Intercambio",
                        export = "Índice de precios de exportación",
                        import = "Índice de precios de importación")

    datos_largo <- .datos_df |>
      tidyr::pivot_longer(c(indice_rri, indice_precios_ponderados_export, indice_precios_ponderados_import),
                          names_to = "serie", values_to = "indice") |>
      dplyr::mutate(
        nivel = dplyr::case_when(serie == "indice_rri" ~ rri,
                                 serie == "indice_precios_ponderados_export" ~ precios_ponderados_export,
                                 TRUE ~ precios_ponderados_import),
        serie = dplyr::case_when(serie == "indice_rri" ~ nombres_series[["rri"]],
                                 serie == "indice_precios_ponderados_export" ~ nombres_series[["export"]],
                                 TRUE ~ nombres_series[["import"]]),
        serie = factor(serie, levels = unname(nombres_series))
      )

    rri_plt <-
      ggplot2::ggplot(data = datos_largo,
                      mapping = ggplot2::aes(x = .data[[.col_fecha]], y = indice, color = serie)) +
      ggplot2::geom_hline(yintercept = 100) +
      ggplot2::geom_line(mapping = ggplot2::aes(group = interaction(serie, if (is.null(.col_nombres)) 1 else .data[[.col_nombres]]))) +
      ggiraph::geom_point_interactive(
        mapping = ggplot2::aes(
          data_id = serie,
          tooltip = paste0("Fecha: ", fecha_txt(.data[[.col_fecha]]), "\n",
                           serie, ": ", fmt(indice), " (base 100 en el primer periodo)\n",
                           "Nivel: ", fmt(nivel))
        ),
        size = 1
      )

    if (is.null(.paleta_utilizada)) {
      .paleta_utilizada <- stats::setNames(c("#2C3E50", "#5F2987", "#B8860B"), unname(nombres_series))
    }
  } else {
    rri_plt <-
      ggplot2::ggplot(data = .datos_df,
                      mapping = ggplot2::aes(x = .data[[.col_fecha]], y = rri,
                                             color = "Relación Real de Intercambio")) +
      ggplot2::geom_hline(yintercept = 1) +
      ggplot2::geom_line(mapping = ggplot2::aes(group = if (is.null(.col_nombres)) 1 else .data[[.col_nombres]])) +
      ggiraph::geom_point_interactive(
        mapping = ggplot2::aes(
          tooltip = paste0("Fecha: ", fecha_txt(.data[[.col_fecha]]), "\n",
                           "RRI: ", fmt(rri), "\n",
                           "Índice RRI (base 100 en el primer periodo): ", fmt(indice_rri))
        ),
        size = 1
      )

    if (is.null(.paleta_utilizada)) {
      .paleta_utilizada <- c("Relación Real de Intercambio" = "#2C3E50")
    }
  }

  if (!is.null(.col_nombres) && isTRUE(.nombres_por_facets)) {
    rri_plt <- rri_plt + ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(.col_nombres)))
  }

  rri_plt |>
    graficos_estilo_victorgm(
      .accuracy = .accuracy,
      .title = .title,
      .title_size = .title_size,
      .subtitle = .subtitle,
      .subtitle_size = .subtitle_size,
      .caption = .caption,
      .caption_size = .caption_size,
      .legend_position = .legend_position,
      .fuente_letra = .fuente_letra,
      .paleta_utilizada = .paleta_utilizada,
      .estatico = .estatico,
      ...
    )
}

# Gráfico de la evolución de los flujos en términos reales ----
#' Gráfico de los flujos comerciales en términos reales
#'
#' Representa la evolución del volumen (flujos reales) de exportaciones e importaciones, bien como
#' índices con base 100 en el primer periodo (`.representar_indices = TRUE`) o en niveles. Admite el
#' dataframe original, uno con precios ponderados o uno que ya contenga `flujos_reales`.
#'
#' @inheritParams get_precio_producto_plt
#' @inheritParams get_flujos_reales
#' @param .representar_indices Valor lógico. Si `TRUE` (por defecto) representa índices con base 100 en el primer periodo; si `FALSE`, los flujos reales en niveles.
#' @param .paleta_utilizada Vector de colores con nombre para las series de exportación e importación. Por defecto, `paleta_exportaciones_importaciones()`.
#' @return Un objeto `ggplot` o `girafe`.
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
                                  .estatico = FALSE,
                                  .representar_indices = TRUE,
                                  .fuente_letra = "Source Sans 3",
                                  .paleta_utilizada = NULL,
                                  ...) {
  if (!"flujos_reales" %in% colnames(.datos_df)) {
    if ("rri" %in% colnames(.datos_df)) {
      .datos_df <- .datos_df |> dplyr::select(-rri)
    }
    .datos_df <- .datos_df |>
      get_flujos_reales(.año_referencia = .año_referencia,
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

  if (!is.null(.flujo_seleccionado)) {
    .datos_df <- .datos_df |>
      dplyr::filter(.data[[.col_flujo]] == .flujo_seleccionado)
  }

  etiquetas_flujo <- stats::setNames(c("Exportación en volumen", "Importación en volumen"), c(.export, .import))

  .datos_df <- .datos_df |>
    dplyr::arrange(!!rlang::sym(.col_fecha)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(.col_flujo, .col_nombres)))) |>
    dplyr::mutate(indice_flujos_reales = flujos_reales / dplyr::first(flujos_reales) * 100) |>
    dplyr::ungroup() |>
    dplyr::mutate(serie = unname(etiquetas_flujo[as.character(.data[[.col_flujo]])]),
                  serie = ifelse(is.na(serie), as.character(.data[[.col_flujo]]), serie))

  fmt <- scales::number_format(accuracy = .accuracy, big.mark = ".", decimal.mark = ",")
  fmt_kg <- scales::number_format(accuracy = .accuracy, suffix = " kg", big.mark = ".", decimal.mark = ",")
  fecha_txt <- function(x) format(x, "%m/%Y")
  grupo <- if (is.null(.col_nombres)) "serie" else .col_nombres

  col_y <- if (isTRUE(.representar_indices)) "indice_flujos_reales" else "flujos_reales"
  ref_y <- if (isTRUE(.representar_indices)) 100 else 0

  flujos_reales_plt <-
    ggplot2::ggplot(data = .datos_df,
                    mapping = ggplot2::aes(x = .data[[.col_fecha]], y = .data[[col_y]], color = serie)) +
    ggplot2::geom_hline(yintercept = ref_y) +
    ggplot2::geom_line(mapping = ggplot2::aes(group = interaction(serie, .data[[grupo]]))) +
    ggiraph::geom_point_interactive(
      mapping = ggplot2::aes(
        data_id = serie,
        tooltip = paste0("Fecha: ", fecha_txt(.data[[.col_fecha]]), "\n",
                         serie, "\n",
                         "Volumen de la cesta: ", fmt_kg(flujos_reales), "\n",
                         "Índice (base 100 en el primer periodo): ", fmt(indice_flujos_reales))
      ),
      size = 1
    )

  if (!is.null(.col_nombres) && isTRUE(.nombres_por_facets)) {
    flujos_reales_plt <- flujos_reales_plt + ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(.col_nombres)))
  }

  if (is.null(.paleta_utilizada)) {
    pal <- paleta_exportaciones_importaciones()
    .paleta_utilizada <- stats::setNames(c(pal[["E"]], pal[["I"]]), unname(etiquetas_flujo))
  }

  flujos_reales_plt |>
    graficos_estilo_victorgm(
      .accuracy = .accuracy,
      .title = .title,
      .title_size = .title_size,
      .subtitle = .subtitle,
      .subtitle_size = .subtitle_size,
      .caption = .caption,
      .caption_size = .caption_size,
      .legend_position = .legend_position,
      .fuente_letra = .fuente_letra,
      .paleta_utilizada = .paleta_utilizada,
      .estatico = .estatico,
      ...
    )
}

# Gráfico de la descomposición de la tasa de variación ----
#' Gráfico de la descomposición precio-volumen de la tasa de variación
#'
#' Partiendo de un dataframe como el generado con [get_descomposicion_tv()], representa en barras
#' apiladas las contribuciones del precio y del volumen a la tasa de variación nominal, que se
#' superpone como línea con puntos.
#'
#' @inheritParams get_precio_producto_plt
#' @inheritParams get_descomposicion_tv
#' @param .col_tv_nominal Cadena de caracteres. Columna con la tasa de variación nominal. Por defecto, `"tv_nominal"`.
#' @param .col_contribucion_precios Cadena de caracteres. Columna con la contribución del precio. Por defecto, `"contribucion_precios"`.
#' @param .col_contribucion_volumen Cadena de caracteres. Columna con la contribución del volumen. Por defecto, `"contribucion_volumen"`.
#' @param .quitar_valores_extremos Valor lógico. Si `TRUE` (por defecto), las contribuciones que se alejan más de `.desviaciones_extremos` desviaciones típicas de su media no se dibujan (la tasa nominal se mantiene).
#' @param .desviaciones_extremos Número de desviaciones típicas a partir del cual una contribución se considera extrema. Por defecto, `2`.
#' @param .paleta_utilizada Vector de colores con nombre para `"Contribución del precio"`, `"Contribución del volumen"` y `"Tasa de variación"`. Por defecto, se toma de `paleta_efecto_precio_volumen()`.
#' @return Un objeto `ggplot` o `girafe`.
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
                                      .estatico = FALSE,
                                      .fuente_letra = "Source Sans 3",
                                      .paleta_utilizada = NULL,
                                      ...) {
  if (isTRUE(.remove_na)) {
    .datos_df <- .datos_df |>
      dplyr::filter(!is.na(.data[[.col_tv_nominal]]),
                    !is.nan(.data[[.col_contribucion_precios]]),
                    !is.nan(.data[[.col_contribucion_volumen]]))
  }

  if (!is.null(.flujo_seleccionado)) {
    .datos_df <- .datos_df |>
      dplyr::filter(.data[[.col_flujo]] == .flujo_seleccionado)
  }

  if (isTRUE(.quitar_valores_extremos)) {
    n_sd <- .desviaciones_extremos
    .datos_df <- .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::any_of(c(.col_flujo, .col_nombres)))) |>
      dplyr::mutate(
        .media_precios = mean(.data[[.col_contribucion_precios]], na.rm = TRUE),
        .sd_precios = stats::sd(.data[[.col_contribucion_precios]], na.rm = TRUE),
        .media_volumen = mean(.data[[.col_contribucion_volumen]], na.rm = TRUE),
        .sd_volumen = stats::sd(.data[[.col_contribucion_volumen]], na.rm = TRUE),
        es_outlier = (abs(.data[[.col_contribucion_precios]] - .media_precios) > n_sd * .sd_precios) |
          (abs(.data[[.col_contribucion_volumen]] - .media_volumen) > n_sd * .sd_volumen),
        es_outlier = dplyr::coalesce(es_outlier, FALSE)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-.media_precios, -.sd_precios, -.media_volumen, -.sd_volumen)

    n_outliers <- sum(.datos_df$es_outlier)
    if (n_outliers > 0) {
      fechas_outliers <- .datos_df |>
        dplyr::filter(es_outlier) |>
        dplyr::pull(.data[[.col_fecha]]) |>
        format("%Y-%m") |>
        unique()
      message("Se han omitido ", n_outliers, " observaciones con contribuciones extremas (",
              if (length(fechas_outliers) <= 5) paste(fechas_outliers, collapse = ", ")
              else paste0(paste(utils::head(fechas_outliers, 3), collapse = ", "), " y ", length(fechas_outliers) - 3, " más"),
              ").")
    }

    .datos_df <- .datos_df |>
      dplyr::mutate(
        !!.col_contribucion_precios := ifelse(es_outlier, NA, .data[[.col_contribucion_precios]]),
        !!.col_contribucion_volumen := ifelse(es_outlier, NA, .data[[.col_contribucion_volumen]])
      ) |>
      dplyr::select(-es_outlier)
  }

  etiqueta_precio <- "Contribución del precio"
  etiqueta_volumen <- "Contribución del volumen"
  etiqueta_tv <- "Tasa de variación"

  datos_largo <- .datos_df |>
    tidyr::pivot_longer(cols = dplyr::all_of(c(.col_contribucion_precios, .col_contribucion_volumen)),
                        names_to = "tipo_contribucion", values_to = "valor") |>
    dplyr::mutate(tipo_contribucion = ifelse(tipo_contribucion == .col_contribucion_precios,
                                             etiqueta_precio, etiqueta_volumen),
                  tipo_contribucion = factor(tipo_contribucion, levels = c(etiqueta_precio, etiqueta_volumen)))

  fmt_pp <- scales::number_format(accuracy = .accuracy, suffix = " p.p.", big.mark = ".", decimal.mark = ",")
  fmt_pct <- scales::number_format(accuracy = .accuracy, suffix = " %", big.mark = ".", decimal.mark = ",")
  fecha_txt <- function(x) format(x, "%m/%Y")

  descomposicion_tv_plt <-
    ggplot2::ggplot() +
    ggiraph::geom_col_interactive(
      data = datos_largo,
      mapping = ggplot2::aes(x = .data[[.col_fecha]], y = valor, fill = tipo_contribucion,
                             data_id = tipo_contribucion,
                             tooltip = paste0("Fecha: ", fecha_txt(.data[[.col_fecha]]), "\n",
                                              tipo_contribucion, ": ", fmt_pp(valor))),
      position = "stack", alpha = 0.75
    ) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_line(data = .datos_df,
                       mapping = ggplot2::aes(x = .data[[.col_fecha]], y = .data[[.col_tv_nominal]],
                                              color = etiqueta_tv, group = 1)) +
    ggiraph::geom_point_interactive(
      data = .datos_df,
      mapping = ggplot2::aes(x = .data[[.col_fecha]], y = .data[[.col_tv_nominal]], color = etiqueta_tv,
                             tooltip = paste0("Fecha: ", fecha_txt(.data[[.col_fecha]]), "\n",
                                              "Tasa de variación nominal: ", fmt_pct(.data[[.col_tv_nominal]]))),
      size = 1
    )

  facetas <- c(if (is.null(.flujo_seleccionado) && .col_flujo %in% names(.datos_df)) .col_flujo, .col_nombres)
  if (length(facetas) > 0) {
    descomposicion_tv_plt <- descomposicion_tv_plt +
      ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(facetas)))
  }

  if (is.null(.paleta_utilizada)) {
    pal <- paleta_efecto_precio_volumen()
    .paleta_utilizada <- c(pal[["Contribución precio"]], pal[["Contribución volumen"]], pal[["Variación total"]])
    names(.paleta_utilizada) <- c(etiqueta_precio, etiqueta_volumen, etiqueta_tv)
  }

  descomposicion_tv_plt |>
    graficos_estilo_victorgm(
      .suffix = " %",
      .accuracy = .accuracy,
      .title = .title,
      .title_size = .title_size,
      .subtitle = .subtitle,
      .subtitle_size = .subtitle_size,
      .caption = .caption,
      .caption_size = .caption_size,
      .legend_position = .legend_position,
      .fuente_letra = .fuente_letra,
      .paleta_utilizada = .paleta_utilizada,
      .estatico = .estatico,
      ...
    )
}
