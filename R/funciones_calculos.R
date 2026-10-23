# Saldo----
#' Calcula el saldo. Devuelve el dataframe original, pero en la columna `.col_flujo` añade una observación 'saldo' que resta `E` menos `I`.
#' @param .datos_df Data frame que contiene los datos de importación y exportación sobre los que se quiere calcular el saldo.
#' @param .col_valores Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores. Por defecto, `"valores"`.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres sobre los que se quiere calcular el saldo. Por ejemplo, si se quiere calcular el saldo para cada provincia se podrá especificar aquí. Admite varias columnas (p.ej. `c("flujo", "pais")`). Por defecto, `"nombres"`. En su ausencia señalar `.col_nombres = NULL`.
#' @param .col_flujo Cadena de caracteres. Nombre de la columna del dataframe que contiene el tipo de flujo. El valor en esta columna deberá ser `E` para las exportaciones e `I` para las importaciones.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas (en formato fecha). Por defecto, `"fecha"`.
#' @param .export Nombre que denota las exportaciones en la columna .col_flujo. Por defecto, `"E"`.
#' @param .import Nombre que denota las importaciones en la columna .col_flujo. Por defecto, `"I"`.
#' @param .saldo Nombre que denota el saldo resultante en la columna .col_flujo. Por defecto, `"Saldo"`.
#' @export
get_saldo <- function(.datos_df,
                      .col_fecha = "fecha",
                      .col_flujo = "flujo",
                      .col_nombres = "nombres",
                      .col_valores = "valores",
                      .export = "E",
                      .import = "I",
                      .saldo = "Saldo") {

  .datos_df <- .datos_df |>
    dplyr::filter(!!rlang::sym(.col_flujo) %in% c(.export, .import))

  suma_export <-
    .datos_df |>
    dplyr::filter(!!rlang::sym(.col_flujo) == .export) |>
    dplyr::mutate(suma_E = sum(!!rlang::sym(.col_valores), na.rm = TRUE)) |>
    dplyr::pull(suma_E) |>
    unique()

  suma_import <-
    .datos_df |>
    dplyr::filter(!!rlang::sym(.col_flujo) == .import) |>
    dplyr::mutate(suma_I = sum(!!rlang::sym(.col_valores), na.rm = TRUE)) |>
    dplyr::pull(suma_I) |>
    unique()

  if (is.na(suma_export) || is.na(suma_import)) {
    print("La suma de exportaciones o de importaciones devuelve un valor NA.")
    return(.datos_df)
  }

  # Verificar exportaciones negativas
  if (suma_export < 0) {
    warning("Hay datos de exportaciones negativos. No se ha calculado el saldo.")
    return(.datos_df)
  }

  # Determinar columnas de agrupación
  if (!is.null(.col_nombres)) {
    cols_agrupar <- c(.col_fecha, .col_nombres)
  } else {
    cols_agrupar <- .col_fecha
  }

  if (suma_import < 0) {
    .datos_df |>
      tidyr::pivot_wider(names_from = dplyr::all_of(.col_flujo),
                         values_from = dplyr::all_of(.col_valores)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(cols_agrupar))) |>
      dplyr::mutate(!!rlang::sym(.saldo) := !!rlang::sym(.export) + !!rlang::sym(.import)) |>
      dplyr::ungroup() |>
      tidyr::pivot_longer(
        cols = c(
          dplyr::all_of(.export),
          dplyr::all_of(.import),
          dplyr::all_of(.saldo)
        ),
        names_to = .col_flujo,
        values_to = .col_valores
      )
  } else if (suma_import >= 0 & suma_export >= 0) {
    .datos_df |>
      tidyr::pivot_wider(names_from = dplyr::all_of(.col_flujo),
                         values_from = dplyr::all_of(.col_valores)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(cols_agrupar))) |>
      dplyr::mutate(!!rlang::sym(.saldo) := !!rlang::sym(.export) - !!rlang::sym(.import)) |>
      dplyr::ungroup() |>
      tidyr::pivot_longer(
        cols = c(
          dplyr::all_of(.export),
          dplyr::all_of(.import),
          dplyr::all_of(.saldo)
        ),
        names_to = .col_flujo,
        values_to = .col_valores
      )
  }
}

# Cobertura----
#' Calcula la cobertura. Devuelve el dataframe original, pero en la columna `.col_flujo` añade una observación 'cobertura' que divide `E` entre `I`.
#' @param .datos_df Data frame que contiene los datos de importación y exportación sobre los que se quiere calcular la cobertura.
#' @param .col_valores Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores. Por defecto, `"valores"`.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres sobre los que se quiere calcular la cobertura. Por ejemplo, si se quiere calcular la cobertura para cada provincia se podrá especificar aquí. Admite varias columnas (p.ej. `c("flujo", "pais")`). Por defecto, `"nombres"`. En su ausencia señalar `.col_nombres = NULL`.
#' @param .col_flujo Cadena de caracteres. Nombre de la columna del dataframe que contiene el tipo de flujo. El valor en esta columna deberá ser `E` para las exportaciones e `I` para las importaciones.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas (en formato fecha). Por defecto, `"fecha"`.
#' @param .export Nombre que denota las exportaciones en la columna .col_flujo. Por defecto, `"E"`.
#' @param .import Nombre que denota las importaciones en la columna .col_flujo. Por defecto, `"I"`.
#' @param .cobertura Nombre que denota la cobertura resultante en la columna .col_flujo. Por defecto, `"Cobertura"`.
#' @param .en_tanto_por_uno Valor lógico. Si se establece en `TRUE` devuelve los datos en tanto por uno y en caso contrario en tanto por cien. Por defecto, en tanto por cien.
#' @export
get_cobertura <- function(.datos_df,
                      .col_fecha = "fecha",
                      .col_flujo = "flujo",
                      .col_nombres = "nombres",
                      .col_valores = "valores",
                      .export = "E",
                      .import = "I",
                      .cobertura = "Cobertura",
                      .en_tanto_por_uno = FALSE) {
  .datos_df <- .datos_df |>
    dplyr::filter(!!rlang::sym(.col_flujo) %in% c(.export, .import))

  suma_export <- sum(.datos_df[[.col_valores]][.datos_df[[.col_flujo]] == .export], na.rm = TRUE)
  suma_import <- sum(.datos_df[[.col_valores]][.datos_df[[.col_flujo]] == .import], na.rm = TRUE)

  if (is.na(suma_export) || is.na(suma_import)) {
    warning("La suma de exportaciones o de importaciones devuelve un valor NA. No se ha calculado la cobertura.")
    return(.datos_df)
  }
  if (suma_export < 0) {
    warning("Hay datos de exportaciones negativos. No se ha calculado la cobertura.")
    return(.datos_df)
  }

  factor_escala <- if (isTRUE(.en_tanto_por_uno)) 1 else 100

  .datos_df |>
    tidyr::pivot_wider(names_from = dplyr::all_of(.col_flujo),
                       values_from = dplyr::all_of(.col_valores)) |>
    dplyr::mutate(!!rlang::sym(.cobertura) := factor_escala * !!rlang::sym(.export) / !!rlang::sym(.import)) |>
    tidyr::pivot_longer(
      cols = c(dplyr::all_of(.export), dplyr::all_of(.import), dplyr::all_of(.cobertura)),
      names_to = .col_flujo,
      values_to = .col_valores
    )
}

# Tasa de variación ----
#' Calcula la tasa de variación a los periodos especificados. Devuelve el dataframe original con una columna adicional llamada `tv_(.periodos_atras)_(frecuencia)` que contiene el dato.
#' @param .datos_df Data frame que contiene los datos sobre los que se quiere calcular la tasa de variación.
#' @param .periodos_atras Número. Número de periodos sobre el que se quiera calcular la tasa de variación. Por defecto, 1 año (en el caso de datos semanales se aproximará con 52 semanas, y en el caso de datos diarios con 365 días sin tener en cuenta años bisiestos).
#' @param .frecuencia Cadena de caracteres. Indica la frecuencia de los datos. Permite `"anual"`, `"semestral"`, `"trimestral"`, `"mensual"`, `"semanal"` y `"diaria"`. Por defecto, `"mensual"`.
#' @param .col_valores Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores sobre los que se quiere calcular la tasa de variación. Por defecto, `"valores"`.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres sobre los que se quiere calcular la tasa de variación. Por ejemplo, si se quiere calcular la tasa de variación para cada provincia o para cada flujo se podrá especificar aquí `.col_nombres = c("provincia", "flujo")`. Admite varias columnas (p.ej. `c("flujo", "pais")`). Por defecto, `"nombres"`. En su ausencia señalar `.col_nombres = NULL`.
#' @param .col_flujo Cadena de caracteres. Establece el nombre de la columna que determina el sentido del flujo. Si no existiese esta columna y todas las observaciones reflejasen el mismo tipo de flujo permite establecer `.col_flujo = NULL`. Nótese que también se puede establecer la columna flujo en `.col_nombres` con el mismo efecto. Por defecto, `NULL`.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas (en formato fecha). La fórmula entiende que los datos son mensuales. Por defecto, `"fecha"`.
#' @param .en_tanto_por_uno Valor lógico. Si se establece en `TRUE` devuelve los datos en tanto por uno y en caso contrario en tanto por cien. Por defecto, en tanto por cien.
#' @export
get_tasa_variacion <- function(.datos_df,
                               .periodos_atras = NULL,
                               .frecuencia = "mensual",
                               .col_valores = "valores",
                               .col_nombres = "nombres",
                               .col_flujo = NULL,
                               .col_fecha = "fecha",
                               .en_tanto_por_uno = FALSE) {
  stopifnot(
    "El input debe ser un data frame." = is.data.frame(.datos_df),
    "No existe la columna señalada para la fecha." = .col_fecha %in% names(.datos_df),
    "No existe la columna señalada para los valores." = .col_valores %in% names(.datos_df)
  )

  if (!inherits(.datos_df[[.col_fecha]], "Date")) {
    warning("Se ha convertido la columna de fechas a formato fecha.")
    .datos_df[[.col_fecha]] <- as.Date(.datos_df[[.col_fecha]])
  }

  .datos_df <-
    .datos_df[!is.na(.datos_df[[.col_fecha]]), ]

  if(!is.null(.periodos_atras)){
    periodos_atras <- .periodos_atras
  } else if (.frecuencia == "anual"){
    periodos_atras <- 1
  } else if (.frecuencia == "semestral"){
    periodos_atras <- 2
  } else if (.frecuencia == "trimestral"){
    periodos_atras <- 4
  } else if (.frecuencia == "mensual"){
    periodos_atras <- 12
  } else if (.frecuencia == "semanal"){
    periodos_atras <- 52
  } else if (.frecuencia == "diaria"){
    periodos_atras <- 365
  }

  if (.frecuencia == "anual"){
    variable_secuencia <- "year"
    periodo <- "años"
  } else if (.frecuencia == "semestral"){
    variable_secuencia <- "6 months"
    periodo <- "semestres"
  } else if (.frecuencia == "trimestral"){
    variable_secuencia <- "quarter"
    periodo <- "trimestres"
  } else if (.frecuencia == "mensual"){
    variable_secuencia <- "month"
    periodo <- "meses"
  } else if (.frecuencia == "semanal"){
    variable_secuencia <- "week"
    periodo <- "semanas"
  } else if (.frecuencia == "diaria"){
    variable_secuencia <- "day"
    periodo <- "dias"
  }

  min_date <- min(.datos_df[[.col_fecha]], na.rm = TRUE)
  max_date <- max(.datos_df[[.col_fecha]], na.rm = TRUE)

  if (is.infinite(min_date) || is.infinite(max_date)) {
    stop("Las fechas no tienen un formato válido.")
  }

  date_seq <- seq.Date(from = min_date,
                       to = max_date,
                       by = variable_secuencia)

  if (is.null(.col_nombres) & is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(stats::setNames(list(0), .col_valores))
  } else if (!is.null(.col_nombres) & is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  } else if (is.null(.col_nombres) & !is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  } else if (!is.null(.col_nombres) & !is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  }

  col_tv <- paste0("tv_", periodos_atras, "_", periodo)
  primeras_fechas <- sort(unique(.datos_df[[.col_fecha]]))[1:(periodos_atras - 1)]

  if(is.null(.col_nombres) & is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_tv := (!!rlang::sym(.col_valores) - dplyr::lag(!!rlang::sym(.col_valores), periodos_atras)) / dplyr::lag(!!rlang::sym(.col_valores), periodos_atras)) |>
      dplyr::ungroup() |>
      dplyr::group_by(dplyr::across(all_of(.col_fecha))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_tv := dplyr::case_when(
        .data[[.col_fecha]] %in% primeras_fechas ~ NA_real_,  # Reemplaza por NA los primeros periodos
        TRUE ~ !!rlang::sym(col_tv))) |>
      dplyr::ungroup()
  } else if(!is.null(.col_nombres) & is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)))  |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_tv := (!!rlang::sym(.col_valores) - dplyr::lag(!!rlang::sym(.col_valores), periodos_atras)) / dplyr::lag(!!rlang::sym(.col_valores), periodos_atras)) |>
      dplyr::ungroup() |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_tv := dplyr::case_when(
        .data[[.col_fecha]] %in% primeras_fechas ~ NA_real_,  # Reemplaza por NA los primeros periodos
        TRUE ~ !!rlang::sym(col_tv))) |>
      dplyr::ungroup()
  } else if(is.null(.col_nombres) & !is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo)))  |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_tv := (!!rlang::sym(.col_valores) - dplyr::lag(!!rlang::sym(.col_valores), periodos_atras)) / dplyr::lag(!!rlang::sym(.col_valores), periodos_atras)) |>
      dplyr::ungroup() |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_tv := dplyr::case_when(
        .data[[.col_fecha]] %in% primeras_fechas ~ NA_real_,  # Reemplaza por NA los primeros periodos
        TRUE ~ !!rlang::sym(col_tv))) |>
      dplyr::ungroup()
  } else {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo)))  |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_tv := (!!rlang::sym(.col_valores) - dplyr::lag(!!rlang::sym(.col_valores), periodos_atras)) / dplyr::lag(!!rlang::sym(.col_valores), periodos_atras)) |>
      dplyr::ungroup() |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_tv := dplyr::case_when(
        .data[[.col_fecha]] %in% primeras_fechas ~ NA_real_,  # Reemplaza por NA los primeros periodos
        TRUE ~ !!rlang::sym(col_tv))) |>
      dplyr::ungroup()
  }

  if(!.en_tanto_por_uno){
    .datos_df <-
      .datos_df |>
      dplyr::mutate(!!rlang::sym(col_tv) := !!rlang::sym(col_tv) * 100)
  }
  return(.datos_df)
}

# Tasa de variación con respecto a una fecha ----
#' Calcula la tasa de variación de una variable con respecto al valor alcanzado en la fecha seleccionada. Devuelve el dataframe original con una columna adicional llamada `tv_respecto_(.fecha_referencia)`.
#' @param .datos_df Data frame que contiene los datos sobre los que se quiere calcular la tasa de variación.
#' @param .fecha_referencia Fecha. Periodo con respecto al que se quiere obtener la comparación.
#' @param .frecuencia Cadena de caracteres. Indica la frecuencia de los datos. Permite `"anual"`, `"semestral"`, `"trimestral"`, `"mensual"`, `"semanal"` y `"diaria"`. Por defecto, `"mensual"`.
#' @param .col_valores Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores sobre los que se quiere calcular la tasa de variación. Por defecto, `"valores"`.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres sobre los que se quiere calcular la tasa de variación. Por ejemplo, si se quiere calcular la tasa de variación para cada provincia o para cada flujo se podrá especificar aquí. Admite varias columnas (p.ej. `c("flujo", "pais")`). Por defecto, "nombres". En su ausencia señalar `.col_nombres = NULL`.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas (en formato fecha). La fórmula entiende que los datos son mensuales. Por defecto, `"fecha"`.
#' @param .col_flujo Cadena de caracteres. Establece el nombre de la columna que determina el sentido del flujo. Si no existiese esta columna y todas las observaciones reflejasen el mismo tipo de flujo permite establecer `.col_flujo = NULL`. Nótese que también se puede establecer la columna flujo en `.col_nombres` con el mismo efecto. Por defecto, `NULL`.
#' @param .en_tanto_por_uno Valor lógico. Si se establece en `TRUE` devuelve los datos en tanto por uno y en caso contrario en tanto por cien. Por defecto, en tanto por cien.
#' @param .completar_df Valor lógico. Indica si quieres rellenar los datos faltantes con ceros. Por defecto, `FALSE`.
#' @export
get_tasa_variacion_fecha_ref <- function(.datos_df,
                                         .fecha_referencia,
                                         .frecuencia = "mensual",
                                         .col_valores = "valores",
                                         .col_nombres = "nombres",
                                         .col_fecha = "fecha",
                                         .col_flujo = NULL,
                                         .en_tanto_por_uno = FALSE,
                                         .completar_df = FALSE) {
  stopifnot(
    "El input debe ser un data frame." = is.data.frame(.datos_df),
    "No existe la columna señalada para la fecha." = .col_fecha %in% names(.datos_df),
    "No existe la columna señalada para los valores." = .col_valores %in% names(.datos_df)
  )

  .fecha_referencia <- as.Date(.fecha_referencia)
  fecha_referencia_character <-
    as.character(.fecha_referencia) |>
    stringr::str_replace_all("-", "_")

  if (!inherits(.datos_df[[.col_fecha]], "Date")) {
    warning("Se ha convertido la columna de fechas a formato fecha.")
    .datos_df[[.col_fecha]] <- as.Date(.datos_df[[.col_fecha]])
  }

  .datos_df <-
    .datos_df[!is.na(.datos_df[[.col_fecha]]), ]

  min_date <- min(.datos_df[[.col_fecha]], na.rm = TRUE)
  max_date <- max(.datos_df[[.col_fecha]], na.rm = TRUE)

  if (is.infinite(min_date) || is.infinite(max_date)) {
    stop("Las fechas no tienen un formato válido.")
  }

  if (.completar_df == TRUE){
    if (.frecuencia == "anual"){
      variable_secuencia <- "year"
    } else if (.frecuencia == "semestral"){
      variable_secuencia <- "6 months"
    } else if (.frecuencia == "trimestral"){
      variable_secuencia <- "quarter"
    } else if (.frecuencia == "mensual"){
      variable_secuencia <- "month"
    } else if (.frecuencia == "semanal"){
      variable_secuencia <- "week"
    } else if (.frecuencia == "diaria"){
      variable_secuencia <- "day"
    }

    date_seq <- seq.Date(from = min_date,
                         to = max_date,
                         by = variable_secuencia)

    if (is.null(.col_nombres) & is.null(.col_flujo)) {
      .datos_df <-
        .datos_df |>
        tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
        tidyr::replace_na(stats::setNames(list(0), .col_valores))
      } else if (!is.null(.col_nombres) & is.null(.col_flujo)) {
      .datos_df <-
        .datos_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres))) |>
        tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
        tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
        dplyr::ungroup()
      } else if (is.null(.col_nombres) & !is.null(.col_flujo)) {
        .datos_df <-
          .datos_df |>
          dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo))) |>
          tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
          tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
          dplyr::ungroup()
      } else if (!is.null(.col_nombres) & !is.null(.col_flujo)) {
        .datos_df <-
          .datos_df |>
          dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo))) |>
          tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
          tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
          dplyr::ungroup()
      }
  }

  col_tv <- paste0("tv_respecto_", fecha_referencia_character)
  # valor_referencia <-
  #   .datos_df |>
  #   dplyr::filter(.col_fecha == .fecha_referencia) |>
  #   dplyr::pull(.col_valores)

  if (is.null(.col_nombres) & is.null(.col_flujo)) {
    valor_referencia <-
      .datos_df |>
      dplyr::filter(!!rlang::sym(.col_fecha) == .fecha_referencia) |>
      dplyr::pull(.col_valores)

    .datos_df <-
      .datos_df |>
      dplyr::mutate(valores_referencia = valor_referencia)

    .datos_df <-
      .datos_df |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_tv := (!!rlang::sym(.col_valores) - valores_referencia) / valores_referencia) |>
      dplyr::ungroup()
  } else if (!is.null(.col_nombres) & is.null(.col_flujo)) {
    valores_referencia <-
      .datos_df |>
      dplyr::filter(!!rlang::sym(.col_fecha) == .fecha_referencia) |>
      dplyr::select(dplyr::all_of(.col_nombres), dplyr::all_of(.col_valores))

    .datos_df <-
      .datos_df |>
      dplyr::left_join(valores_referencia, by = .col_nombres, suffix = c("", "_referencia"))

    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)))  |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_tv := (!!rlang::sym(.col_valores) - valores_referencia) / valores_referencia) |>
      dplyr::select(-valores_referencia) |>
      dplyr::ungroup()
  } else if (is.null(.col_nombres) & !is.null(.col_flujo)) {
    valores_referencia <-
      .datos_df |>
      dplyr::filter(!!rlang::sym(.col_fecha) == .fecha_referencia) |>
      dplyr::select(dplyr::all_of(.col_flujo), dplyr::all_of(.col_valores))

    .datos_df <-
      .datos_df |>
      dplyr::left_join(valores_referencia, by = .col_flujo, suffix = c("", "_referencia"))

    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo)))  |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_tv := (!!rlang::sym(.col_valores) - valores_referencia) / valores_referencia) |>
      dplyr::select(-valores_referencia) |>
      dplyr::ungroup()
  } else if (!is.null(.col_nombres) & !is.null(.col_flujo)) {
    valores_referencia <-
      .datos_df |>
      dplyr::filter(!!rlang::sym(.col_fecha) == .fecha_referencia) |>
      dplyr::select(dplyr::all_of(.col_flujo), dplyr::all_of(.col_nombres), dplyr::all_of(.col_valores))

    .datos_df <-
      .datos_df |>
      dplyr::left_join(valores_referencia, by = c(.col_nombres, .col_flujo), suffix = c("", "_referencia"))

    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres), dplyr::across(dplyr::all_of(.col_flujo))))  |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_tv := (!!rlang::sym(.col_valores) - valores_referencia) / valores_referencia) |>
      dplyr::select(-valores_referencia) |>
      dplyr::ungroup()
  }

  if(!.en_tanto_por_uno){
    .datos_df <-
      .datos_df |>
      dplyr::mutate(!!rlang::sym(col_tv) := !!rlang::sym(col_tv) * 100)
  }

  return(.datos_df)
}

# Variación ----
#' Calcula la variación (que no la tasa de variación) a los periodos especificados. Devuelve el dataframe original con una columna adicional llamada `variacion_(.periodos_atras)_(frecuencia)` que contiene el dato.
#' @param .datos_df Data frame que contiene los datos sobre los que se quiere calcular la variación.
#' @param .periodos_atras Número. Número de periodos sobre el que se quiera calcular la variación. Por defecto, 1 año (en el caso de datos semanales se aproximará con 52 semanas, y en el caso de datos diarios con 365 días sin tener en cuenta años bisiestos).
#' @param .frecuencia Cadena de caracteres. Indica la frecuencia de los datos. Permite `"anual"`, `"semestral"`, `"trimestral"`, `"mensual"`, `"semanal"` y `"diaria"`. Por defecto, `"mensual"`.
#' @param .col_valores Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores sobre los que se quiere calcular la variación. Por defecto, `"valores"`.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres sobre los que se quiere calcular la variación. Por ejemplo, si se quiere calcular la variación para cada provincia o para cada flujo se podrá especificar aquí `.col_nombres = c("provincia", "flujo")`. Admite varias columnas (p.ej. `c("flujo", "pais")`). Por defecto, `"nombres"`. En su ausencia señalar `.col_nombres = NULL`.
#' @param .col_flujo Cadena de caracteres. Establece el nombre de la columna que determina el sentido del flujo. Si no existiese esta columna y todas las observaciones reflejasen el mismo tipo de flujo permite establecer `.col_flujo = NULL`. Por defecto, `NULL`. Nótese que también se puede establecer la columna flujo en `.col_nombres` con el mismo efecto.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas (en formato fecha). La fórmula entiende que los datos son mensuales. Por defecto, `"fecha"`.
#' @export
get_variacion <- function(.datos_df,
                          .periodos_atras = NULL,
                          .frecuencia = "mensual",
                          .col_valores = "valores",
                          .col_nombres = "nombres",
                          .col_flujo = NULL,
                          .col_fecha = "fecha") {
  stopifnot(
    "El input debe ser un data frame." = is.data.frame(.datos_df),
    "No existe la columna señalada para la fecha." = .col_fecha %in% names(.datos_df),
    "No existe la columna señalada para los valores." = .col_valores %in% names(.datos_df)
  )

  if (!inherits(.datos_df[[.col_fecha]], "Date")) {
    warning("Se ha convertido la columna de fechas a formato fecha.")
    .datos_df[[.col_fecha]] <- as.Date(.datos_df[[.col_fecha]])
  }

  .datos_df <-
    .datos_df[!is.na(.datos_df[[.col_fecha]]), ]

  if(!is.null(.periodos_atras)){
    periodos_atras <- .periodos_atras
  } else if (.frecuencia == "anual"){
    periodos_atras <- 1
  } else if (.frecuencia == "semestral"){
    periodos_atras <- 2
  } else if (.frecuencia == "trimestral"){
    periodos_atras <- 4
  } else if (.frecuencia == "mensual"){
    periodos_atras <- 12
  } else if (.frecuencia == "semanal"){
    periodos_atras <- 52
  } else if (.frecuencia == "diaria"){
    periodos_atras <- 365
  }

  if (.frecuencia == "anual"){
    variable_secuencia <- "year"
    periodo <- "años"
  } else if (.frecuencia == "semestral"){
    variable_secuencia <- "6 months"
    periodo <- "semestres"
  } else if (.frecuencia == "trimestral"){
    variable_secuencia <- "quarter"
    periodo <- "trimestres"
  } else if (.frecuencia == "mensual"){
    variable_secuencia <- "month"
    periodo <- "meses"
  } else if (.frecuencia == "semanal"){
    variable_secuencia <- "week"
    periodo <- "semanas"
  } else if (.frecuencia == "diaria"){
    variable_secuencia <- "day"
    periodo <- "dias"
  }

  min_date <- min(.datos_df[[.col_fecha]], na.rm = TRUE)
  max_date <- max(.datos_df[[.col_fecha]], na.rm = TRUE)

  if (is.infinite(min_date) || is.infinite(max_date)) {
    stop("Las fechas no tienen un formato válido.")
  }

  date_seq <- seq.Date(from = min_date,
                       to = max_date,
                       by = variable_secuencia)

  if (is.null(.col_nombres) & is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(stats::setNames(list(0), .col_valores))
  } else if (!is.null(.col_nombres) & is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  } else if (is.null(.col_nombres) & !is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  } else if (!is.null(.col_nombres) & !is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  }

  col_variacion <- paste0("variacion_", periodos_atras, "_", periodo)
  primeras_fechas <- sort(unique(.datos_df[[.col_fecha]]))[1:(periodos_atras - 1)]

  if(is.null(.col_nombres) & is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_variacion := (!!rlang::sym(.col_valores) - dplyr::lag(!!rlang::sym(.col_valores), periodos_atras))) |>
      dplyr::ungroup() |>
      dplyr::group_by(dplyr::across(all_of(.col_fecha))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_variacion := dplyr::case_when(
        .data[[.col_fecha]] %in% primeras_fechas ~ NA_real_,  # Reemplaza por NA los primeros periodos
        TRUE ~ !!rlang::sym(col_variacion))) |>
      dplyr::ungroup()
  } else if(!is.null(.col_nombres) & is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)))  |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_variacion := (!!rlang::sym(.col_valores) - dplyr::lag(!!rlang::sym(.col_valores), periodos_atras))) |>
      dplyr::ungroup() |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_variacion := dplyr::case_when(
        .data[[.col_fecha]] %in% primeras_fechas ~ NA_real_,  # Reemplaza por NA los primeros periodos
        TRUE ~ !!rlang::sym(col_variacion))) |>
      dplyr::ungroup()
  } else if(is.null(.col_nombres) & !is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo)))  |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_variacion := (!!rlang::sym(.col_valores) - dplyr::lag(!!rlang::sym(.col_valores), periodos_atras))) |>
      dplyr::ungroup() |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_variacion := dplyr::case_when(
        .data[[.col_fecha]] %in% primeras_fechas ~ NA_real_,  # Reemplaza por NA los primeros periodos
        TRUE ~ !!rlang::sym(col_variacion))) |>
      dplyr::ungroup()
  } else {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo)))  |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_variacion := (!!rlang::sym(.col_valores) - dplyr::lag(!!rlang::sym(.col_valores), periodos_atras))) |>
      dplyr::ungroup() |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_variacion := dplyr::case_when(
        .data[[.col_fecha]] %in% primeras_fechas ~ NA_real_,  # Reemplaza por NA los primeros periodos
        TRUE ~ !!rlang::sym(col_variacion))) |>
      dplyr::ungroup()
  }

  return(.datos_df)
}

# Variación con respecto a una fecha ----
#' Calcula la variación (que no la tasa) de una variable con respecto al valor alcanzado en la fecha seleccionada. Devuelve el dataframe original con una columna adicional llamada `variacion_respecto_(.fecha_referencia)`.
#' @param .datos_df Data frame que contiene los datos sobre los que se quiere calcular la variación.
#' @param .fecha_referencia Fecha. Periodo con respecto al que se quiere obtener la comparación.
#' @param .frecuencia Cadena de caracteres. Indica la frecuencia de los datos. Permite `"anual"`, `"semestral"`, `"trimestral"`, `"mensual"`, `"semanal"` y `"diaria"`. Por defecto, `"mensual"`.
#' @param .col_valores Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores sobre los que se quiere calcular la variación. Por defecto, `"valores"`.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres sobre los que se quiere calcular la variación. Por ejemplo, si se quiere calcular la variación para cada provincia o para cada flujo se podrá especificar aquí. Admite varias columnas (p.ej. `c("flujo", "pais")`). Por defecto, "nombres". En su ausencia señalar `.col_nombres = NULL`.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas (en formato fecha). La fórmula entiende que los datos son mensuales. Por defecto, `"fecha"`.
#' @param .col_flujo Cadena de caracteres. Establece el nombre de la columna que determina el sentido del flujo. Si no existiese esta columna y todas las observaciones reflejasen el mismo tipo de flujo permite establecer `.col_flujo = NULL`. Nótese que también se puede establecer la columna flujo en `.col_nombres` con el mismo efecto. Por defecto, `NULL`.
#' @param .completar_df Valor lógico. Indica si quieres rellenar los datos faltantes con ceros. Por defecto, `FALSE`.
#' @export
get_variacion_fecha_ref <- function(.datos_df,
                                    .fecha_referencia,
                                    .frecuencia = "mensual",
                                    .col_valores = "valores",
                                    .col_nombres = "nombres",
                                    .col_fecha = "fecha",
                                    .col_flujo = NULL,
                                    .completar_df = FALSE) {
  stopifnot(
    "El input debe ser un data frame." = is.data.frame(.datos_df),
    "No existe la columna señalada para la fecha." = .col_fecha %in% names(.datos_df),
    "No existe la columna señalada para los valores." = .col_valores %in% names(.datos_df)
  )

  .fecha_referencia <- as.Date(.fecha_referencia)
  fecha_referencia_character <-
    as.character(.fecha_referencia) |>
    stringr::str_replace_all("-", "_")

  if (!inherits(.datos_df[[.col_fecha]], "Date")) {
    warning("Se ha convertido la columna de fechas a formato fecha.")
    .datos_df[[.col_fecha]] <- as.Date(.datos_df[[.col_fecha]])
  }

  .datos_df <-
    .datos_df[!is.na(.datos_df[[.col_fecha]]), ]

  min_date <- min(.datos_df[[.col_fecha]], na.rm = TRUE)
  max_date <- max(.datos_df[[.col_fecha]], na.rm = TRUE)

  if (is.infinite(min_date) || is.infinite(max_date)) {
    stop("Las fechas no tienen un formato válido.")
  }

  if (.completar_df == TRUE){
    if (.frecuencia == "anual"){
      variable_secuencia <- "year"
    } else if (.frecuencia == "semestral"){
      variable_secuencia <- "6 months"
    } else if (.frecuencia == "trimestral"){
      variable_secuencia <- "quarter"
    } else if (.frecuencia == "mensual"){
      variable_secuencia <- "month"
    } else if (.frecuencia == "semanal"){
      variable_secuencia <- "week"
    } else if (.frecuencia == "diaria"){
      variable_secuencia <- "day"
    }

    date_seq <- seq.Date(from = min_date,
                         to = max_date,
                         by = variable_secuencia)

    if (is.null(.col_nombres) & is.null(.col_flujo)) {
      .datos_df <-
        .datos_df |>
        tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
        tidyr::replace_na(stats::setNames(list(0), .col_valores))
    } else if (!is.null(.col_nombres) & is.null(.col_flujo)) {
      .datos_df <-
        .datos_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres))) |>
        tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
        tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
        dplyr::ungroup()
    } else if (is.null(.col_nombres) & !is.null(.col_flujo)) {
      .datos_df <-
        .datos_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo))) |>
        tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
        tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
        dplyr::ungroup()
    } else if (!is.null(.col_nombres) & !is.null(.col_flujo)) {
      .datos_df <-
        .datos_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo))) |>
        tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
        tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
        dplyr::ungroup()
    }
  }

  col_variacion <- paste0("variacion_respecto_", fecha_referencia_character)
  # valor_referencia <-
  #   .datos_df |>
  #   dplyr::filter(.col_fecha == .fecha_referencia) |>
  #   dplyr::pull(.col_valores)

  if (is.null(.col_nombres) & is.null(.col_flujo)) {
    valor_referencia <-
      .datos_df |>
      dplyr::filter(!!rlang::sym(.col_fecha) == .fecha_referencia) |>
      dplyr::pull(.col_valores)

    .datos_df <-
      .datos_df |>
      dplyr::mutate(valores_referencia = valor_referencia)

    .datos_df <-
      .datos_df |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_variacion := (!!rlang::sym(.col_valores) - valores_referencia)) |>
      dplyr::ungroup()
  } else if (!is.null(.col_nombres) & is.null(.col_flujo)) {
    valores_referencia <-
      .datos_df |>
      dplyr::filter(!!rlang::sym(.col_fecha) == .fecha_referencia) |>
      dplyr::select(dplyr::all_of(.col_nombres), dplyr::all_of(.col_valores))

    .datos_df <-
      .datos_df |>
      dplyr::left_join(valores_referencia, by = .col_nombres, suffix = c("", "_referencia"))

    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)))  |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_variacion := (!!rlang::sym(.col_valores) - valores_referencia)) |>
      dplyr::select(-valores_referencia) |>
      dplyr::ungroup()
  } else if (is.null(.col_nombres) & !is.null(.col_flujo)) {
    valores_referencia <-
      .datos_df |>
      dplyr::filter(!!rlang::sym(.col_fecha) == .fecha_referencia) |>
      dplyr::select(dplyr::all_of(.col_flujo), dplyr::all_of(.col_valores))

    .datos_df <-
      .datos_df |>
      dplyr::left_join(valores_referencia, by = .col_flujo, suffix = c("", "_referencia"))

    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo)))  |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_variacion := (!!rlang::sym(.col_valores) - valores_referencia)) |>
      dplyr::select(-valores_referencia) |>
      dplyr::ungroup()
  } else if (!is.null(.col_nombres) & !is.null(.col_flujo)) {
    valores_referencia <-
      .datos_df |>
      dplyr::filter(!!rlang::sym(.col_fecha) == .fecha_referencia) |>
      dplyr::select(dplyr::all_of(.col_flujo), dplyr::all_of(.col_nombres), dplyr::all_of(.col_valores))

    .datos_df <-
      .datos_df |>
      dplyr::left_join(valores_referencia, by = c(.col_nombres, .col_flujo), suffix = c("", "_referencia"))

    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres), dplyr::across(dplyr::all_of(.col_flujo))))  |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_variacion := (!!rlang::sym(.col_valores) - valores_referencia)) |>
      dplyr::select(-valores_referencia) |>
      dplyr::ungroup()
  }

  return(.datos_df)
}

# Acumulado ----
#' Calcula el valor acumulado a los periodos especificados. Devuelve el dataframe original con una columna adicional llamada `acumulado_(.periodos_atras)_(frecuencia)` que contiene el cálculo de la suma de los últimos periodos.
#' @param .datos_df Data frame que contiene los datos sobre los que se quiere calcular el acumulado.
#' @param .periodos_atras Número. Número de periodos sobre el que se quiera calcular el acumulado. Por defecto, 1 año (en el caso de datos semanales se aproximará con 52 semanas, y en el caso de datos diarios con 365 días sin tener en cuenta años bisiestos).
#' @param .frecuencia Cadena de caracteres. Indica la frecuencia de los datos. Permite `"anual"`, `"semestral"`, `"trimestral"`, `"mensual"`, `"semanal"` y `"diaria"`. Por defecto, `"mensual"`.
#' @param .col_valores Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores sobre los que se quiere calcular el acumulado. Por defecto, `"valores"`.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres sobre los que se quiere calcular el acumulado. Por ejemplo, si se quiere calcular el acumulado para cada provincia o para cada flujo se podrá especificar aquí. Admite varias columnas (p.ej. `c("flujo", "pais")`). Por defecto, `"nombres"`. En su ausencia señalar `.col_nombres = NULL`.
#' @param .col_flujo Cadena de caracteres. Establece el nombre de la columna que determina el sentido del flujo. Si no existiese esta columna y todas las observaciones reflejasen el mismo tipo de flujo permite establecer `.col_flujo = NULL`. Nótese que también se puede establecer la columna flujo en `.col_nombres` con el mismo efecto. Por defecto, `NULL`.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas (en formato fecha). Se entiende que habrá un dato mensual. Por defecto, `"fecha"`.
#' @export
get_acumulado <- function(.datos_df,
                          .periodos_atras = NULL,
                          .frecuencia = "mensual",
                          .col_valores = "valores",
                          .col_nombres = "nombres",
                          .col_flujo = NULL,
                          .col_fecha = "fecha") {
  stopifnot(
    "El input debe ser un data frame." = is.data.frame(.datos_df),
    "No existe la columna señalada para la fecha." = .col_fecha %in% names(.datos_df),
    "No existe la columna señalada para los valores." = .col_valores %in% names(.datos_df)
  )

  # if (!is.null(.col_nombres)) {
  #   stopifnot("Las columnas especificadas en .col_nombres no existen." = all(.col_nombres %in% names(.datos_df)))
  # }

  if (!inherits(.datos_df[[.col_fecha]], "Date")) {
    warning("Se ha convertido la columna de fechas a formato fecha.")
    .datos_df[[.col_fecha]] <- as.Date(.datos_df[[.col_fecha]])
  }

  if(!is.null(.periodos_atras)){
    periodos_atras <- .periodos_atras
  } else if (.frecuencia == "anual"){
    periodos_atras <- 1
  } else if (.frecuencia == "semestral"){
    periodos_atras <- 2
  } else if (.frecuencia == "trimestral"){
    periodos_atras <- 4
  } else if (.frecuencia == "mensual"){
    periodos_atras <- 12
  } else if (.frecuencia == "semanal"){
    periodos_atras <- 52
  } else if (.frecuencia == "diaria"){
    periodos_atras <- 365
  }

  if (.frecuencia == "anual"){
    variable_secuencia <- "year"
    periodo <- "años"
  } else if (.frecuencia == "semestral"){
    variable_secuencia <- "6 months"
    periodo <- "semestres"
  } else if (.frecuencia == "trimestral"){
    variable_secuencia <- "quarter"
    periodo <- "trimestres"
  } else if (.frecuencia == "mensual"){
    variable_secuencia <- "month"
    periodo <- "meses"
  } else if (.frecuencia == "semanal"){
    variable_secuencia <- "week"
    periodo <- "semanas"
  } else if (.frecuencia == "diaria"){
    variable_secuencia <- "day"
    periodo <- "dias"
  }

  .datos_df <-
    .datos_df[!is.na(.datos_df[[.col_fecha]]), ]

  min_date <- min(.datos_df[[.col_fecha]], na.rm = TRUE)
  max_date <- max(.datos_df[[.col_fecha]], na.rm = TRUE)

  if (is.infinite(min_date) || is.infinite(max_date)) {
    stop("Las fechas no tienen un formato válido.")
  }

  date_seq <- seq.Date(from = min_date,
                       to = max_date,
                       by = variable_secuencia)

  if (is.null(.col_nombres) & is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(stats::setNames(list(0), .col_valores))
  } else if (!is.null(.col_nombres) & is.null(.col_flujo)){
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  } else if (is.null(.col_nombres) & !is.null(.col_flujo)){
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  } else if (!is.null(.col_nombres) & !is.null(.col_flujo)){
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  }

  col_acumulado <- paste0("acumulado_", periodos_atras, "_", periodo)

  primeras_fechas <- sort(unique(.datos_df[[.col_fecha]]))[1:(periodos_atras - 1)]

  if (is.null(.col_nombres) & is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!rlang::sym(col_acumulado) := zoo::rollapply(.data[[.col_valores]], width = periodos_atras, FUN = sum, align = "right", fill = NA, partial = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!rlang::sym(col_acumulado) := dplyr::case_when(
        .data[[.col_fecha]] %in% primeras_fechas ~ NA_real_,  # Reemplaza por NA los primeros periodos
        TRUE ~ !!rlang::sym(col_acumulado))) |>
      dplyr::ungroup()
  } else if (!is.null(.col_nombres) & is.null(.col_flujo)){
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)))  |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!rlang::sym(col_acumulado) := zoo::rollapply(.data[[.col_valores]], width = periodos_atras, FUN = sum, align = "right", fill = NA, partial = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!rlang::sym(col_acumulado) := dplyr::case_when(
        .data[[.col_fecha]] %in% primeras_fechas ~ NA_real_,  # Reemplaza por NA los primeros periodos
        TRUE ~ !!rlang::sym(col_acumulado))) |>
      dplyr::ungroup()
  } else if (is.null(.col_nombres) & !is.null(.col_flujo)){
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo)))  |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!rlang::sym(col_acumulado) := zoo::rollapply(.data[[.col_valores]], width = periodos_atras, FUN = sum, align = "right", fill = NA, partial = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!rlang::sym(col_acumulado) := dplyr::case_when(
        .data[[.col_fecha]] %in% primeras_fechas ~ NA_real_,  # Reemplaza por NA los primeros periodos
        TRUE ~ !!rlang::sym(col_acumulado))) |>
      dplyr::ungroup()
  } else if (!is.null(.col_nombres) & !is.null(.col_flujo)){
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo)))  |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!rlang::sym(col_acumulado) := zoo::rollapply(.data[[.col_valores]], width = periodos_atras, FUN = sum, align = "right", fill = NA, partial = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!rlang::sym(col_acumulado) := dplyr::case_when(
        .data[[.col_fecha]] %in% primeras_fechas ~ NA_real_,  # Reemplaza por NA los primeros periodos
        TRUE ~ !!rlang::sym(col_acumulado))) |>
      dplyr::ungroup()
  }

  return(.datos_df)
}

# Acumulado optimizado con duckplyr ----
#' Calcula el valor acumulado a los periodos especificados (versión optimizada).
#' @param .datos_df Data frame que contiene los datos sobre los que se quiere calcular el acumulado.
#' @param .periodos_atras Número. Número de periodos sobre el que se quiera calcular el acumulado.
#' @param .frecuencia Cadena de caracteres. Indica la frecuencia de los datos.
#' @param .col_valores Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres.
#' @param .col_flujo Cadena de caracteres. Establece el nombre de la columna que determina el sentido del flujo.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas.
#' @param .use_duckplyr Valor lógico. Si TRUE, usa duckplyr para optimización. Por defecto TRUE.
#' @export
get_acumulado_duckplyr <- function(.datos_df,
                          .periodos_atras = NULL,
                          .frecuencia = "mensual",
                          .col_valores = "valores",
                          .col_nombres = "nombres",
                          .col_flujo = NULL,
                          .col_fecha = "fecha",
                          .use_duckplyr = TRUE) {

  # Validaciones iniciales
  stopifnot(
    "El input debe ser un data frame." = is.data.frame(.datos_df),
    "No existe la columna señalada para la fecha." = .col_fecha %in% names(.datos_df),
    "No existe la columna señalada para los valores." = .col_valores %in% names(.datos_df)
  )

  # Activar duckplyr si está disponible y solicitado
  if (.use_duckplyr && requireNamespace("duckplyr", quietly = TRUE)) {
    .datos_df <- duckplyr::as_duckplyr_df(.datos_df)
  }

  # Conversión de fecha optimizada
  if (!inherits(.datos_df[[.col_fecha]], "Date")) {
    warning("Se ha convertido la columna de fechas a formato fecha.")
    .datos_df[[.col_fecha]] <- as.Date(.datos_df[[.col_fecha]])
  }

  # Filtrar NAs de fecha de manera eficiente
  .datos_df <- .datos_df |> dplyr::filter(!is.na(!!rlang::sym(.col_fecha)))

  # Calcular parámetros de secuencia
  if (!is.null(.periodos_atras)) {
    periodos_atras <- .periodos_atras
  } else {
    periodos_atras <- switch(.frecuencia,
                             "anual" = 1, "semestral" = 2, "trimestral" = 4,
                             "mensual" = 12, "semanal" = 52, "diaria" = 365
    )
  }

  variable_secuencia <- switch(.frecuencia,
                               "anual" = "year", "semestral" = "6 months", "trimestral" = "quarter",
                               "mensual" = "month", "semanal" = "week", "diaria" = "day"
  )

  periodo <- switch(.frecuencia,
                    "anual" = "años", "semestral" = "semestres", "trimestral" = "trimestres",
                    "mensual" = "meses", "semanal" = "semanas", "diaria" = "dias"
  )

  # Generar secuencia de fechas
  date_range <- range(.datos_df[[.col_fecha]], na.rm = TRUE)
  if (any(is.infinite(date_range))) {
    stop("Las fechas no tienen un formato válido.")
  }

  date_seq <- seq.Date(from = date_range[1], to = date_range[2], by = variable_secuencia)

  # Completar series temporales de manera optimizada
  grouping_cols <- c(.col_nombres[!is.null(.col_nombres)], .col_flujo[!is.null(.col_flujo)])

  if (length(grouping_cols) > 0) {
    .datos_df <- .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  } else {
    .datos_df <- .datos_df |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(stats::setNames(list(0), .col_valores))
  }

  # Calcular acumulado usando funciones de ventana optimizadas
  col_acumulado <- paste0("acumulado_", periodos_atras, "_", periodo)
  primeras_fechas <- sort(unique(.datos_df[[.col_fecha]]))[1:(periodos_atras - 1)]

  if (length(grouping_cols) > 0) {
    resultado <- .datos_df |>
      dplyr::arrange(dplyr::across(dplyr::all_of(c(grouping_cols, .col_fecha)))) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) |>
      dplyr::mutate(
        !!col_acumulado := dplyr::case_when(
          !!rlang::sym(.col_fecha) %in% primeras_fechas ~ NA_real_,
          TRUE ~ sum(!!rlang::sym(.col_valores), na.rm = TRUE) |>
            slider::slide_dbl(.before = periodos_atras - 1, .complete = TRUE)
        )
      ) |>
      dplyr::ungroup()
  } else {
    resultado <- .datos_df |>
      dplyr::arrange(!!rlang::sym(.col_fecha)) |>
      dplyr::mutate(
        !!col_acumulado := dplyr::case_when(
          !!rlang::sym(.col_fecha) %in% primeras_fechas ~ NA_real_,
          TRUE ~ slider::slide_dbl(!!rlang::sym(.col_valores),
                                   sum, .before = periodos_atras - 1, .complete = TRUE)
        )
      )
  }

  # Convertir de vuelta si era duckplyr
  if (.use_duckplyr && inherits(resultado, "duckplyr_df")) {
    resultado <- dplyr::collect(resultado)
  }

  return(resultado)
}

# Acumulado desde 1 de enero----
#' Calcula el valor acumulado desde el 1 de enero de cada año. Devuelve el dataframe original con una columna adicional llamada `acumulado_desde_1_enero`.
#' @param .datos_df Data frame que contiene los datos sobre los que se quiere calcular el acumulado.
#' @param .frecuencia Cadena de caracteres. Indica la frecuencia de los datos. Permite `"anual"`, `"semestral"`, `"trimestral"`, `"mensual"`, `"semanal"` y `"diaria"`. Por defecto, `"mensual"`.
#' @param .col_valores Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores sobre los que se quiere calcular el acumulado. Por defecto, `"valores"`.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres sobre los que se quiere calcular el acumulado. Por ejemplo, si se quiere calcular el acumulado para cada provincia o para cada flujo se podrá especificar aquí. Admite varias columnas (p.ej. `c("flujo", "pais")`). Por defecto, `"nombres"`. En su ausencia señalar `.col_nombres = NULL`.
#' @param .col_flujo Cadena de caracteres. Establece el nombre de la columna que determina el sentido del flujo. Si no existiese esta columna y todas las observaciones reflejasen el mismo tipo de flujo permite establecer `.col_flujo = NULL`. Nótese que también se puede establecer la columna flujo en `.col_nombres` con el mismo efecto. Por defecto, `NULL`.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas (en formato fecha). Se entiende que habrá un dato mensual. Por defecto, `"fecha"`.
#' @param .completar_df Valor lógico. Establecer en `TRUE` si se desea que se complete el dataframe para las observaciones faltantes con valor cero. Por defecto, `FALSE`.
#' @export
get_acumulado_desde_1_enero <- function(.datos_df,
                                        .frecuencia = "mensual",
                                        .col_valores = "valores",
                                        .col_nombres = "nombres",
                                        .col_flujo = NULL,
                                        .col_fecha = "fecha",
                                        .completar_df = FALSE) {


  stopifnot(
    "El input debe ser un data frame." = is.data.frame(.datos_df),
    "No existe la columna señalada para la fecha." = .col_fecha %in% names(.datos_df),
    "No existe la columna señalada para los valores." = .col_valores %in% names(.datos_df)
  )

  if (!inherits(.datos_df[[.col_fecha]], "Date")) {
    warning("Se ha convertido la columna de fechas a formato fecha.")
    .datos_df[[.col_fecha]] <- as.Date(.datos_df[[.col_fecha]])
  }

  .datos_df <-
    .datos_df[!is.na(.datos_df[[.col_fecha]]), ]

  min_date <- min(.datos_df[[.col_fecha]], na.rm = TRUE)
  max_date <- max(.datos_df[[.col_fecha]], na.rm = TRUE)

  if (is.infinite(min_date) || is.infinite(max_date)) {
    stop("Las fechas no tienen un formato válido.")
  }

  if (.completar_df == TRUE){
    if (.frecuencia == "anual"){
      variable_secuencia <- "year"
    } else if (.frecuencia == "semestral"){
      variable_secuencia <- "6 months"
    } else if (.frecuencia == "trimestral"){
      variable_secuencia <- "quarter"
    } else if (.frecuencia == "mensual"){
      variable_secuencia <- "month"
    } else if (.frecuencia == "semanal"){
      variable_secuencia <- "week"
    } else if (.frecuencia == "diaria"){
      variable_secuencia <- "day"
    }

    date_seq <- seq.Date(from = min_date,
                         to = max_date,
                         by = variable_secuencia)

    if (is.null(.col_nombres) & is.null(.col_flujo)) {
      .datos_df <-
        .datos_df |>
        tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
        tidyr::replace_na(stats::setNames(list(0), .col_valores))
    } else if (!is.null(.col_nombres) & is.null(.col_flujo)) {
      .datos_df <-
        .datos_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres))) |>
        tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
        tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
        dplyr::ungroup()
    } else if (is.null(.col_nombres) & !is.null(.col_flujo)) {
      .datos_df <-
        .datos_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo))) |>
        tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
        tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
        dplyr::ungroup()
    } else if (!is.null(.col_nombres) & !is.null(.col_flujo)) {
      .datos_df <-
        .datos_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo))) |>
        tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
        tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
        dplyr::ungroup()
    }
  }

  col_acumulado <- "acumulado_desde_1_enero"

  if (is.null(.col_nombres) & is.null(.col_flujo)) {
    resultado <-
      .datos_df |>
      dplyr::group_by(año = lubridate::year(.data[[.col_fecha]])) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_acumulado := cumsum(!!rlang::sym(.col_valores))) |>
      dplyr::ungroup()
  } else if (!is.null(.col_nombres) & is.null(.col_flujo)) {
    resultado <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)), año = lubridate::year(.data[[.col_fecha]])) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_acumulado := cumsum(!!rlang::sym(.col_valores))) |>
      dplyr::ungroup()
  } else if (is.null(.col_nombres) & !is.null(.col_flujo)) {
    resultado <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo)), año = lubridate::year(.data[[.col_fecha]])) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_acumulado := cumsum(!!rlang::sym(.col_valores))) |>
      dplyr::ungroup()
  } else if (!is.null(.col_nombres) & !is.null(.col_flujo)) {
    resultado <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo)), año = lubridate::year(.data[[.col_fecha]])) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(!!col_acumulado := cumsum(!!rlang::sym(.col_valores))) |>
      dplyr::ungroup()
  }

  return(resultado)
}

# Contribución ----
#' Calcula la contribución al crecimiento en los periodos especificados. Devuelve el dataframe original con una columna adicional llamada `contribucion_(.periodos_atras)_(periodos)` que contiene el cálculo teniendo en cuenta la tasa de variación y el peso.
#' @param .datos_df Data frame que contiene los datos sobre los que se quiere calcular la contribución.
#' @param .periodos_atras Número. Número de periodos sobre el que se quiera calcular el acumulado. Por defecto, 1 año (en el caso de datos semanales se aproximará con 52 semanas, y en el caso de datos diarios con 365 días sin tener en cuenta años bisiestos).
#' @param .frecuencia Cadena de caracteres. Indica la frecuencia de los datos. Permite "anual", "semestral", "trimestral", "mensual", "semanal" y "diaria". Por defecto, "mensual".
#' @param .col_valores Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores sobre los que se quiere calcular la contribución. Por defecto, "valores".
#' @param .col_nombres Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres sobre los que se quiere calcular la contribución. Por ejemplo, si se quiere calcular la contribución de un sector a las exportaciones se debería especificar el nombre de la columna que especifique el sector. Admite varias columnas (p.ej. `c("flujo", "pais")`). Por defecto, `"nombres"`. En su ausencia señalar `.col_nombres = NULL`.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas (en formato fecha). Se entiende que habrá un dato mensual. Por defecto, "fecha".
#' @param .col_flujo Cadena de caracteres. Establece el nombre de la columna que determina el sentido del flujo. Si no existiese esta columna y todas las observaciones reflejan el mismo tipo de flujo permite establecer `.col_flujo = NULL`. Por defecto, `.col_flujo = "flujo"`.
#' @param .col_filtro Opcional. Cadena de caracteres. Establece la columna (adicional) que incorpore datos sobre los que se quiera establecer el filtro indicado por el parámetro `.filtro`.
#' @param .filtro Opcional. Establece el valor al que se tiene que igualar la columna `.col_filtro` para que la observación no sea descartada de inicio (p.ej. esto sirve para calcular la contribución de un sector a las exportaciones de EEUU).
#' @param .en_tanto_por_uno Valor lógico. Si se establece en `TRUE` devuelve los datos en tanto por uno y en caso contrario en tanto por cien. Por defecto, en tanto por cien.
#' @export
get_contribucion <- function(.datos_df,
                             .periodos_atras = NULL,
                             .frecuencia = "mensual",
                             .col_valores = "valores",
                             .col_nombres = "nombres",
                             .col_fecha = "fecha",
                             .col_flujo = "flujo",
                             .col_filtro = NULL,
                             .filtro = NULL,
                             .en_tanto_por_uno = FALSE) {
  stopifnot(
    "El input debe ser un data frame." = is.data.frame(.datos_df),
    "No existe la columna señalada para la fecha." = .col_fecha %in% names(.datos_df),
    "No existe la columna señalada para los valores." = .col_valores %in% names(.datos_df)
  )

  if (!is.null(.col_filtro)) {
    stopifnot("Las columnas especificadas en .col_filtro no existen." = all(.col_filtro %in% names(.datos_df)))
  }

  if (!is.null(.col_filtro)) {
    .datos_df <-
      .datos_df |>
      dplyr::filter(!!rlang::sym(.col_filtro) == .filtro)
  }

  if (!inherits(.datos_df[[.col_fecha]], "Date")) {
    warning("Se ha convertido la columna de fechas a formato fecha.")
    .datos_df[[.col_fecha]] <- as.Date(.datos_df[[.col_fecha]])
  }

  if (!is.null(.periodos_atras)) {
    periodos_atras <- .periodos_atras
  } else if (.frecuencia == "anual") {
    periodos_atras <- 1
  } else if (.frecuencia == "semestral") {
    periodos_atras <- 2
  } else if (.frecuencia == "trimestral") {
    periodos_atras <- 4
  } else if (.frecuencia == "mensual") {
    periodos_atras <- 12
  } else if (.frecuencia == "semanal") {
    periodos_atras <- 52
  } else if (.frecuencia == "diaria") {
    periodos_atras <- 365
  }

  if (.frecuencia == "anual") {
    variable_secuencia <- "year"
    periodo <- "años"
  } else if (.frecuencia == "semestral") {
    variable_secuencia <- "6 months"
    periodo <- "semestres"
  } else if (.frecuencia == "trimestral") {
    variable_secuencia <- "quarter"
    periodo <- "trimestres"
  } else if (.frecuencia == "mensual") {
    variable_secuencia <- "month"
    periodo <- "meses"
  } else if (.frecuencia == "semanal") {
    variable_secuencia <- "week"
    periodo <- "semanas"
  } else if (.frecuencia == "diaria") {
    variable_secuencia <- "day"
    periodo <- "dias"
  }

  .datos_df <-
    .datos_df[!is.na(.datos_df[[.col_fecha]]), ]

  min_date <- min(.datos_df[[.col_fecha]], na.rm = TRUE)
  max_date <- max(.datos_df[[.col_fecha]], na.rm = TRUE)

  if (is.infinite(min_date) || is.infinite(max_date)) {
    stop("Las fechas no tienen un formato válido.")
  }

  date_seq <- seq.Date(from = min_date, to = max_date, by = variable_secuencia)

  if (is.null(.col_nombres) & is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(stats::setNames(list(0), .col_valores))
  } else if (!is.null(.col_nombres) & is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  } else if  (is.null(.col_nombres) & !is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  } else if  (!is.null(.col_nombres) & !is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)),
                      dplyr::across(dplyr::all_of(.col_flujo))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  }

  col_contribucion <- paste0("contribucion_", periodos_atras, "_", periodo)
  primeras_fechas <- sort(unique(.datos_df[[.col_fecha]]))[1:(periodos_atras - 1)]
  # dato_anterior <- tail(.datos_df$.col_valores, periodos_atras + 1)[1]

  if (is.null(.col_nombres) & is.null(.col_flujo)) {
    # Calculamos dato_anterior para todo el dataset
    dato_anterior_total <- .datos_df[[.col_valores]][periodos_atras + 1]

    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::summarise(!!rlang::sym(.col_valores) := sum(!!rlang::sym(.col_valores), na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::mutate(
        !!rlang::sym(col_contribucion) :=
          (!!rlang::sym(.col_valores) - dplyr::lag(!!rlang::sym(.col_valores), periodos_atras)) /
          dato_anterior_total) |>
      dplyr::mutate(
        !!rlang::sym(col_contribucion) := dplyr::case_when(
          .data[[.col_fecha]] %in% primeras_fechas ~ NA_real_,
          TRUE ~ !!rlang::sym(col_contribucion)
        )
      )
  } else if (!is.null(.col_nombres) & is.null(.col_flujo)) {
    # Calcular totales por fecha
    totales_por_fecha <- .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::summarise(total_fecha = sum(!!rlang::sym(.col_valores), na.rm = TRUE)) |>
      dplyr::ungroup()

    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha)),
                      dplyr::across(dplyr::all_of(.col_nombres))) |>
      dplyr::summarise(!!rlang::sym(.col_valores) := sum(!!rlang::sym(.col_valores), na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::left_join(totales_por_fecha, by = .col_fecha) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha)),
                     dplyr::across(dplyr::all_of(.col_nombres))) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres))) |>
      dplyr::mutate(
        total_fecha_anterior = dplyr::lag(total_fecha, periodos_atras),
        !!rlang::sym(col_contribucion) :=
          (!!rlang::sym(.col_valores) - dplyr::lag(!!rlang::sym(.col_valores), periodos_atras)) /
          total_fecha_anterior) |>
      dplyr::select(-total_fecha, -total_fecha_anterior) |>
      dplyr::mutate(
        !!rlang::sym(col_contribucion) := dplyr::case_when(
          .data[[.col_fecha]] %in% primeras_fechas ~ NA_real_,
          TRUE ~ !!rlang::sym(col_contribucion)
        )
      ) |>
      dplyr::ungroup()
  } else if (is.null(.col_nombres) & !is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha)),
                      dplyr::across(dplyr::all_of(.col_flujo))) |>
      dplyr::summarise(!!rlang::sym(.col_valores) := sum(!!rlang::sym(.col_valores), na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_flujo)),
                     dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha)),
                      dplyr::across(dplyr::all_of(.col_flujo))) |>
      dplyr::mutate(total = sum(!!rlang::sym(.col_valores), na.rm = TRUE)) |>
      dplyr::mutate(!!rlang::sym(col_contribucion) :=
                      (!!rlang::sym(.col_valores) - dplyr::lag(!!rlang::sym(.col_valores), periodos_atras)) /
                      dplyr::lag(total, n = periodos_atras)) |>
      dplyr::ungroup() |>
      dplyr::select(-total)
  } else if (!is.null(.col_nombres) & !is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha)),
                      dplyr::across(dplyr::all_of(.col_nombres)),
                      dplyr::across(dplyr::all_of(.col_flujo))) |>
      dplyr::summarise(!!rlang::sym(.col_valores) := sum(!!rlang::sym(.col_valores), na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_flujo)),
                     dplyr::across(dplyr::all_of(.col_fecha)),
                     dplyr::across(dplyr::all_of(.col_nombres))) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha)),
                      dplyr::across(dplyr::all_of(.col_flujo))) |>
      dplyr::mutate(total = sum(!!rlang::sym(.col_valores), na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo)),
                      dplyr::across(dplyr::all_of(.col_nombres))) |>
      dplyr::mutate(!!rlang::sym(col_contribucion) :=
                      (!!rlang::sym(.col_valores) - dplyr::lag(!!rlang::sym(.col_valores), periodos_atras)) /
                      dplyr::lag(total, n = periodos_atras)) |>
      dplyr::ungroup() |>
      dplyr::select(-total)
  }

  if(!.en_tanto_por_uno){
    .datos_df <-
      .datos_df |>
      dplyr::mutate(!!rlang::sym(col_contribucion) := !!rlang::sym(col_contribucion) * 100)
  }

  return(.datos_df)
}

# Peso ----
#' Calcula el peso sobre el total del flujo. Devuelve un dataframe como el original, pero con una columna adicional: `peso`, que hace referencia al peso del flujo sobre el total de `col.nombres` en la fecha indicada.
#' @param .datos_df Data frame que contiene los datos.
#' @param .col_valores Cadena de caracteres. Nombre de la columna con valores. Por defecto, `"valores"`.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna con nombres. Por defecto, `"nombres"`.
#' @param .col_flujo Cadena de caracteres. Nombre de la columna de flujo. Si es NULL, se ignora. Por defecto, `"flujo"`.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna de fecha. Por defecto, `"fecha"`.
#' @param .en_tanto_por_uno Valor lógico. Si se establece en `TRUE` devuelve los datos en tanto por uno y en caso contrario en tanto por cien. Por defecto, en tanto por cien.
#' @export
get_peso <- function(.datos_df,
                     .col_valores = "valores",
                     .col_nombres = "nombres",
                     .col_flujo = "flujo",
                     .col_fecha = "fecha",
                     .en_tanto_por_uno = FALSE) {

  # Verificar que las columnas existen
  cols_required <- c(.col_fecha, .col_valores)
  if (!is.null(.col_nombres)) cols_required <- c(cols_required, .col_nombres)
  if (!is.null(.col_flujo)) cols_required <- c(cols_required, .col_flujo)

  cols_missing <- setdiff(cols_required, names(.datos_df))
  if (length(cols_missing) > 0) {
    stop("Columnas faltantes: ", paste(cols_missing, collapse = ", "))
  }

  # Crear columnas de agrupación dinámicamente
  group_cols <- .col_fecha
  if (!is.null(.col_flujo)) group_cols <- c(group_cols, .col_flujo)
  if (!is.null(.col_nombres)) group_cols <- c(group_cols, .col_nombres)

  # Calcular totales por fecha (y flujo si existe)
  if (!is.null(.col_flujo)) {
    totales <- .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(.col_fecha, .col_flujo)))) |>
      dplyr::summarise(
        total_fecha_flujo = sum(!!rlang::sym(.col_valores), na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    totales <- .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha))) |>
      dplyr::summarise(
        total_fecha = sum(!!rlang::sym(.col_valores), na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Agregar datos por grupos y calcular pesos
  if (!is.null(.col_nombres)) {
    # Caso con nombres: agregar por fecha, flujo (si existe) y nombres
    resultado <- .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(
        !!rlang::sym(.col_valores) := sum(!!rlang::sym(.col_valores), na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    # Caso sin nombres: agregar solo por fecha y flujo (si existe)
    if (!is.null(.col_flujo)) {
      resultado <- .datos_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c(.col_fecha, .col_flujo)))) |>
        dplyr::summarise(
          !!rlang::sym(.col_valores) := sum(!!rlang::sym(.col_valores), na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      resultado <- .datos_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha))) |>
        dplyr::summarise(
          !!rlang::sym(.col_valores) := sum(!!rlang::sym(.col_valores), na.rm = TRUE),
          .groups = "drop"
        )
    }
  }

  # Unir con totales y calcular peso
  if (!is.null(.col_flujo)) {
    resultado <- resultado |>
      dplyr::left_join(totales, by = c(.col_fecha, .col_flujo)) |>
      dplyr::mutate(peso = !!rlang::sym(.col_valores) / total_fecha_flujo) |>
      dplyr::select(-total_fecha_flujo)
  } else {
    resultado <- resultado |>
      dplyr::left_join(totales, by = .col_fecha) |>
      dplyr::mutate(peso = !!rlang::sym(.col_valores) / total_fecha) |>
      dplyr::select(-total_fecha)
  }

  # Ordenar resultado
  resultado <- resultado |>
    dplyr::arrange(dplyr::across(dplyr::all_of(group_cols)))


  if(!.en_tanto_por_uno){
    resultado <- resultado |>
      dplyr::mutate(peso = peso * 100)
  }

  return(resultado)
}

# Media móvil ----
#' Calcula la media móvil. Devuelve el dataframe original con una columna adicional `mm_(.periodos_atras)_(periodo)` que hace referencia a la media móvil a `.periodos_atras` periodos.
#' @param .datos_df Data frame que contiene los datos sobre los que se quiere calcular la media móvil.
#' @param .periodos_atras Número. Número de periodos sobre el que se quiera calcular la tasa de variación. Por defecto, 1 año (en el caso de datos semanales se aproximará con 52 semanas, y en el caso de datos diarios con 365 días sin tener en cuenta años bisiestos).
#' @param .frecuencia Cadena de caracteres. Indica la frecuencia de los datos. Permite `"anual"`, `"semestral"`, `"trimestral"`, `"mensual"`, `"semanal"` y `"diaria"`. Por defecto, `"mensual"`.
#' @param .col_valores Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores. Por defecto, "valores".
#' @param .col_nombres Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres para realizar la agrupación. Permite introducir el nombre de varias columnas. Por ejemplo, si se quiere calcular la media móvil de las exportaciones para cada provincia sobre el total se podrá especificar aquí `.col_nombres = ("flujo", "provincia")`. Por defecto, `"nombres"`.
#' @param .col_flujo Cadena de caracteres. Establece el nombre de la columna que determina el sentido del flujo. Si no existiese esta columna y todas las observaciones reflejasen el mismo tipo de flujo permite establecer `.col_flujo = NULL`. Nótese que también se puede establecer la columna flujo en `.col_nombres` con el mismo efecto. Por defecto, `NULL`.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas (en formato fecha). Por defecto, `"fecha"`.
#' @export
get_media_movil <- function(.datos_df,
                            .periodos_atras = NULL,
                            .frecuencia = "mensual",
                            .col_valores = "valores",
                            .col_nombres = "nombres",
                            .col_flujo = NULL,
                            .col_fecha = "fecha") {
  stopifnot(
    "El input debe ser un data frame." = is.data.frame(.datos_df),
    "No existe la columna señalada para la fecha." = .col_fecha %in% names(.datos_df),
    "No existe la columna señalada para los valores." = .col_valores %in% names(.datos_df)
  )


  if (!inherits(.datos_df[[.col_fecha]], "Date")) {
    warning("Se ha convertido la columna de fechas a formato fecha.")
    .datos_df[[.col_fecha]] <- as.Date(.datos_df[[.col_fecha]])
  }

  if(!is.null(.periodos_atras)){
    periodos_atras <- .periodos_atras
  } else if (.frecuencia == "anual"){
    periodos_atras <- 1
  } else if (.frecuencia == "semestral"){
    periodos_atras <- 2
  } else if (.frecuencia == "trimestral"){
    periodos_atras <- 4
  } else if (.frecuencia == "mensual"){
    periodos_atras <- 12
  } else if (.frecuencia == "semanal"){
    periodos_atras <- 52
  } else if (.frecuencia == "diaria"){
    periodos_atras <- 365
  }

  if (.frecuencia == "anual"){
    variable_secuencia <- "year"
    periodo <- "años"
  } else if (.frecuencia == "semestral"){
    variable_secuencia <- "6 months"
    periodo <- "semestres"
  } else if (.frecuencia == "trimestral"){
    variable_secuencia <- "quarter"
    periodo <- "trimestres"
  } else if (.frecuencia == "mensual"){
    variable_secuencia <- "month"
    periodo <- "meses"
  } else if (.frecuencia == "semanal"){
    variable_secuencia <- "week"
    periodo <- "semanas"
  } else if (.frecuencia == "diaria"){
    variable_secuencia <- "day"
    periodo <- "dias"
  }

  .datos_df <-
    .datos_df[!is.na(.datos_df[[.col_fecha]]),]

  min_date <- min(.datos_df[[.col_fecha]], na.rm = TRUE)
  max_date <- max(.datos_df[[.col_fecha]], na.rm = TRUE)

  if (is.infinite(min_date) || is.infinite(max_date)) {
    stop("Las fechas no tienen un formato válido.")
  }

  date_seq <- seq.Date(from = min_date,
                       to = max_date,
                       by = variable_secuencia)

  .datos_df <-
    .datos_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo))) |>
    tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
    tidyr::replace_na(stats::setNames(list(0), .col_valores)) |>
    dplyr::ungroup()

  col_media_movil <- paste0("mm_", periodos_atras, "_",  periodo)
  primeras_fechas <- sort(unique(.datos_df[[.col_fecha]]))[1:(periodos_atras - 1)]

  resultado <-
    .datos_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha)), dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo))) |>
    dplyr::summarise(
      !!.col_valores := sum(!!rlang::sym(.col_valores), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha)), dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo))) |>
    dplyr::mutate(
      !!col_media_movil := zoo::rollmean(!!rlang::sym(.col_valores), periodos_atras, fill = NA, align = "right")
    ) |>
    dplyr::mutate(
      !!col_media_movil := dplyr::case_when(
        .data[[.col_fecha]] %in% primeras_fechas ~ NA_real_,
        TRUE ~ !!rlang::sym(col_media_movil)
      )
    ) |>
    dplyr::ungroup()

  return(resultado)
}

# Contribución acumulada ----
#' Calcula la contribución de cada grupo al crecimiento del acumulado entre la última fecha y `.años_atras_contribucion` años antes: la diferencia del acumulado de cada grupo dividida por el acumulado total inicial (del mismo flujo). Devuelve un dataframe con el flujo y los nombres (si los hay), `fecha_inicial`, `fecha_final`, `acumulado_inicial`, `acumulado_final` y `contribucion`, ordenado de mayor a menor contribución.
#' @param .datos_df Data frame que contiene los datos sobre los que se quiere calcular la contribución. Es necesario que comprenda únicamente exportaciones o únicamente importaciones. Preferiblemente incluirá 3 columnas `fecha`, `nombres` y `valores`, y la columna `nombres` incluirá datos de la variable sobre la que se quiere calcular la contribución y al nivel de desagregación deseado (p.ej. si se quiere obtener la contribución por sectores nivel 1, `datos_df` incluirá el nombre de los sectores nivel 1 en la columna `nombres`).
#' @param .años_atras_contribucion Número. Número de años sobre el que se quiera calcular la contribución al crecimiento. Por defecto, 3 años.
#' @param .periodos_atras_acumulado Número. Número de periodos sobre el que se quiera calcular el acumulado sobre el que establecer la tasa de variación. Por defecto, 1 año (en el caso de datos semanales se aproximará con 52 semanas, y en el caso de datos diarios con 365 días sin tener en cuenta años bisiestos).
#' @param .frecuencia Cadena de caracteres. Indica la frecuencia de los datos. Permite `"anual"`, `"semestral"`, `"trimestral"`, `"mensual"`, `"semanal"`` y `"diaria"`. Por defecto, `"mensual"`.
#' @param .col_valores_contr_acum Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores sobre los que se quiere calcular el acumulado. Por defecto, `"valores"`.
#' @param .col_nombres_contr_acum Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres sobre los que se quiere calcular el acumulado. Por ejemplo, si se quiere calcular el acumulado para cada provincia o para cada flujo se podrá especificar aquí. Admite varias columnas (p.ej. `c("flujo", "pais")`). Por defecto, `"nombres"`.
#' @param .col_fecha_contr_acum Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas (en formato fecha). Se entiende que habrá un dato mensual. Por defecto, "fecha".
#' @param .col_flujo Cadena de caracteres. Establece el nombre de la columna que determina el sentido del flujo. Si no se existe esta columna y todas las observaciones reflejan el mismo tipo de flujo permite establecer `.col_flujo = NULL`. Por defecto, `"flujo"`.
#' @param .col_filtro Opcional. Cadena de caracteres. Establece la columna (adicional) que incorpore datos sobre los que se quiera establecer el filtro indicado por el parámetro `.filtro`.
#' @param .filtro Opcional. Establece el valor al que se tiene que igualar la columna `.col_filtro` para que la observación no sea descartada de inicio (p.ej. esto sirve para calcular la contribución de un sector a las exportaciones de EEUU).
#' @param .en_tanto_por_uno Valor lógico. Si se establece en `TRUE` devuelve los datos en tanto por uno y en caso contrario en tanto por cien. Por defecto, en tanto por cien.
#' @export
get_contribucion_acumulada <- function(.datos_df,
                                       .años_atras_contribucion = 3,
                                       .periodos_atras_acumulado = NULL,
                                       .frecuencia = "mensual",
                                       .col_valores_contr_acum = "valores",
                                       .col_nombres_contr_acum = "nombres",
                                       .col_fecha_contr_acum = "fecha",
                                       .col_flujo = "flujo",
                                       .col_filtro = NULL,
                                       .filtro = NULL,
                                       .en_tanto_por_uno = FALSE) {
  stopifnot(
    "El input debe ser un data frame." = is.data.frame(.datos_df),
    "No existe la columna señalada para la fecha." = .col_fecha_contr_acum %in% names(.datos_df),
    "No existe la columna señalada para los valores." = .col_valores_contr_acum %in% names(.datos_df),
    "`.frecuencia` no reconocida." = .frecuencia %in% c("anual", "semestral", "trimestral", "mensual", "semanal", "diaria")
  )

  if (!is.null(.col_filtro)) {
    stopifnot("Las columnas especificadas en .col_filtro no existen." = all(.col_filtro %in% names(.datos_df)))
    .datos_df <- .datos_df |>
      dplyr::filter(!!rlang::sym(.col_filtro) == .filtro)
  }

  if (!inherits(.datos_df[[.col_fecha_contr_acum]], "Date")) {
    warning("Se ha convertido la columna de fechas a formato fecha.")
    .datos_df[[.col_fecha_contr_acum]] <- as.Date(.datos_df[[.col_fecha_contr_acum]])
  }

  periodos_por_frecuencia <- c(anual = 1, semestral = 2, trimestral = 4, mensual = 12, semanal = 52, diaria = 365)
  nombre_periodo <- c(anual = "años", semestral = "semestres", trimestral = "trimestres",
                      mensual = "meses", semanal = "semanas", diaria = "dias")
  secuencia <- c(anual = "year", semestral = "6 months", trimestral = "quarter", mensual = "month",
                 semanal = "week", diaria = "day")

  periodos_atras_acumulado <- if (!is.null(.periodos_atras_acumulado)) .periodos_atras_acumulado else periodos_por_frecuencia[[.frecuencia]]

  .datos_df <- .datos_df[!is.na(.datos_df[[.col_fecha_contr_acum]]), ]
  min_date <- min(.datos_df[[.col_fecha_contr_acum]], na.rm = TRUE)
  max_date <- max(.datos_df[[.col_fecha_contr_acum]], na.rm = TRUE)
  if (is.infinite(min_date) || is.infinite(max_date)) stop("Las fechas no tienen un formato válido.")

  cols_grupo <- c(.col_flujo, .col_nombres_contr_acum)
  date_seq <- seq.Date(from = min_date, to = max_date, by = secuencia[[.frecuencia]])

  # Serie completa y agregada por fecha y grupos
  datos_df <- .datos_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(.col_fecha_contr_acum, cols_grupo)))) |>
    dplyr::summarise(!!rlang::sym(.col_valores_contr_acum) := sum(!!rlang::sym(.col_valores_contr_acum), na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(cols_grupo))) |>
    tidyr::complete(!!rlang::sym(.col_fecha_contr_acum) := date_seq) |>
    tidyr::replace_na(stats::setNames(list(0), .col_valores_contr_acum)) |>
    dplyr::ungroup()

  col_acumulado <- paste0("acumulado_", periodos_atras_acumulado, "_", nombre_periodo[[.frecuencia]])

  acumulados_df <- datos_df |>
    get_acumulado(.periodos_atras = periodos_atras_acumulado,
                  .frecuencia = .frecuencia,
                  .col_valores = .col_valores_contr_acum,
                  .col_nombres = .col_nombres_contr_acum,
                  .col_fecha = .col_fecha_contr_acum,
                  .col_flujo = .col_flujo) |>
    dplyr::rename(acumulado = !!rlang::sym(col_acumulado))

  fecha_final <- max_date
  fecha_inicial <- fecha_final - lubridate::years(.años_atras_contribucion)
  if (!fecha_inicial %in% acumulados_df[[.col_fecha_contr_acum]]) {
    stop("No hay observaciones en la fecha inicial (", fecha_inicial, "): no se puede calcular la contribución.")
  }

  escala <- if (isTRUE(.en_tanto_por_uno)) 1 else 100

  acumulados_df |>
    dplyr::filter(!!rlang::sym(.col_fecha_contr_acum) %in% c(fecha_inicial, fecha_final)) |>
    dplyr::mutate(momento = ifelse(!!rlang::sym(.col_fecha_contr_acum) == fecha_final, "acumulado_final", "acumulado_inicial")) |>
    dplyr::select(dplyr::all_of(cols_grupo), momento, acumulado) |>
    tidyr::pivot_wider(names_from = momento, values_from = acumulado) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo))) |>
    dplyr::mutate(
      fecha_inicial = fecha_inicial,
      fecha_final = fecha_final,
      contribucion = escala * (acumulado_final - acumulado_inicial) / sum(acumulado_inicial, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::across(dplyr::all_of(.col_flujo)), dplyr::desc(contribucion)) |>
    dplyr::relocate(fecha_inicial, fecha_final, .before = acumulado_inicial)
}

# Agregado por periodos ----
#' Agrega los datos en bloques de `.periodos` observaciones consecutivas, devolviendo una sola observación por bloque. Permite, por ejemplo, convertir datos mensuales en trimestrales (`.periodos = 3`), semestrales (`.periodos = 6`) o anuales (`.periodos = 12`). El bloque parcial final (con menos de `.periodos` observaciones) se descarta.
#' @param .datos_df Data frame que contiene los datos sobre los que se quiere realizar la agregación.
#' @param .periodos Número entero. Número de observaciones consecutivas que componen cada bloque. Por defecto, `3` (conversión de datos mensuales a trimestrales).
#' @param .transformacion Cadena de caracteres o función. Transformación a aplicar dentro de cada bloque. Admite las cadenas `"suma"`, `"media"`, `"mediana"`, `"max"` y `"min"`, o cualquier función que reciba un vector numérico y devuelva un escalar (p. ej. `mean`, `sd`, `\(x) quantile(x, 0.9)`). Por defecto, `"suma"`.
#' @param .alineacion Cadena de caracteres. Determina la fecha asignada al bloque agregado y, en su caso, cómo se forman los bloques. `"ultima"` (por defecto) agrupa observaciones consecutivas desde el inicio de la serie de cada grupo y asigna como fecha la del último periodo del bloque. `"natural"` alinea los bloques al calendario (p. ej. trimestres ene-mar, abr-jun, jul-sep, oct-dic) en función de `.frecuencia`, asignando también la fecha del último periodo del bloque.
#' @param .frecuencia Cadena de caracteres. Frecuencia de los datos de entrada. Solo se utiliza cuando `.alineacion = "natural"` para determinar las fronteras del calendario. Permite `"anual"`, `"semestral"`, `"trimestral"`, `"mensual"`, `"semanal"` y `"diaria"`. Por defecto, `"mensual"`.
#' @param .col_valores Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores sobre los que se quiere realizar la agregación. Por defecto, `"valores"`. La columna devuelta conserva este mismo nombre.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres sobre los que se quiere realizar la agregación por separado (p. ej. para cada provincia). Admite varias columnas (p. ej. `c("flujo", "pais")`). Por defecto, `"nombres"`. En su ausencia señalar `.col_nombres = NULL`.
#' @param .col_flujo Cadena de caracteres. Establece el nombre de la columna que determina el sentido del flujo. Si no existiese esta columna y todas las observaciones reflejasen el mismo tipo de flujo permite establecer `.col_flujo = NULL`. Nótese que también se puede establecer la columna flujo en `.col_nombres` con el mismo efecto. Por defecto, `NULL`.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna del dataframe que contiene las fechas (en formato fecha). Por defecto, `"fecha"`.
#' @export
get_agregado <- function(.datos_df,
                         .periodos = 3,
                         .transformacion = "suma",
                         .alineacion = "ultima",
                         .frecuencia = "mensual",
                         .col_valores = "valores",
                         .col_nombres = "nombres",
                         .col_flujo = NULL,
                         .col_fecha = "fecha") {
  stopifnot(
    "El input debe ser un data frame." = is.data.frame(.datos_df),
    "No existe la columna señalada para la fecha." = .col_fecha %in% names(.datos_df),
    "No existe la columna señalada para los valores." = .col_valores %in% names(.datos_df),
    "`.periodos` debe ser un entero positivo." = is.numeric(.periodos) && length(.periodos) == 1 && .periodos >= 1,
    "`.alineacion` debe ser \"ultima\" o \"natural\"." = .alineacion %in% c("ultima", "natural")
  )

  .periodos <- as.integer(.periodos)

  # Resolver la transformación a una función escalar
  if (is.character(.transformacion)) {
    fun_agregar <- switch(
      .transformacion,
      "suma"     = function(x) sum(x, na.rm = TRUE),
      "media"    = function(x) mean(x, na.rm = TRUE),
      "mediana"  = function(x) stats::median(x, na.rm = TRUE),
      "max"      = function(x) max(x, na.rm = TRUE),
      "min"      = function(x) min(x, na.rm = TRUE),
      stop("Transformación no reconocida. Use \"suma\", \"media\", \"mediana\", \"max\", \"min\" o proporcione una función.")
    )
  } else if (is.function(.transformacion)) {
    fun_agregar <- .transformacion
  } else {
    stop("`.transformacion` debe ser una cadena de caracteres o una función.")
  }

  if (!inherits(.datos_df[[.col_fecha]], "Date")) {
    warning("Se ha convertido la columna de fechas a formato fecha.")
    .datos_df[[.col_fecha]] <- as.Date(.datos_df[[.col_fecha]])
  }

  .datos_df <- .datos_df[!is.na(.datos_df[[.col_fecha]]), ]

  # Columnas de agrupación (excluyendo la fecha y los valores)
  cols_grupo <- c(.col_nombres, .col_flujo)
  cols_grupo <- cols_grupo[!is.null(cols_grupo)]

  # Asignar identificador de bloque y fecha del bloque (última observación del bloque)
  if (.alineacion == "ultima") {
    asignar_bloque <- function(df) {
      df <- df |>
        dplyr::arrange(!!rlang::sym(.col_fecha)) |>
        dplyr::mutate(.bloque_id = ((dplyr::row_number() - 1L) %/% .periodos) + 1L)
      df
    }
  } else {
    # Alineación al calendario en función de la frecuencia de entrada
    indice_periodo <- function(fechas) {
      switch(
        .frecuencia,
        "anual"      = as.integer(format(fechas, "%Y")),
        "semestral"  = {
          y <- as.integer(format(fechas, "%Y"))
          m <- as.integer(format(fechas, "%m"))
          y * 2L + ((m - 1L) %/% 6L)
        },
        "trimestral" = {
          y <- as.integer(format(fechas, "%Y"))
          m <- as.integer(format(fechas, "%m"))
          y * 4L + ((m - 1L) %/% 3L)
        },
        "mensual"    = {
          y <- as.integer(format(fechas, "%Y"))
          m <- as.integer(format(fechas, "%m"))
          y * 12L + (m - 1L)
        },
        "semanal"    = as.integer(floor(unclass(fechas) / 7)),
        "diaria"     = as.integer(unclass(fechas)),
        stop("`.frecuencia` no reconocida.")
      )
    }
    asignar_bloque <- function(df) {
      idx <- indice_periodo(df[[.col_fecha]])
      df$.bloque_id <- idx %/% as.integer(.periodos)
      df
    }
  }

  if (length(cols_grupo) > 0) {
    .datos_df <- .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(cols_grupo))) |>
      dplyr::group_modify(~ asignar_bloque(.x)) |>
      dplyr::ungroup()
  } else {
    .datos_df <- asignar_bloque(.datos_df)
  }

  cols_resumen <- c(cols_grupo, ".bloque_id")

  resultado <- .datos_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(cols_resumen))) |>
    dplyr::summarise(
      !!rlang::sym(.col_fecha) := max(!!rlang::sym(.col_fecha), na.rm = TRUE),
      !!rlang::sym(.col_valores) := fun_agregar(!!rlang::sym(.col_valores)),
      .n_obs = dplyr::n(),
      .groups = "drop"
    ) |>
    # Descartar bloques incompletos (parciales)
    dplyr::filter(.n_obs == .periodos) |>
    dplyr::select(-.bloque_id, -.n_obs) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(cols_grupo, .col_fecha))))

  return(resultado)
}
