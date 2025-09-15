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
  }
  else if (suma_import < 0) {
    .datos_df |>
      tidyr::pivot_wider(names_from = tidyselect::all_of(.col_flujo),
                         values_from = .col_valores) |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(.col_nombres))) |>
      dplyr::mutate(!!rlang::sym(.saldo) := !!rlang::sym(.export) + !!rlang::sym(.import)) |>
      dplyr::ungroup() |>
      tidyr::pivot_longer(
        cols = c(tidyselect::all_of(.export), tidyselect::all_of(.import), tidyselect::all_of(.saldo)),
        names_to = .col_flujo,
        values_to = .col_valores
      )
  } else if (suma_import >= 0 & suma_export >= 0){
    .datos_df |>
      tidyr::pivot_wider(names_from = tidyselect::all_of(.col_flujo),
                         values_from = .col_valores) |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(.col_nombres))) |>
      dplyr::mutate(!!rlang::sym(.saldo) := !!rlang::sym(.export) - !!rlang::sym(.import)) |>
      dplyr::ungroup() |>
      tidyr::pivot_longer(
        cols = c(tidyselect::all_of(.export), tidyselect::all_of(.import), tidyselect::all_of(.saldo)),
        names_to = .col_flujo,
        values_to = .col_valores
      )
  } else if (suma_export < 0){
    print("Hay datos de exportaciones negativos. No se ha calculado el saldo.")
    .datos_df
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
  }
  else if (suma_import < 0) {
    .datos_df <-
      .datos_df |>
      tidyr::pivot_wider(names_from = tidyselect::all_of(.col_flujo),
                         values_from = .col_valores) |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(.col_nombres))) |>
      dplyr::mutate(!!rlang::sym(.cobertura) := !!rlang::sym(.export) / !!rlang::sym(.import)) |>
      dplyr::ungroup() |>
      tidyr::pivot_longer(
        cols = c(tidyselect::all_of(.export), tidyselect::all_of(.import), tidyselect::all_of(.cobertura)),
        names_to = .col_flujo,
        values_to = .col_valores
      )
    if(!.en_tanto_por_uno){
      .datos_df <-
        .datos_df |>
        dplyr::mutate(!!rlang::sym(.cobertura) := !!rlang::sym(.cobertura) * 100)
    }
    .datos_df
  } else if (suma_import >= 0 & suma_export >= 0){
    .datos_df <-
      .datos_df |>
      tidyr::pivot_wider(names_from = tidyselect::all_of(.col_flujo),
                         values_from = .col_valores) |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(.col_nombres))) |>
      dplyr::mutate(!!rlang::sym(.cobertura) := !!rlang::sym(.export) / !!rlang::sym(.import)) |>
      dplyr::ungroup() |>
      tidyr::pivot_longer(
        cols = c(tidyselect::all_of(.export), tidyselect::all_of(.import), tidyselect::all_of(.cobertura)),
        names_to = .col_flujo,
        values_to = .col_valores
      )
    if(!.en_tanto_por_uno){
      .datos_df <-
        .datos_df |>
        dplyr::mutate(!!rlang::sym(.cobertura) := !!rlang::sym(.cobertura) * 100)
    }
    .datos_df
  } else if (suma_export < 0){
    print("Hay datos de exportaciones negativos. No se ha calculado la cobertura.")
    .datos_df
  }
}

# Tasa de variación ----
#' Calcula la tasa de variación a los periodos especificados. Devuelve el dataframe original con una columna adicional llamada `tv_(.periodos_atras)_(frecuencia)` que contiene el dato.
#' @param .datos_df Data frame que contiene los datos sobre los que se quiere calcular la tasa de variación.
#' @param .periodos_atras Número. Número de periodos sobre el que se quiera calcular la tasa de variación. Por defecto, 1 año (en el caso de datos semanales se aproximará con 52 semanas, y en el caso de datos diarios con 365 días sin tener en cuenta años bisiestos).
#' @param .frecuencia Cadena de caracteres. Indica la frecuencia de los datos. Permite `"anual"`, `"semestral"`, `"trimestral"`, `"mensual"`, `"semanal"` y `"diaria"`. Por defecto, `"mensual"`.
#' @param .col_valores Cadena de caracteres. Nombre de la columna del dataframe que contiene los valores sobre los que se quiere calcular la tasa de variación. Por defecto, `"valores"`.
#' @param .col_nombres Cadena de caracteres. Nombre de la columna del dataframe que contiene los nombres sobre los que se quiere calcular la tasa de variación. Por ejemplo, si se quiere calcular la tasa de variación para cada provincia o para cada flujo se podrá especificar aquí `.col_nombres = c("provincia", "flujo")`. Admite varias columnas (p.ej. `c("flujo", "pais")`). Por defecto, `"nombres"`. En su ausencia señalar `.col_nombres = NULL`.
#' @param .col_flujo Cadena de caracteres. Establece el nombre de la columna que determina el sentido del flujo. Si no existiese esta columna y todas las observaciones reflejasen el mismo tipo de flujo permite establecer `.col_flujo = NULL`, que es su valor por defecto. Nótese que también se puede establecer la columna flujo en `.col_nombres` con el mismo efecto.
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
      tidyr::replace_na(setNames(list(0), .col_valores))
  } else if (!is.null(.col_nombres) & is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  } else if (is.null(.col_nombres) & !is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  } else if (!is.null(.col_nombres) & !is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(setNames(list(0), .col_valores)) |>
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
#' @param .col_flujo Cadena de caracteres. Establece el nombre de la columna que determina el sentido del flujo. Si no existiese esta columna y todas las observaciones reflejasen el mismo tipo de flujo permite establecer `.col_flujo = NULL`, que es su valor por defecto. Nótese que también se puede establecer la columna flujo en `.col_nombres` con el mismo efecto.
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
        tidyr::replace_na(setNames(list(0), .col_valores))
      } else if (!is.null(.col_nombres) & is.null(.col_flujo)) {
      .datos_df <-
        .datos_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres))) |>
        tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
        tidyr::replace_na(setNames(list(0), .col_valores)) |>
        dplyr::ungroup()
      } else if (is.null(.col_nombres) & !is.null(.col_flujo)) {
        .datos_df <-
          .datos_df |>
          dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo))) |>
          tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
          tidyr::replace_na(setNames(list(0), .col_valores)) |>
          dplyr::ungroup()
      } else if (!is.null(.col_nombres) & !is.null(.col_flujo)) {
        .datos_df <-
          .datos_df |>
          dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo))) |>
          tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
          tidyr::replace_na(setNames(list(0), .col_valores)) |>
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
#' @param .col_flujo Cadena de caracteres. Establece el nombre de la columna que determina el sentido del flujo. Si no existiese esta columna y todas las observaciones reflejasen el mismo tipo de flujo permite establecer `.col_flujo = NULL`, que es su valor por defecto. Nótese que también se puede establecer la columna flujo en `.col_nombres` con el mismo efecto.
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
      tidyr::replace_na(setNames(list(0), .col_valores))
  } else if (!is.null(.col_nombres) & is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  } else if (is.null(.col_nombres) & !is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  } else if (!is.null(.col_nombres) & !is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(setNames(list(0), .col_valores)) |>
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
#' @param .col_flujo Cadena de caracteres. Establece el nombre de la columna que determina el sentido del flujo. Si no existiese esta columna y todas las observaciones reflejasen el mismo tipo de flujo permite establecer `.col_flujo = NULL`, que es su valor por defecto. Nótese que también se puede establecer la columna flujo en `.col_nombres` con el mismo efecto.
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
        tidyr::replace_na(setNames(list(0), .col_valores))
    } else if (!is.null(.col_nombres) & is.null(.col_flujo)) {
      .datos_df <-
        .datos_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres))) |>
        tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
        tidyr::replace_na(setNames(list(0), .col_valores)) |>
        dplyr::ungroup()
    } else if (is.null(.col_nombres) & !is.null(.col_flujo)) {
      .datos_df <-
        .datos_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo))) |>
        tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
        tidyr::replace_na(setNames(list(0), .col_valores)) |>
        dplyr::ungroup()
    } else if (!is.null(.col_nombres) & !is.null(.col_flujo)) {
      .datos_df <-
        .datos_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo))) |>
        tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
        tidyr::replace_na(setNames(list(0), .col_valores)) |>
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
#' @param .col_flujo Cadena de caracteres. Establece el nombre de la columna que determina el sentido del flujo. Si no existiese esta columna y todas las observaciones reflejasen el mismo tipo de flujo permite establecer `.col_flujo = NULL`, que es su valor por defecto. Nótese que también se puede establecer la columna flujo en `.col_nombres` con el mismo efecto.
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
      tidyr::replace_na(setNames(list(0), .col_valores))
  } else if (!is.null(.col_nombres) & is.null(.col_flujo)){
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  } else if (is.null(.col_nombres) & !is.null(.col_flujo)){
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  } else if (!is.null(.col_nombres) & !is.null(.col_flujo)){
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(setNames(list(0), .col_valores)) |>
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
      tidyr::replace_na(setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  } else {
    .datos_df <- .datos_df |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(setNames(list(0), .col_valores))
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
#' @param .col_flujo Cadena de caracteres. Establece el nombre de la columna que determina el sentido del flujo. Si no existiese esta columna y todas las observaciones reflejasen el mismo tipo de flujo permite establecer `.col_flujo = NULL`, que es su valor por defecto. Nótese que también se puede establecer la columna flujo en `.col_nombres` con el mismo efecto.
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
        tidyr::replace_na(setNames(list(0), .col_valores))
    } else if (!is.null(.col_nombres) & is.null(.col_flujo)) {
      .datos_df <-
        .datos_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres))) |>
        tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
        tidyr::replace_na(setNames(list(0), .col_valores)) |>
        dplyr::ungroup()
    } else if (is.null(.col_nombres) & !is.null(.col_flujo)) {
      .datos_df <-
        .datos_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo))) |>
        tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
        tidyr::replace_na(setNames(list(0), .col_valores)) |>
        dplyr::ungroup()
    } else if (!is.null(.col_nombres) & !is.null(.col_flujo)) {
      .datos_df <-
        .datos_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)), dplyr::across(dplyr::all_of(.col_flujo))) |>
        tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
        tidyr::replace_na(setNames(list(0), .col_valores)) |>
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
      tidyr::replace_na(setNames(list(0), .col_valores))
  } else if (!is.null(.col_nombres) & is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  } else if  (is.null(.col_nombres) & !is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(setNames(list(0), .col_valores)) |>
      dplyr::ungroup()
  } else if  (!is.null(.col_nombres) & !is.null(.col_flujo)) {
    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres)),
                      dplyr::across(dplyr::all_of(.col_flujo))) |>
      tidyr::complete(!!rlang::sym(.col_fecha) := date_seq) |>
      tidyr::replace_na(setNames(list(0), .col_valores)) |>
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
    dato_anterior_total <- .datos_df[[.col_valores]][periodos_atras + 1]

    .datos_df <-
      .datos_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha)),
                      dplyr::across(dplyr::all_of(.col_nombres))) |>
      dplyr::summarise(!!rlang::sym(.col_valores) := sum(!!rlang::sym(.col_valores), na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.col_fecha)),
                     dplyr::across(dplyr::all_of(.col_nombres))) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres))) |>
      dplyr::mutate(
        # dato_anterior_grupo = dplyr::lag(!!rlang::sym(.col_valores), periodos_atras + 1),
        !!rlang::sym(col_contribucion) :=
          (!!rlang::sym(.col_valores) - dplyr::lag(!!rlang::sym(.col_valores), periodos_atras)) /
          dato_anterior_total) |>
      # dplyr::select(-dato_anterior_grupo) |>
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
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres))) |>
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
#' Calcula el peso sobre el total del flujo. Devuelve un dataframe como el original, pero con una columna adiciona: `peso`, que hace referencia al peso del flujo sobre el total de `col.nombres` en la fecha indicada.
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
#' @param .col_flujo Cadena de caracteres. Establece el nombre de la columna que determina el sentido del flujo. Si no existiese esta columna y todas las observaciones reflejasen el mismo tipo de flujo permite establecer `.col_flujo = NULL`, que es su valor por defecto. Nótese que también se puede establecer la columna flujo en `.col_nombres` con el mismo efecto.
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
    tidyr::replace_na(setNames(list(0), .col_valores)) |>
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
#' Calcula la contribución al crecimiento en los últimos `.años_atras_contribucion` del acumulado a `.periodos_atras_acumulado`. Devuelve un dataframe con 2 columnas: `nombres` y `valores`, que hacen referencia a la contribución a `años_atras_contribucion` a la variación del `.flujo` en el último periodo con datos disponibles.
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
    "No existe la columna señalada para los valores." = .col_valores_contr_acum %in% names(.datos_df)
  )

  if (!is.null(.col_filtro)) {
    stopifnot("Las columnas especificadas en .col_filtro no existen." = all(.col_filtro %in% names(.datos_df)))
  }

  if (!is.null(.col_filtro)) {
    .datos_df <-
      .datos_df |>
      dplyr::filter(!!rlang::sym(.col_filtro) == .filtro)
  }

  if (!inherits(.datos_df[[.col_fecha_contr_acum]], "Date")) {
    warning("Se ha convertido la columna de fechas a formato fecha.")
    .datos_df[[.col_fecha_contr_acum]] <- as.Date(.datos_df[[.col_fecha_contr_acum]])
  }

  if(!is.null(.periodos_atras_acumulado)){
    periodos_atras_acumulado <- .periodos_atras_acumulado
  } else if (.frecuencia == "anual"){
    periodos_atras_acumulado <- 1
  } else if (.frecuencia == "semestral"){
    periodos_atras_acumulado <- 2
  } else if (.frecuencia == "trimestral"){
    periodos_atras_acumulado <- 4
  } else if (.frecuencia == "mensual"){
    periodos_atras_acumulado <- 12
  } else if (.frecuencia == "semanal"){
    periodos_atras_acumulado <- 52
  } else if (.frecuencia == "diaria"){
    periodos_atras_acumulado <- 365
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
    .datos_df[!is.na(.datos_df[[.col_fecha_contr_acum]]),]

  min_date <- min(.datos_df[[.col_fecha_contr_acum]], na.rm = TRUE)
  max_date <- max(.datos_df[[.col_fecha_contr_acum]], na.rm = TRUE)

  if (is.infinite(min_date) || is.infinite(max_date)) {
    stop("Las fechas no tienen un formato válido.")
  }

  date_seq <- seq.Date(from = min_date,
                       to = max_date,
                       by = variable_secuencia)

  .datos_df <-
    .datos_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres_contr_acum))) |>
    tidyr::complete(!!rlang::sym(.col_fecha_contr_acum) := date_seq) |>
    tidyr::replace_na(setNames(list(0), .col_valores_contr_acum)) |>
    dplyr::ungroup()

  if (is.null(.col_nombres_contr_acum) & is.null(.col_flujo)) {
    datos_df <-
      .datos_df |>
      dplyr::select(dplyr::all_of(.col_fecha_contr_acum), dplyr::all_of(.col_valores_contr_acum)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha_contr_acum))) |>
      dplyr::summarise(.col_valores_contr_acum = sum(.col_valores_contr_acum, na.rm = TRUE)) |>
      dplyr::ungroup()

  } else if (!is.null(.col_nombres_contr_acum) & is.null(.col_flujo)) {
    datos_df <-
      .datos_df |>
      dplyr::select(dplyr::all_of(.col_fecha_contr_acum), dplyr::all_of(.col_nombres_contr_acum), dplyr::all_of(.col_valores_contr_acum)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha_contr_acum)), dplyr::across(dplyr::all_of(.col_nombres_contr_acum))) |>
      dplyr::summarise(.col_valores_contr_acum = sum(.col_valores_contr_acum, na.rm = TRUE)) |>
      dplyr::ungroup()
  } else if (is.null(.col_nombres_contr_acum) & !is.null(.col_flujo)) {
    datos_df <-
      .datos_df |>
      dplyr::select(dplyr::all_of(.col_fecha_contr_acum), dplyr::all_of(.col_flujo), dplyr::all_of(.col_valores_contr_acum)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha_contr_acum)), dplyr::across(dplyr::all_of(.col_flujo))) |>
      dplyr::summarise(.col_valores_contr_acum = sum(.col_valores_contr_acum, na.rm = TRUE)) |>
      dplyr::ungroup()
  } else if (!is.null(.col_nombres_contr_acum) & !is.null(.col_flujo)) {
    datos_df <-
      .datos_df |>
      dplyr::select(dplyr::all_of(.col_fecha_contr_acum), dplyr::all_of(.col_flujo), dplyr::all_of(.col_nombres_contr_acum), dplyr::all_of(.col_valores_contr_acum)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_fecha_contr_acum)), dplyr::across(dplyr::all_of(.col_flujo)), dplyr::across(dplyr::all_of(.col_nombres_contr_acum))) |>
      dplyr::summarise(.col_valores_contr_acum = sum(.col_valores_contr_acum, na.rm = TRUE)) |>
      dplyr::ungroup()
  }

  col_acumulado <- paste0("acumulado_", periodos_atras_acumulado, "_", periodo)

  acumulados_df <-
    datos_df |>
    get_acumulado(.periodos_atras = periodos_atras_acumulado, .col_valores = .col_valores_contr_acum, .col_nombres = .col_nombres_contr_acum, .col_fecha = .col_fecha_contr_acum, .col_flujo = .col_flujo) |>
    dplyr::filter(.col_fecha_contr_acum %in% c(max(.col_fecha_contr_acum), max(.col_fecha_contr_acum) - years(.años_atras_contribucion))) |>
    dplyr::rename(col_acumulado = !!rlang::sym(col_acumulado))

  if (is.null(.col_nombres_contr_acum) & is.null(.col_flujo)) {
    dato_anterior_total <-
      acumulados_df |>
      dplyr::filter(.col_fecha_contr_acum == min(.col_fecha_contr_acum)) |>
      dplyr::group_by(.col_fecha_contr_acum) |>
      dplyr::summarise(col_acumulado = sum(col_acumulado, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::pull(col_acumulado)

    contribucion_acumulada_df <-
      acumulados_df |>
      dplyr::select(dplyr::all_of(.col_fecha_contr_acum), col_acumulado) |>
      dplyr::mutate(col_contribucion = (col_acumulado - dplyr::lag(col_acumulado, 1)) * 100 / (dato_anterior_total)) |>
      dplyr::filter(.col_fecha_contr_acum == max(.col_fecha_contr_acum)) |>
      dplyr::arrange(desc(col_contribucion)) |>
      dplyr::select(col_contribucion) |>
      dplyr::rename(valores = col_contribucion)
  } else if (!is.null(.col_nombres_contr_acum) & is.null(.col_flujo)){
    dato_anterior_total <-
      acumulados_df |>
      dplyr::filter(.col_fecha_contr_acum == min(.col_fecha_contr_acum)) |>
      dplyr::group_by(.col_fecha_contr_acum) |>
      dplyr::summarise(col_acumulado = sum(col_acumulado, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::pull(col_acumulado)

    contribucion_acumulada_df <-
      acumulados_df |>
      dplyr::select(dplyr::all_of(.col_fecha_contr_acum), dplyr::all_of(.col_nombres_contr_acum), col_acumulado) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres_contr_acum))) |>
      dplyr::mutate(col_contribucion = (col_acumulado - dplyr::lag(col_acumulado, 1)) * 100 / (dato_anterior_total)) |>
      dplyr::ungroup() |>
      dplyr::filter(.col_fecha_contr_acum == max(.col_fecha_contr_acum)) |>
      dplyr::arrange(desc(col_contribucion)) |>
      dplyr::select(dplyr::all_of(.col_nombres_contr_acum), col_contribucion) |>
      dplyr::rename(valores = col_contribucion)
  } else if (is.null(.col_nombres_contr_acum) & !is.null(.col_flujo)){
    contribucion_acumulada_df <-
      acumulados_df |>
      dplyr::select(dplyr::all_of(.col_fecha_contr_acum), dplyr::all_of(.col_flujo), col_acumulado) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo))) |>
      dplyr::group_modify(~{
        grupo_actual <- .x
        dato_anterior_grupo <-
          grupo_actual |>
          dplyr::filter(.col_fecha_contr_acum == min(.col_fecha_contr_acum)) |>
          dplyr::group_by(.col_fecha_contr_acum) |>
          dplyr::summarise(col_acumulado = sum(col_acumulado, na.rm = TRUE)) |>
          dplyr::ungroup() |>
          dplyr::pull(col_acumulado)
        grupo_actual |>
          dplyr::mutate(col_contribucion = (col_acumulado - dplyr::lag(col_acumulado, 1)) * 100 / (dato_anterior_grupo)) |>
          dplyr::filter(.col_fecha_contr_acum == max(.col_fecha_contr_acum)) |>
          dplyr::arrange(desc(col_contribucion)) |>
          dplyr::select(col_contribucion) |>
          dplyr::rename(valores = col_contribucion)
        }) |>
      dplyr::ungroup()
  } else if (!is.null(.col_nombres_contr_acum) & !is.null(.col_flujo)){
    contribucion_acumulada_df <-
      acumulados_df |>
      dplyr::select(dplyr::all_of(.col_fecha_contr_acum), dplyr::all_of(.col_nombres_contr_acum), dplyr::all_of(.col_flujo), col_acumulado) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.col_flujo))) |>
      dplyr::group_modify( ~ {
        grupo_actual <- .x
        dato_anterior_grupo <-
          grupo_actual |>
          dplyr::filter(.col_fecha_contr_acum == min(.col_fecha_contr_acum)) |>
          dplyr::group_by(.col_fecha_contr_acum) |>
          dplyr::summarise(col_acumulado = sum(col_acumulado, na.rm = TRUE)) |>
          dplyr::ungroup() |>
          dplyr::pull(col_acumulado)
        grupo_actual |>
          dplyr::group_by(dplyr::across(dplyr::all_of(.col_nombres_contr_acum))) |>
          dplyr::mutate(col_contribucion = (col_acumulado - dplyr::lag(col_acumulado, 1)) * 100 / (dato_anterior_grupo)) |>
          dplyr::ungroup() |>
          dplyr::filter(.col_fecha_contr_acum == max(.col_fecha_contr_acum)) |>
          dplyr::arrange(desc(col_contribucion)) |>
          dplyr::select(dplyr::all_of(.col_nombres_contr_acum), col_contribucion) |>
          dplyr::rename(valores = col_contribucion)
      }) |>
      dplyr::ungroup()
  }

  if(!.en_tanto_por_uno){
    .datos_df <-
      .datos_df |>
      dplyr::mutate(valores = valores * 100)
  }

  return(contribucion_acumulada_df)
}
