# Datos de FRED (Reserva Federal de San Luis) ----

# Descarga una serie de FRED y sus metadatos (título, unidades y frecuencia)
.descargar_fred <- function(.analysed_series_id,
                            .analysed_observation_start = NULL,
                            .analysed_observation_end = NULL,
                            .analysed_frequency = NULL,
                            .analysed_aggregation_method = NULL,
                            .analysed_limit = NULL,
                            .analysed_offset = NULL,
                            .analysed_sort_order = NULL,
                            .analysed_units = NULL,
                            .analysed_realtime_start = NULL,
                            .analysed_realtime_end = NULL,
                            .analysed_vintage_dates = NULL,
                            .analysed_output_type = NULL) {
  datos_df <-
    fredr::fredr(series_id = .analysed_series_id,
                 observation_start = .analysed_observation_start,
                 observation_end = .analysed_observation_end,
                 frequency = .analysed_frequency,
                 aggregation_method = .analysed_aggregation_method,
                 limit = .analysed_limit,
                 offset = .analysed_offset,
                 sort_order = .analysed_sort_order,
                 units = .analysed_units,
                 realtime_start = .analysed_realtime_start,
                 realtime_end = .analysed_realtime_end,
                 vintage_dates = .analysed_vintage_dates,
                 output_type = .analysed_output_type) |>
    dplyr::select(fecha = date, valores = value)

  serie <- fredr::fredr_series(series_id = .analysed_series_id)

  list(
    datos = datos_df,
    titulo = serie$title[[1]],
    unidades = serie$units[[1]],
    unidades_cortas = serie$units_short[[1]],
    frecuencia = serie$frequency[[1]]
  )
}

# Formatea la fecha según la frecuencia de la serie (anual, trimestral, mensual, diaria)
.formatear_fecha_fred <- function(fecha, frecuencia) {
  fecha <- as.Date(fecha)
  switch(
    frecuencia,
    "Annual" = format(fecha, "%Y"),
    "Quarterly" = paste0(lubridate::quarter(fecha), "T", lubridate::year(fecha)),
    "Monthly" = format(fecha, "%m/%Y"),
    "Daily" = format(fecha, "%d/%m/%Y"),
    format(fecha, "%d/%m/%Y")
  )
}

#' Obtener un dataframe con datos de FRED
#'
#' Descarga una serie temporal de FRED (Federal Reserve Bank of St. Louis) y devuelve un tibble con
#' dos columnas, `Fecha` y `Valores`. La fecha se formatea según la frecuencia de la serie (anual,
#' trimestral, mensual o diaria). Requiere una clave de API configurada con [fredr::fredr_set_key()].
#'
#' @param .analysed_series_id Requerido. Cadena de caracteres. Código de la serie FRED. Se pueden consultar todas las series disponibles en \url{https://fred.stlouisfed.org/}.
#' @param .analysed_observation_start Fecha o cadena de caracteres (formato YYYY-MM-DD). Primera observación de la serie temporal. Por defecto, `NULL` (desde la primera observación disponible).
#' @param .analysed_observation_end Fecha o cadena de caracteres (formato YYYY-MM-DD). Última observación de la serie temporal. Por defecto, `NULL` (hasta la última observación disponible).
#' @param .analysed_frequency Cadena de caracteres. Frecuencia de agregación de los datos: `"d"` (diaria), `"w"` (semanal), `"bw"` (bisemanal), `"m"` (mensual), `"q"` (trimestral), `"sa"` (semestral), `"a"` (anual). Por defecto, `NULL` (frecuencia original de la serie).
#' @param .analysed_aggregation_method Cadena de caracteres. Método de agregación cuando se cambia la frecuencia: `"avg"` (media), `"sum"` (suma), `"eop"` (fin de periodo). Por defecto, `NULL`.
#' @param .analysed_limit Entero. Número máximo de observaciones a devolver. Por defecto, `NULL` (sin límite).
#' @param .analysed_offset Entero. Número de observaciones a saltar desde el inicio. Por defecto, `NULL`.
#' @param .analysed_sort_order Cadena de caracteres. Orden de las observaciones: `"asc"` o `"desc"`. Por defecto, `NULL`.
#' @param .analysed_units Cadena de caracteres. Transformación de unidades: `"lin"` (niveles), `"chg"` (cambio), `"ch1"` (cambio interanual), `"pch"` (cambio porcentual), `"pc1"` (cambio porcentual interanual), `"pca"` (cambio porcentual anualizado), `"cch"` (cambio porcentual compuesto), `"cca"` (cambio porcentual compuesto anualizado), `"log"` (logaritmo natural). Por defecto, `NULL`.
#' @param .analysed_realtime_start Fecha o cadena de caracteres. Inicio del periodo de tiempo real para datos revisados. Por defecto, `NULL`.
#' @param .analysed_realtime_end Fecha o cadena de caracteres. Fin del periodo de tiempo real para datos revisados. Por defecto, `NULL`.
#' @param .analysed_vintage_dates Cadena de caracteres o vector. Fechas de vintage para obtener datos históricos. Por defecto, `NULL`.
#' @param .analysed_output_type Entero. Tipo de salida: 1 (observaciones), 2 (todas las revisiones), 3 (nuevas y revisadas), 4 (fechas de publicación inicial). Por defecto, `NULL`.
#' @return Un tibble con dos columnas: `Fecha` (formateada según la frecuencia) y `Valores`.
#' @examples
#' \dontrun{
#' get_fred_df("GDP")
#' get_fred_df("UNRATE", .analysed_observation_start = "2020-01-01")
#' }
#' @seealso [get_fred_tbl()], [get_fred_plt()]
#' @export
get_fred_df <- function(.analysed_series_id,
                        .analysed_observation_start = NULL,
                        .analysed_observation_end = NULL,
                        .analysed_frequency = NULL,
                        .analysed_aggregation_method = NULL,
                        .analysed_limit = NULL,
                        .analysed_offset = NULL,
                        .analysed_sort_order = NULL,
                        .analysed_units = NULL,
                        .analysed_realtime_start = NULL,
                        .analysed_realtime_end = NULL,
                        .analysed_vintage_dates = NULL,
                        .analysed_output_type = NULL) {
  fred <- .descargar_fred(.analysed_series_id, .analysed_observation_start, .analysed_observation_end,
                          .analysed_frequency, .analysed_aggregation_method, .analysed_limit,
                          .analysed_offset, .analysed_sort_order, .analysed_units,
                          .analysed_realtime_start, .analysed_realtime_end, .analysed_vintage_dates,
                          .analysed_output_type)

  fred$datos |>
    dplyr::mutate(Fecha = .formatear_fecha_fred(fecha, fred$frecuencia)) |>
    dplyr::select(Fecha, Valores = valores)
}

#' Obtener una tabla gt con datos de FRED
#'
#' Descarga una serie temporal de FRED y la devuelve como tabla `gt` con el título de la serie y las
#' unidades de medida como encabezado, con el estilo de [tablas_estilo_victorgm()].
#'
#' @inheritParams get_fred_df
#' @param ... Otros argumentos que se pasan a [tablas_estilo_victorgm()].
#' @return Un objeto `gt_tbl`.
#' @examples
#' \dontrun{
#' get_fred_tbl("GDP")
#' }
#' @seealso [get_fred_df()] para obtener el dataframe.
#' @export
get_fred_tbl <- function(.analysed_series_id,
                         .analysed_observation_start = NULL,
                         .analysed_observation_end = NULL,
                         .analysed_frequency = NULL,
                         .analysed_aggregation_method = NULL,
                         .analysed_limit = NULL,
                         .analysed_offset = NULL,
                         .analysed_sort_order = NULL,
                         .analysed_units = NULL,
                         .analysed_realtime_start = NULL,
                         .analysed_realtime_end = NULL,
                         .analysed_vintage_dates = NULL,
                         .analysed_output_type = NULL,
                         ...) {
  fred <- .descargar_fred(.analysed_series_id, .analysed_observation_start, .analysed_observation_end,
                          .analysed_frequency, .analysed_aggregation_method, .analysed_limit,
                          .analysed_offset, .analysed_sort_order, .analysed_units,
                          .analysed_realtime_start, .analysed_realtime_end, .analysed_vintage_dates,
                          .analysed_output_type)

  fred$datos |>
    dplyr::mutate(Fecha = .formatear_fecha_fred(fecha, fred$frecuencia)) |>
    dplyr::select(Fecha, Valores = valores) |>
    tablas_estilo_victorgm(
      .title = fred$titulo,
      .subtitle = fred$unidades,
      .caption = "Fuente: Federal Reserve Bank of St. Louis (FRED).",
      .columnas_numericas = "Valores",
      ...
    )
}

#' Gráfico de una serie de FRED
#'
#' Descarga una serie temporal de FRED y la representa como línea con puntos interactivos, con el
#' estilo de [graficos_estilo_victorgm()]. El título y el subtítulo se toman de los metadatos de la
#' serie si no se indican.
#'
#' @inheritParams get_fred_df
#' @param .title,.subtitle Título y subtítulo del gráfico. Por defecto, el nombre y las unidades de la serie.
#' @param .caption Pie del gráfico. Por defecto, `"Fuente: Federal Reserve Bank of St. Louis (FRED)."`.
#' @param .estatico Valor lógico. `TRUE` devuelve un `ggplot` estático; `FALSE` (por defecto) un gráfico interactivo `ggiraph`.
#' @param .representar_eje_horizontal Valor lógico. Si `TRUE` (por defecto) dibuja una línea horizontal en cero.
#' @param .color Color de la serie. Por defecto, el primero de [colores_victorgm()].
#' @param .estatico_toggle Obsoleto; usar `.estatico`.
#' @param ... Otros argumentos que se pasan a [graficos_estilo_victorgm()].
#' @return Un objeto `ggplot` o `girafe`.
#' @examples
#' \dontrun{
#' get_fred_plt("UNRATE", .analysed_observation_start = "2015-01-01")
#' }
#' @export
get_fred_plt <- function(.analysed_series_id,
                         .analysed_observation_start = NULL,
                         .analysed_observation_end = NULL,
                         .analysed_frequency = NULL,
                         .analysed_aggregation_method = NULL,
                         .analysed_limit = NULL,
                         .analysed_offset = NULL,
                         .analysed_sort_order = NULL,
                         .analysed_units = NULL,
                         .analysed_realtime_start = NULL,
                         .analysed_realtime_end = NULL,
                         .analysed_vintage_dates = NULL,
                         .analysed_output_type = NULL,
                         .title = NULL,
                         .subtitle = NULL,
                         .caption = "Fuente: Federal Reserve Bank of St. Louis (FRED).",
                         .estatico = FALSE,
                         .representar_eje_horizontal = TRUE,
                         .color = NULL,
                         .estatico_toggle = NULL,
                         ...) {
  if (!is.null(.estatico_toggle)) {
    warning("`.estatico_toggle` está obsoleto; usa `.estatico`.")
    .estatico <- .estatico_toggle
  }
  if (is.null(.color)) .color <- colores_victorgm()[[1]]

  fred <- .descargar_fred(.analysed_series_id, .analysed_observation_start, .analysed_observation_end,
                          .analysed_frequency, .analysed_aggregation_method, .analysed_limit,
                          .analysed_offset, .analysed_sort_order, .analysed_units,
                          .analysed_realtime_start, .analysed_realtime_end, .analysed_vintage_dates,
                          .analysed_output_type)

  datos_df <- fred$datos |>
    dplyr::mutate(fecha = as.Date(fecha),
                  fecha_mostrar = .formatear_fecha_fred(fecha, fred$frecuencia))

  formato <- switch(
    fred$frecuencia,
    "Annual" = list(labels = "%Y", breaks = "1 year"),
    "Quarterly" = list(labels = "%b %Y", breaks = "3 months"),
    "Monthly" = list(labels = "%b %Y", breaks = "1 month"),
    "Weekly" = list(labels = "%d %b %Y", breaks = "1 week"),
    "Daily" = list(labels = "%d/%m/%Y", breaks = "1 day"),
    list(labels = "%Y", breaks = "1 year")
  )
  # Con muchas observaciones, cortes anuales para que el eje se lea
  if (nrow(datos_df) > 24 && fred$frecuencia != "Annual") formato <- list(labels = "%Y", breaks = "1 year")

  analysed_plt <-
    ggplot2::ggplot(datos_df, ggplot2::aes(x = fecha, y = valores))

  if (isTRUE(.representar_eje_horizontal)) {
    analysed_plt <- analysed_plt + ggplot2::geom_hline(yintercept = 0)
  }

  analysed_plt <- analysed_plt +
    ggplot2::geom_line(color = .color) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = paste0(fred$titulo, "\n",
                                    es_number(valores, accuracy = 0.1), " ", fred$unidades_cortas,
                                    " (", fecha_mostrar, ")")),
      color = .color, size = 0.8
    )

  analysed_plt |>
    graficos_estilo_victorgm(
      .title = if (is.null(.title)) fred$titulo else .title,
      .subtitle = if (is.null(.subtitle)) fred$unidades else .subtitle,
      .caption = .caption,
      .date_labels = formato$labels,
      .date_breaks = formato$breaks,
      .suffix = paste0(" ", fred$unidades_cortas),
      .estatico = .estatico,
      ...
    )
}
