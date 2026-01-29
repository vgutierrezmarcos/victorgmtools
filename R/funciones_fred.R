# Sacar tabla con datos de FRED (Reserva Federal de Saint Louis)----
#' Obtiene una tabla con los datos de series temporales de FRED (St. Louis FED) adaptado en formato.
#' @param .analysed_series_id Requerido. Cadena de caracteres. Indica el código de la serie de la que se desea obtener la tabla. Se pueden consultar todas las series disponibles en la página web: https://fred.stlouisfed.org/.
#' @param .analysed_observation_start Cadena de caracteres. Primera observación de la serie temporal. Por defecto, NULL, devuelve desde la primera observación disponible.
#' @param .analysed_observation_end Cadena de caracteres. Última observación de la serie temporal. Por defecto, NULL, devuelve desde la última observación disponible.
#' @param .analysed_frequency Cadena de caracteres. Frecuencia de la agregación de los datos. Puede ser: 'd' (diario), 'w' (semanal), 'bw' (bisemanal), 'm' (mensual), 'q' (trimestral), 'sa' (semestral), 'a' (anual), 'wef' (semanal, terminando el viernes), 'weth' (semanal, terminando el jueves), 'wew' (semanal, terminando el miércoles), 'wetu' (semanal, terminando el martes), 'wem' (semanal, terminando el lunes), 'wesu' (semanal, terminando el domingo), 'wesa' (semanal, terminando el sábado), 'bwew' (bisemanal, terminando el miércoles), 'bwem' (bisemanal, terminando el lunes). Por defecto, `NULL` (frecuencia nativa).
#' @param .analysed_aggregation_method Cadena de caracteres. Método de agregación de los datos. Puede ser: 'avg' (promedio), 'sum' (suma), 'eop' (fin de periodo). Por defecto, `NULL`.
#' @param .analysed_limit Número entero. El número máximo de resultados a devolver. Por defecto, `NULL` (devuelve todos).
#' @param .analysed_offset Número entero. El número de resultados a saltar. Por defecto, `NULL`.
#' @param .analysed_sort_order Cadena de caracteres. El orden de clasificación de los resultados. Puede ser 'asc' (ascendente) o 'desc' (descendente). Por defecto, `NULL`.
#' @param .analysed_units Cadena de caracteres. Unidades de transformación de los datos. Puede ser: 'lin' (niveles), 'chg' (cambio), 'ch1' (cambio respecto a hace 1 año), 'pch' (porcentaje de cambio), 'pc1' (porcentaje de cambio respecto a hace 1 año), 'pca' (porcentaje de cambio anual compuesto), 'cch' (cambio continuo compuesto), 'cca' (cambio continuo compuesto anual), 'log' (logaritmo natural). Por defecto, `NULL` (niveles).
#' @param .analysed_realtime_start Cadena de caracteres o Fecha. Fecha de inicio del periodo en tiempo real. Por defecto, `NULL`.
#' @param .analysed_realtime_end Cadena de caracteres o Fecha. Fecha de fin del periodo en tiempo real. Por defecto, `NULL`.
#' @param .analysed_vintage_dates Cadena de caracteres o vector de Fechas. Fechas vintage a descargar. Por defecto, `NULL`.
#' @param .analysed_output_type Número entero. Tipo de salida. 1 = Observaciones por periodo en tiempo real, 2 = Observaciones por fecha vintage, 3 = Observaciones por fecha vintage y todas las observaciones, 4 = Observaciones iniciales por fecha vintage. Por defecto, `NULL`.
#' @return Un objeto `gt_tbl` con los datos de la serie temporal formateados.
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
                         .analysed_output_type = NULL){

  analysed_df <-
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
    dplyr::select(date, value) |>
    dplyr::rename(Fecha = date) |>
    dplyr::rename(Valores = value)

  title <- fredr::fredr_series(series_id = .analysed_series_id) |> dplyr::pull(title)
  measure_units <- fredr::fredr_series(series_id = .analysed_series_id) |> dplyr::pull(units)
  measure_units_short <- fredr::fredr_series(series_id = .analysed_series_id) |> dplyr::pull(units_short)
  frequency <- fredr::fredr_series(series_id = .analysed_series_id) |> dplyr::pull(frequency)

  if(frequency == "Annual") {
    analysed_df <-
      analysed_df |>
      dplyr::mutate(Fecha = format(as.Date(Fecha),"%Y"))
  } else if (frequency == "Quarterly"){
    analysed_df <-
      analysed_df |>
      dplyr::arrange(desc(Fecha)) |>
      dplyr::mutate(Trimestre = lubridate::quarter(Fecha)) |>
      dplyr::mutate(Año = lubridate::year(Fecha)) |>
      dplyr::mutate(Fecha = paste0(Trimestre, "T", Año)) |>
      dplyr::select(-Trimestre, -Año)
  } else if (frequency == "Monthly"){
    analysed_df <-
      analysed_df |>
      dplyr::mutate(Fecha = format(as.Date(Fecha),"%m/%Y"))
  } else if (frequency == "Daily"){
    analysed_df <-
      analysed_df |>
      dplyr::mutate(Fecha = format(as.Date(Fecha),"%d/%m/%Y"))
  }

  analysed_tbl <-
    analysed_df |>
    gt::gt() |>
    gt::tab_header(
      title = gt::md(title,
                     subtitle = measure_units)
    ) |>
    gt::tab_style(
      style = cell_text(weight = "bold"),
      locations = gt::cells_title()
    )

  analysed_tbl
}

# Sacar gráfico con datos de FRED (Reserva Federal de Saint Louis)----
#' Obtiene un gráfico con los datos de series temporales de FRED (St. Louis FED) adaptado en formato.
#' @param .analysed_series_id Requerido. Cadena de caracteres. Indica el código de la serie de la que se desea obtener el gráfico. Se pueden consultar todas las series disponibles en la página web: https://fred.stlouisfed.org/.
#' @param .analysed_observation_start Cadena de caracteres. Primera observación de la serie temporal. Por defecto, NULL, devuelve desde la primera observación disponible.
#' @param .analysed_observation_end Cadena de caracteres. Última observación de la serie temporal. Por defecto, NULL, devuelve desde la última observación disponible.
#' @param .analysed_frequency Cadena de caracteres. Frecuencia de la agregación de los datos. Puede ser: 'd' (diario), 'w' (semanal), 'bw' (bisemanal), 'm' (mensual), 'q' (trimestral), 'sa' (semestral), 'a' (anual), 'wef' (semanal, terminando el viernes), 'weth' (semanal, terminando el jueves), 'wew' (semanal, terminando el miércoles), 'wetu' (semanal, terminando el martes), 'wem' (semanal, terminando el lunes), 'wesu' (semanal, terminando el domingo), 'wesa' (semanal, terminando el sábado), 'bwew' (bisemanal, terminando el miércoles), 'bwem' (bisemanal, terminando el lunes). Por defecto, `NULL` (frecuencia nativa).
#' @param .analysed_aggregation_method Cadena de caracteres. Método de agregación de los datos. Puede ser: 'avg' (promedio), 'sum' (suma), 'eop' (fin de periodo). Por defecto, `NULL`.
#' @param .analysed_limit Número entero. El número máximo de resultados a devolver. Por defecto, `NULL` (devuelve todos).
#' @param .analysed_offset Número entero. El número de resultados a saltar. Por defecto, `NULL`.
#' @param .analysed_sort_order Cadena de caracteres. El orden de clasificación de los resultados. Puede ser 'asc' (ascendente) o 'desc' (descendente). Por defecto, `NULL`.
#' @param .analysed_units Cadena de caracteres. Unidades de transformación de los datos. Puede ser: 'lin' (niveles), 'chg' (cambio), 'ch1' (cambio respecto a hace 1 año), 'pch' (porcentaje de cambio), 'pc1' (porcentaje de cambio respecto a hace 1 año), 'pca' (porcentaje de cambio anual compuesto), 'cch' (cambio continuo compuesto), 'cca' (cambio continuo compuesto anual), 'log' (logaritmo natural). Por defecto, `NULL` (niveles).
#' @param .analysed_realtime_start Cadena de caracteres o Fecha. Fecha de inicio del periodo en tiempo real. Por defecto, `NULL`.
#' @param .analysed_realtime_end Cadena de caracteres o Fecha. Fecha de fin del periodo en tiempo real. Por defecto, `NULL`.
#' @param .analysed_vintage_dates Cadena de caracteres o vector de Fechas. Fechas vintage a descargar. Por defecto, `NULL`.
#' @param .analysed_output_type Número entero. Tipo de salida. 1 = Observaciones por periodo en tiempo real, 2 = Observaciones por fecha vintage, 3 = Observaciones por fecha vintage y todas las observaciones, 4 = Observaciones iniciales por fecha vintage. Por defecto, `NULL`.
#' @param .estatico_toggle Valor lógico. En caso de `TRUE` el gráfico devuelto no es interactivo (no usa el paquete ggiraph). Por defecto, `FALSE`.
#' @param .representar_eje_horizontal Valor lógico. En caso de `TRUE` se dibuja una línea horizontal en y = 0. Por defecto, `TRUE`.
#' @return Un objeto `ggplot` (estático) o `girafe` (interactivo) con la visualización de la serie temporal.
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
                         .estatico_toggle = FALSE,
                         .representar_eje_horizontal = TRUE){

  estatico_toggle <- .estatico_toggle

  analysed_df <-
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
    dplyr::select(date, value) |>
    dplyr::rename(fecha = date) |>
    dplyr::rename(valores = value)

  title <- fredr::fredr_series(series_id = .analysed_series_id) |> dplyr::pull(title)
  measure_units <- fredr::fredr_series(series_id = .analysed_series_id) |> dplyr::pull(units)
  measure_units_short <- fredr::fredr_series(series_id = .analysed_series_id) |> dplyr::pull(units_short)
  frequency <- fredr::fredr_series(series_id = .analysed_series_id) |> dplyr::pull(frequency)

  if(frequency == "Annual") {
    analysed_df <-
      analysed_df |>
      dplyr::mutate(fecha = format(as.Date(fecha),"%Y")) |>
      dplyr::mutate(fecha_mostrar = fecha)
  } else if (frequency == "Quarterly"){
    analysed_df <-
      analysed_df |>
      dplyr::mutate(trimestre = lubridate::quarter(fecha)) |>
      dplyr::mutate(año = lubridate::year(fecha)) |>
      dplyr::mutate(fecha_mostrar = paste0(trimestre, "T", año)) |>
      dplyr::select(-trimestre, -año)

  } else if (frequency == "Monthly"){
    analysed_df <-
      analysed_df |>
      dplyr::mutate(fecha = format(as.Date(fecha),"%m/%Y")) |>
      dplyr::mutate(fecha_mostrar = fecha)
  } else if (frequency == "Daily"){
    analysed_df <-
      analysed_df |>
      dplyr::mutate(fecha = format(as.Date(fecha),"%d/%m/%Y")) |>
      dplyr::mutate(fecha_mostrar = fecha)
  } else {
    analysed_df <-
      analysed_df |>
      dplyr::mutate(fecha_mostrar = fecha)
  }

  analysed_plt <-
    ggplot2::ggplot(analysed_df, ggplot2::aes(x = fecha, y = valores)) +
    ggplot2::geom_line(color = palette()[[2]]) +
    ggiraph::geom_point_interactive(ggplot2::aes(tooltip = paste0(title, "\n", formatC(round(valores, 1), format = "f", big.mark = ".", digits = 1), measure_units_short, " a ", fecha_mostrar)),
                                    color = palette()[[2]],  # Color fijo para los puntos
                                    stat = "identity",
                                    size = 0.5) +
    ggplot2::theme(
      legend.position = "bottom", legend.box = "horizontal",
      plot.title = ggtext::element_textbox_simple(size = 12)
    ) +
    ggplot2::ggtitle(paste0("**", title, "**", ". ", "_", measure_units,"_", "."))

  analysed_plt <-
    analysed_plt |>
    victorgmtools::graficos_estilo_victorgm() +
    ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 1,
                                                      scale = 1,
                                                      suffix = measure_units_short),
                       position = "right") +
    ggplot2::theme(plot.title = ggtext::element_textbox_simple())

  if(frequency == "Annual") {
    analysed_plt <-
      analysed_plt +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  } else if (frequency == "Quarterly"){
    analysed_plt <-
      analysed_plt +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
  } else if (frequency == "Monthly"){
    analysed_plt <-
      analysed_plt +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
  } else if (frequency == "Weekly"){
    analysed_plt <-
      analysed_plt +
      scale_x_date(date_breaks = "1 week", date_labels = "%W")
  } else if (frequency == "Daily"){
    analysed_plt <-
      analysed_plt +
      scale_x_date(date_breaks = "1 day", date_labels = "%d/%m/%Y")
  }

  if (.representar_eje_horizontal == TRUE){
    analysed_plt <-
      analysed_plt +
      ggplot2::geom_hline(yintercept = 0)
  }

  if (estatico_toggle == FALSE){
    analysed_plt <- ggiraph::girafe_options(
      ggiraph::girafe(
        ggobj = analysed_plt,
        width_svg = 6,
        height_svg = 5,
        options = list(
          ggiraph::opts_hover(css = ''),
          ggiraph::opts_hover_inv(css = "opacity:0.4;"),
          ggiraph::opts_sizing(rescale = FALSE),
          position = "topright",
          saveaspng = TRUE
        )
      ),
      ggiraph::opts_sizing(rescale = TRUE, width = 1)
    )
  }

  analysed_plt
}
