# Tipo de cambio ----
#' Función para obtener el tipo de cambio entre 2 divisas en una fecha específica.
#' @param .from Cadena de caracteres. Código ISO a 3 letras de la divisa de base (base currency). Por defecto, `"EUR"`.
#' @param .to Cadena de caracteres. Código ISO a 3 letras de la divisa de cuenta (quote currency). Por defecto, `"USD"`.
#' @param .fecha Fecha en formato "%Y-%m-%d" para la que se quiere obtener el tipo de cambio. Por defecto, hoy.
#' @export
get_tipo_cambio <- function(.from = "EUR",
                            .to = "USD",
                            .fecha = Sys.Date()) {
  # Obtener tasa de cambio histórica para EURUSD
  codigo <- paste0(.from, .to, "=X")
    tipo_cambio <- quantmod::getSymbols(codigo, from = .fecha, to = .fecha, auto.assign = FALSE)
  return(as.numeric(quantmod::Cl(tipo_cambio)))
}

# Conversión ----
#' Función para convertir los valores monetarios de una columna a otra divisa en un dataframe que refleje datos de series temporales.
#' @param .datos_df Data frame que contiene los datos sobre los que se quiere calcular la conversión.
#' @param .from Cadena de caracteres. Código ISO a 3 letras de la divisa de base (base currency). Por defecto, `"EUR"`.
#' @param .to Cadena de caracteres. Código ISO a 3 letras de la divisa de cuenta (quote currency). Por defecto, `"USD"`.
#' @param .frecuencia Cadena de caracteres. Frecuencia de los datos. Puede ser, `"diaria"`, `"mensual"` o `"anual"`. Por defecto, `"mensual"`.
#' @param .col_valores Cadena de caracteres. Nombre de la columna que contiene los datos que se quieren convertir. Por defecto, `"valores"`.
#' @param .col_fecha Cadena de caracteres. Nombre de la columna que contiene las fechas de los datos que se quieren convertir. Por defecto, `"fecha"`.
#' @export
get_conversion <- function(.datos_df,
                           .from = "EUR",
                           .to = "USD",
                           .frecuencia = "mensual",
                           .col_valores = "valores",
                           .col_fecha = "fecha"
                           ) {
  valores_convertidos <- paste0("valores_", .to)
  # Aplicar la conversión a cada fila del dataframe
  .datos_df <-
    .datos_df |>
    dplyr::mutate(!!sym(valores_convertidos) := mapply(function(.col_fecha, .col_valores) {
      tipo_cambio <- get_tipo_cambio(.to = .to,
                                     .from = .from,
                                     .fecha = .col_fecha)
      .col_valores * tipo_cambio
      }))
}

# Dataframe con el tipo de cambio ----
#' Función para obtener un dataframe con datos del tipo de cambio entre 2 divisas en un periodo especificado. Devuelve el dataframe inicial con una columna adicional `tipo_cambio_".from_currency"_".to_currency"`, que convierte los datos de la columna `.col_valores`.
#' @param .from_currency Cadena de caracteres. Código ISO a 3 letras de la divisa de base (base currency). Por defecto, `"EUR"`.
#' @param .to_currency Cadena de caracteres. Código ISO a 3 letras de la divisa de cuenta (quote currency). Por defecto, `"USD"`.
#' @param .from_date Fecha. Fecha de inicio del dataframe a obtener. Por defecto, hoy hace un mes.
#' @param .to_date Fecha. Fecha de final del dataframe a obtener. Por defecto, hoy.
#' @param .frecuencia Frecuencia de los datos. Puede ser, `"diaria"`, `"mensual"` o `"anual"`. Por defecto, `"diaria"`.
#' @export
get_tipo_cambio_df <- function(.from_currency = "EUR",
                               .to_currency = "USD",
                               .from_date = as.Date(seq(as.Date(Sys.Date()), length = 2, by = "-1 month")[2]),
                               .to_date = Sys.Date(),
                               .frecuencia = "diaria",
                               max_retries = 3,
                               retry_delay = 2) {

  if (!.frecuencia %in% c("diaria", "mensual", "anual")) {
    stop(".frecuencia must be either 'diaria', 'mensual' or 'anual'.")
  }

  # Obtener datos en un solo request
  symbol <- paste0(.from_currency, .to_currency, "=X")

  exchange_data <- tryCatch({
    quantmod::getSymbols(symbol,
                         from = .from_date,
                         to = .to_date,
                         auto.assign = FALSE)
  }, error = function(e) {
    stop(paste("Error al obtener datos:", e$message))
  })

  # Convertir a dataframe
  tipo_cambio_df <- data.frame(
    fecha = zoo::index(exchange_data),
    tipo_cambio = as.numeric(exchange_data[, 4])  # Usando el precio de cierre
  )

  if (nrow(tipo_cambio_df) == 0) {
    stop("No se pudieron obtener datos de tipo de cambio para el período especificado.")
  }

  tipo_cambio_df <- tipo_cambio_df |>
    dplyr::mutate(
      year = lubridate::year(fecha),
      month = lubridate::month(fecha)
    )

  # Aplicar transformación de frecuencia si es mensual
  if (.frecuencia == "mensual") {
    tipo_cambio_df <- tipo_cambio_df |>
      dplyr::group_by(year, month) |>
      dplyr::summarise(
        fecha = dplyr::first(fecha),
        tipo_cambio = mean(tipo_cambio, na.rm = TRUE)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(fecha = lubridate::floor_date(fecha, unit = "month"))
  }

  # Aplicar transformación de frecuencia si es anual
  if (.frecuencia == "anual") {
    tipo_cambio_df <- tipo_cambio_df |>
      dplyr::group_by(year) |>
      dplyr::summarise(
        fecha = dplyr::first(fecha),
        tipo_cambio = mean(tipo_cambio, na.rm = TRUE)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(fecha = lubridate::floor_date(fecha, unit = "year"))
  }

  # Renombrar columna de resultado
  col_resultado <- paste0("tipo_cambio_", .from_currency,"_", .to_currency)

  tipo_cambio_df <- tipo_cambio_df |>
    dplyr::select(-year, -month) |>
    dplyr::rename(!!rlang::sym(col_resultado) := tipo_cambio)

  return(tipo_cambio_df)
}

# Gráfica tipo de cambio ----
#' Función para obtener un gráfico con el tipo de cambio entre 2 divisas en una fecha específica.
#' @param .from_currency Cadena de caracteres. Código ISO a 3 letras de la divisa de base (base currency). Por defecto, `"EUR"`.
#' @param .to_currency Cadena de caracteres. Código ISO a 3 letras de la divisa de cuenta (quote currency). Por defecto, `"USD"`.
#' @param .from_date Fecha. Fecha de inicio del gráfico a obtener. Por defecto, hoy hace un mes.
#' @param .to_date Fecha. Fecha de final del gráfico a obtener. Por defecto, hoy.
#' @param .frecuencia Frecuencia de los datos. Puede ser, `"diaria"`, `"mensual"` o `"anual"`. Por defecto, `"diaria"`.
#' @param .estatico Valor lógico. Establecer en `TRUE` si se desea que el gráfico no sea interactivo. Por defecto, `FALSE`.
#' @export
get_tipo_cambio_plt <- function(.from_currency = "EUR",
                                .to_currency = "USD",
                                .from_date = as.Date(seq(as.Date(Sys.Date()), length = 2, by = "-1 month")[2]),
                                .to_date = Sys.Date(),
                                .frecuencia = "diaria",
                                .estatico = FALSE) {

  tipo_cambio_df <-
    victorgmtools::get_tipo_cambio_df(.to_currency = .to_currency,
                                      .from_currency = .from_currency,
                                      .from_date = .from_date,
                                      .to_date = .to_date,
                                      .frecuencia = .frecuencia)

  col_resultado <- paste0("tipo_cambio_", .from_currency,"_", .to_currency)

  tipo_cambio_df <-
    tipo_cambio_df |>
    dplyr::rename(tipo_cambio = !!rlang::sym(col_resultado))

  tipo_cambio_plt <-
    ggplot2::ggplot() +
    ggplot2::geom_line(data = tipo_cambio_df,
                       mapping = ggplot2::aes(x = fecha, y = tipo_cambio))
    ggiraph::geom_point_interactive(data = tipo_cambio_df,
                                    mapping = ggplot2::aes(x = fecha, y = tipo_cambio,
                                                  tooltip = paste0(fecha, "\n",
                                                                   .to_currency, "/", .from_currency, "\n",
                                                                   tipo_cambio)))

  if(.frecuencia == "diaria") {
    tipo_cambio_plt <-
      tipo_cambio_plt |>
      victorgmtools::graficos_estilo_victorgm(.date_labels = "%d-%m-%Y",
                                                .date_breaks = "1 week",
                                                .accuracy = 0.01) +
      ggplot2::theme(plot.title = ggtext::element_textbox_simple())
  } else if (.frecuencia == "mensual"){
    tipo_cambio_plt <-
      tipo_cambio_plt |>
      victorgmtools::graficos_estilo_victorgm(.date_labels = "%b %Y",
                                                .date_breaks = "1 month",
                                                .accuracy = 0.01) +
      ggplot2::theme(plot.title = ggtext::element_textbox_simple())
  } else if (.frecuencia == "anual"){
    tipo_cambio_plt <-
      tipo_cambio_plt |>
      victorgmtools::graficos_estilo_victorgm(.date_labels = "%Y",
                                                .date_breaks = "6 months",
                                                .accuracy = 0.01) +
      ggplot2::theme(plot.title = ggtext::element_textbox_simple())
  }

  if (.estatico == FALSE){
    tipo_cambio_plt <- ggiraph::girafe_options(
      ggiraph::girafe(
        ggobj = tipo_cambio_plt,
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

    tipo_cambio_plt

    # tipo_cambio_plt |>
    #   victorgmtools::mostrar(.estatico)
}
