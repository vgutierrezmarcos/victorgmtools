# Aplicación Shiny para la distribución salarial de la EES ----

#' Lanzar la aplicación de distribución salarial de la EES
#'
#' Abre una aplicación Shiny interactiva que permite explorar la distribución
#' del salario por hora de la Encuesta de Estructura Salarial (EES) y
#' compararla según distintos criterios (sexo, tipo de propiedad, tipo de
#' contrato, etc.).
#'
#' @param .ruta_datos Cadena de caracteres. Ruta al archivo `.RData` con los
#'   datos armonizados de la EES (`ees_armonizado`). Si es `NULL`, se intentará
#'   cargar desde `./data/ees_armonizado.RData`.
#' @param ... Argumentos adicionales pasados a [shiny::runApp()].
#' @return No devuelve valor (lanza la app).
#' @examples
#' \dontrun{
#' app_salarios_ees("data/ees_armonizado.RData")
#' }
#' @export
app_salarios_ees <- function(.ruta_datos = NULL, ...) {

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("El paquete 'shiny' es necesario. Instalar con: install.packages('shiny')")
  }
  if (!requireNamespace("bslib", quietly = TRUE)) {
    stop("El paquete 'bslib' es necesario. Instalar con: install.packages('bslib')")
  }

  # --- Resolver ruta de datos ---
  if (is.null(.ruta_datos)) {
    rutas_candidatas <- c(
      "./data/ees_armonizado.RData",
      "../ees/data/ees_armonizado.RData"
    )
    .ruta_datos <- rutas_candidatas[file.exists(rutas_candidatas)][1]
    if (is.na(.ruta_datos)) {
      stop("No se encontro ees_armonizado.RData. Proporciona la ruta con .ruta_datos.")
    }
  }

  stopifnot(file.exists(.ruta_datos))

  # --- Cargar y preparar datos ---
  message("Cargando datos desde: ", .ruta_datos)
  env <- new.env()
  load(.ruta_datos, envir = env)

  nombre_obj <- ls(env)[1]
  datos_raw <- get(nombre_obj, envir = env)
  rm(env)

  message("Preparando variables salariales...")
  datos <- preparar_datos_salarios(datos_raw)
  message("Datos listos: ", scales::comma(nrow(datos)), " observaciones validas")

  # --- Pasar datos a la app y lanzar ---
  shiny::shinyOptions(ees_datos = datos)

  app_dir <- system.file("app_salarios_ees", package = "victorgmtools")
  if (app_dir == "") {
    app_dir <- file.path(
      system.file(package = "victorgmtools"),
      "..", "..", "inst", "app_salarios_ees"
    )
    if (!dir.exists(app_dir)) {
      app_dir <- "inst/app_salarios_ees"
    }
  }

  shiny::runApp(app_dir, ...)
}


# Preparar datos para la app de salarios ----

#' Computa el salario por hora y prepara variables para la app
#'
#' @param .datos Data frame con los datos armonizados de la EES.
#' @return Data frame procesado con `salario_hora` y `ln_salario_hora`.
#' @keywords internal
preparar_datos_salarios <- function(.datos) {

  .datos <- .datos |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::any_of(c(
          "salario_base", "complementos_salariales", "complementos_turno_noche",
          "pago_horas_extra", "paga_extra_mensual", "horas_extraordinarias",
          "anos_antiguedad", "meses_antiguedad", "factor_elevacion",
          "jornada_semanal_pactada", "jornada_semanal_horas",
          "jornada_semanal_minutos"
        )),
        ~ as.numeric(as.character(.x))
      ),
      horas_semanales = dplyr::coalesce(
        .data$jornada_semanal_pactada,
        .data$jornada_semanal_horas +
          dplyr::coalesce(.data$jornada_semanal_minutos, 0) / 60
      ),
      salario_bruto_mes =
        dplyr::coalesce(.data$salario_base, 0) +
        dplyr::coalesce(.data$complementos_salariales, 0) +
        dplyr::coalesce(.data$complementos_turno_noche, 0) +
        dplyr::coalesce(.data$pago_horas_extra, 0) +
        dplyr::coalesce(.data$paga_extra_mensual, 0),
      salario_hora = .data$salario_bruto_mes /
        (.data$horas_semanales * 4.35 +
           dplyr::coalesce(.data$horas_extraordinarias, 0)),
      ln_salario_hora = log(.data$salario_hora)
    )

  # Filtros de calidad
  .datos <- .datos |>
    dplyr::filter(
      is.finite(.data$salario_hora),
      .data$salario_hora > 0,
      is.finite(.data$horas_semanales),
      .data$horas_semanales > 0,
      !.data$tamano_unidad %in% c("0", "4")
    )

  # Asegurar tipo integer para periodo
  if ("periodo" %in% names(.datos)) {
    .datos <- .datos |>
      dplyr::mutate(periodo = as.integer(.data$periodo))
  }

  .datos
}
