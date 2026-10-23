# Formato numérico español ----

#' Número con separadores españoles
#'
#' Formatea un vector numérico con punto de miles y coma decimal.
#'
#' @param x Vector numérico.
#' @param ... Otros argumentos de [scales::number()] (por ejemplo `accuracy`, `suffix`, `scale`).
#' @return Vector de caracteres.
#' @examples
#' es_number(1234567.891, accuracy = 0.1)
#' @export
es_number <- function(x, ...) {
  scales::number(x, big.mark = ".", decimal.mark = ",", ...)
}

#' Etiquetas de escala ggplot con separadores españoles
#'
#' @param ... Argumentos de [scales::label_number()].
#' @return Una función de etiquetado para usar en `labels =` de una escala de ggplot2.
#' @examples
#' ggplot2::scale_y_continuous(labels = es_label_number(suffix = " %"))
#' @export
es_label_number <- function(...) {
  scales::label_number(big.mark = ".", decimal.mark = ",", ...)
}

#' Valor en millones de euros
#'
#' Convierte euros a millones con formato español: `"1.234,5 M€"`.
#'
#' @param x Valor en euros.
#' @param accuracy Precisión. Por defecto, `0.1`.
#' @param suffix Sufijo. Por defecto, `" M€"`.
#' @return Vector de caracteres.
#' @examples
#' fmt_mill(1234567890)
#' @export
fmt_mill <- function(x, accuracy = 0.1, suffix = " M€") {
  scales::number(x / 1e6, big.mark = ".", decimal.mark = ",", accuracy = accuracy, suffix = suffix)
}

#' Porcentaje ya expresado en tanto por ciento
#'
#' Formatea un valor que ya está en tanto por ciento, con espacio antes del signo: `"12,3 %"`.
#' Con `signo = TRUE` antepone `+` a los valores positivos (`"+5,3 %"`), útil para variaciones.
#'
#' @param x Valor en tanto por ciento.
#' @param accuracy Precisión. Por defecto, `0.1`.
#' @param signo Valor lógico. Si `TRUE`, los positivos llevan signo `+`. Por defecto, `FALSE`.
#' @return Vector de caracteres.
#' @examples
#' fmt_pct(12.34)
#' fmt_pct(c(5.3, -2.1), signo = TRUE)
#' @export
fmt_pct <- function(x, accuracy = 0.1, signo = FALSE) {
  if (isTRUE(signo)) {
    style <- scales::number(x, big.mark = ".", decimal.mark = ",", accuracy = accuracy, suffix = " %",
                            style_positive = "plus")
    return(style)
  }
  scales::number(x, big.mark = ".", decimal.mark = ",", accuracy = accuracy, suffix = " %")
}

#' Kilogramos a toneladas
#'
#' Convierte kilogramos a toneladas con formato español: `"1.234,5 t"`.
#'
#' @param x Kilogramos.
#' @param accuracy Precisión. Por defecto, `0.1`.
#' @return Vector de caracteres.
#' @examples
#' fmt_ton(1234567)
#' @export
fmt_ton <- function(x, accuracy = 0.1) {
  scales::number(x / 1e3, big.mark = ".", decimal.mark = ",", accuracy = accuracy, suffix = " t")
}

# Cifras en palabras ----

#' Proporción en palabras
#'
#' Convierte una proporción (entre 0 y 1) en una expresión aproximada en palabras para aligerar
#' de cifras un texto: `0.48` pasa a `"casi la mitad"`, `0.27` a `"algo más de una cuarta parte"`.
#'
#' @param x Número entre 0 y 1.
#' @return Cadena de caracteres (o `NA` si `x` es `NA`).
#' @examples
#' fraccion_txt(0.48)
#' fraccion_txt(0.27)
#' @seealso [veces_txt()]
#' @export
fraccion_txt <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    if (v < 0.075) return("menos de una décima parte")
    if (v > 0.95) return("casi la totalidad")
    refs <- c("una décima parte" = 0.1, "una quinta parte" = 0.2, "una cuarta parte" = 0.25,
              "un tercio" = 1 / 3, "dos quintas partes" = 0.4, "la mitad" = 0.5,
              "tres quintas partes" = 0.6, "dos tercios" = 2 / 3, "tres cuartas partes" = 0.75,
              "cuatro quintas partes" = 0.8, "nueve de cada diez" = 0.9)
    i <- which.min(abs(v - refs))
    d <- v - refs[[i]]
    paste0(if (abs(d) <= 0.015) "" else if (d < 0) "casi " else "algo más de ", names(refs)[i])
  }, character(1), USE.NAMES = FALSE)
}

#' Cociente entre dos valores en palabras
#'
#' Convierte el cociente final/inicial en una expresión en palabras: `3.1` pasa a `"se triplica"`,
#' `2.3` a `"se multiplica por más de dos"`, `1.3` a `"aumenta en casi un tercio"`, `0.5` a
#' `"se reduce a la mitad"` y `0.7` a `"cae en casi un tercio"`.
#'
#' @param r Cociente (valor final entre valor inicial).
#' @param plural Valor lógico. Si `TRUE`, concuerda con un sujeto plural (`"las exportaciones se triplican"`). Por defecto, `FALSE`.
#' @return Cadena de caracteres (o `NA` si `r` es `NA`).
#' @examples
#' veces_txt(3.1)
#' veces_txt(0.7, plural = TRUE)
#' @seealso [fraccion_txt()]
#' @export
veces_txt <- function(r, plural = FALSE) {
  vapply(r, function(v) {
    if (is.na(v)) return(NA_character_)
    n_ <- if (plural) "n" else ""
    if (v >= 1.9) {
      n <- round(v)
      pal <- c("", "duplica", "triplica", "cuadruplica", "quintuplica")
      num <- c("", "dos", "tres", "cuatro", "cinco")
      if (n > 5) return(paste0("se multiplica", n_, " por ", n))
      if (abs(v - n) <= 0.1 + 1e-8) return(paste0("se ", pal[n], n_))
      if (v < n) return(paste0("casi se ", pal[n], n_))
      return(paste0("se multiplica", n_, " por más de ", num[n]))
    }
    if (v >= 1) return(paste0("aumenta", n_, " en ", fraccion_txt(v - 1)))
    if (abs(v - 0.5) <= 0.03) return(paste0("se reduce", n_, " a la mitad"))
    paste0(if (plural) "caen" else "cae", " en ", fraccion_txt(1 - v))
  }, character(1), USE.NAMES = FALSE)
}

# Leyendas ----

#' Número de filas para una leyenda equilibrada
#'
#' Calcula cuántas filas debe tener una leyenda para que todas queden completas, o la última con
#' una clave menos solo si hay al menos tres columnas: con 4 claves, una fila de 4 o 2 x 2, nunca
#' 3 + 1; con 5, 3 + 2; con 6, 3 + 3; con 8, 4 + 4. El número de columnas que caben depende del
#' ancho del gráfico (unas 4 con nombres medianos en un gráfico a ancho completo).
#'
#' @param n Número de claves de la leyenda.
#' @param max_col Máximo de columnas que caben. Por defecto, `4`.
#' @return Entero: número de filas, para usar en `.nrows_legend_color` de [graficos_estilo_victorgm()].
#' @examples
#' filas_leyenda(4)
#' filas_leyenda(5)
#' filas_leyenda(6, max_col = 2)
#' @export
filas_leyenda <- function(n, max_col = 4) {
  stopifnot(is.numeric(n), length(n) == 1, n >= 0)
  if (n == 0) return(1L)
  for (f in seq_len(n)) {
    cols <- ceiling(n / f)
    if (cols <= max_col && (n %% cols == 0 || (cols >= 3 && n %% cols == cols - 1))) return(as.integer(f))
  }
  as.integer(ceiling(n / max_col))
}
