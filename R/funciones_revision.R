# Revisión del orden de las capas de ggplot ----

# Rango de cada geom: cuanto menor, más al fondo debe ir
.rango_geoms <- c(bar = 1, histogram = 1.5, rect = 1.5, area = 1.5, ribbon = 1.5, tile = 1.5, col = 2,
                  hline = 3, vline = 4, segment = 4.5, abline = 5,
                  line = 6, step = 6, path = 6, smooth = 6,
                  point = 7, label = 8, text = 8, text_repel = 8, label_repel = 8)

#' Revisar el orden de las capas de los gráficos de un fichero
#'
#' Comprueba en ficheros `.R` y `.qmd` que cada `ggplot()` añade sus geoms en el orden
#' `geom_bar`, `geom_col`, `geom_hline`, `geom_vline`, `geom_abline`, `geom_line`, `geom_point`,
#' `geom_label` (también en sus versiones `_interactive` de ggiraph), para que las barras queden al
#' fondo, las referencias por encima de las barras y debajo de las series, los puntos sobre las
#' líneas y las etiquetas arriba del todo. Las capas que no están en la lista se ordenan por el
#' mismo criterio: `geom_rect`, `geom_area`, `geom_ribbon`, `geom_tile` y `geom_histogram` con las
#' barras; `geom_segment` con las referencias; `geom_step`, `geom_path` y `geom_smooth` con las
#' líneas; `geom_text` y los `*_repel` con las etiquetas.
#'
#' Es una heurística sobre el texto: lee cada cadena de capas unidas con `+` desde `ggplot(` hasta
#' la primera línea que no acaba en `+`.
#'
#' @param .rutas Vector de rutas a ficheros o carpetas (se recorren de forma recursiva).
#' @param .silencioso Valor lógico. Si `TRUE`, no imprime el resultado. Por defecto, `FALSE`.
#' @return De forma invisible, un data frame con una fila por problema (`fichero`, `linea`, `mensaje`); vacío si el orden es correcto.
#' @examples
#' \dontrun{
#' revisar_orden_geoms("R/")
#' revisar_orden_geoms(c("informe.qmd", "graficos.R"))
#' }
#' @export
revisar_orden_geoms <- function(.rutas, .silencioso = FALSE) {
  ficheros <- unlist(lapply(.rutas, function(a) {
    if (dir.exists(a)) list.files(a, "\\.(R|r|qmd|Rmd)$", recursive = TRUE, full.names = TRUE) else a
  }))
  ficheros <- ficheros[!grepl("(^|/)(_freeze|renv|\\.quarto)/", ficheros)]
  no_existe <- ficheros[!file.exists(ficheros)]
  if (length(no_existe)) warning("No existe: ", paste(no_existe, collapse = ", "))
  ficheros <- ficheros[file.exists(ficheros)]

  problemas <- do.call(rbind, lapply(ficheros, .revisar_orden_geoms_fichero))
  if (is.null(problemas)) {
    problemas <- data.frame(fichero = character(), linea = integer(), mensaje = character(),
                            stringsAsFactors = FALSE)
  }

  if (!isTRUE(.silencioso)) {
    if (nrow(problemas)) {
      message(sprintf("Orden de geoms incorrecto en %d gráfico(s):", nrow(problemas)))
      message(paste0("- ", problemas$fichero, ":", problemas$linea, "  ", problemas$mensaje, collapse = "\n"))
    } else {
      message(sprintf("Orden de geoms correcto en %d fichero(s).", length(ficheros)))
    }
  }
  invisible(problemas)
}

.revisar_orden_geoms_fichero <- function(f) {
  l <- readLines(f, warn = FALSE, encoding = "UTF-8")
  l <- sub("#.*$", "", l)
  out <- list()
  i <- 1
  while (i <= length(l)) {
    if (grepl("ggplot\\s*\\(", l[i])) {
      ini <- i
      j <- i
      while (j < length(l) && (grepl("\\+\\s*$", l[j]) || grepl("^\\s*\\+", l[j + 1]))) j <- j + 1
      bloque <- paste(l[ini:j], collapse = "\n")
      geoms <- regmatches(bloque, gregexpr("\\bgeom_[a-z_]+(?=\\s*\\()", bloque, perl = TRUE))[[1]]
      tipo <- sub("^geom_", "", sub("_interactive$", "", geoms))
      r <- .rango_geoms[tipo]
      ok <- !is.na(r)
      if (sum(ok) > 1) {
        rr <- r[ok]
        gg <- geoms[ok]
        for (k in which(diff(rr) < 0)) {
          out[[length(out) + 1]] <- data.frame(
            fichero = f, linea = ini,
            mensaje = sprintf("%s va después de %s (orden: %s)", gg[k + 1], gg[k], paste(gg, collapse = " > ")),
            stringsAsFactors = FALSE
          )
        }
      }
      i <- j + 1
    } else {
      i <- i + 1
    }
  }
  if (length(out)) do.call(rbind, out) else NULL
}
