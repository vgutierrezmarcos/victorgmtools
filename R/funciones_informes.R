# Helpers de informes y artículos en Quarto ----
#
# Mismo vocabulario que el formato de informes de la SGEYPC (kpis, resumen, figura, tabla), con la
# estética de victorgutierrezmarcos.es: morado como tinta, verde salvia como papel y oro como filete.
# El CSS que da forma a estas piezas está en inst/css/informe_victorgm.css (`css_informe_victorgm()`).

# Estado del documento en curso: contadores de gráficos y tablas y prefijo de anexo
.informe <- new.env(parent = emptyenv())
.informe$n <- list("Gráfico" = 0L, Tabla = 0L)
.informe$prefijo <- ""

#' Reiniciar los contadores de gráficos y tablas de un informe
#'
#' Se llama en el chunk de configuración de un documento Quarto para que la numeración de
#' [figura()] y [tabla()] empiece en 1. Opcionalmente fija [tema_victorgm()] como tema por defecto
#' de ggplot2.
#'
#' @param .fijar_tema Valor lógico. Si `TRUE`, ejecuta `ggplot2::theme_set(tema_victorgm())`. Por defecto, `FALSE`.
#' @return `NULL`, de forma invisible.
#' @export
iniciar_informe <- function(.fijar_tema = FALSE) {
  .informe$n <- list("Gráfico" = 0L, Tabla = 0L)
  .informe$prefijo <- ""
  if (isTRUE(.fijar_tema)) ggplot2::theme_set(ggplot2::theme_minimal() + tema_victorgm())
  invisible(NULL)
}

#' Numeración propia de un anexo
#'
#' A partir de la llamada, los gráficos y tablas se numeran como `"Gráfico A.1"`, `"Tabla A.1"`...
#' con los contadores reiniciados.
#'
#' @param letra Letra o prefijo del anexo (`"A"`, `"B"`...).
#' @return `NULL`, de forma invisible.
#' @export
numeracion_anexo <- function(letra) {
  .informe$prefijo <- paste0(letra, ".")
  .informe$n <- list("Gráfico" = 0L, Tabla = 0L)
  invisible(NULL)
}

.siguiente_numero <- function(tipo) {
  n <- .informe$n[[tipo]] + 1L
  .informe$n[[tipo]] <- n
  n
}

#' CSS de los informes con la estética de victorgutierrezmarcos.es
#'
#' Devuelve la hoja de estilos que da forma a [kpis()], [resumen()], [figura()] y [tabla()] (caja
#' sombreada, rótulo numerado con filete oro, fuente y nota) en los colores de la web, con tema
#' claro y oscuro. En un documento Quarto HTML se incluye con un chunk de resultados `asis`.
#'
#' @param .como Cadena de caracteres. `"tag"` (por defecto) devuelve un `<style>` de htmltools listo para
#'   imprimir en el documento; `"ruta"` devuelve la ruta del fichero CSS, por ejemplo para `css:` en el YAML.
#' @return Un objeto `shiny.tag` o una ruta.
#' @examples
#' css_informe_victorgm(.como = "ruta")
#' @export
css_informe_victorgm <- function(.como = c("tag", "ruta")) {
  .como <- match.arg(.como)
  ruta <- system.file("css", "informe_victorgm.css", package = "victorgmtools")
  if (.como == "ruta") return(ruta)
  htmltools::includeCSS(ruta)
}

# ---------- Mostrar gráficos según el formato de salida ----------

.css_tooltip_victorgm <- function(.fuente_letra = "Source Sans 3", .oscuro = FALSE) {
  tk <- tokens_victorgm(.oscuro = .oscuro)
  sprintf(
    "background-color:%s;color:%s;font-family:'%s',sans-serif;font-size:13px;border:1px solid %s;border-radius:4px;padding:8px 10px;box-shadow:0 1px 2px rgba(42,37,48,0.12);",
    tk[["superficie"]], tk[["texto"]], .fuente_letra, tk[["primario"]]
  )
}

.widget_victorgm <- function(p, alto, ancho, .fuente_letra = "Source Sans 3", .oscuro = FALSE) {
  ggiraph::girafe(
    ggobj = p, width_svg = ancho, height_svg = alto, bg = "transparent",
    options = list(
      ggiraph::opts_sizing(rescale = TRUE, width = 1),
      ggiraph::opts_toolbar(saveaspng = FALSE),
      ggiraph::opts_hover(css = ""),
      ggiraph::opts_hover_inv(css = "opacity:0.4;"),
      ggiraph::opts_tooltip(css = .css_tooltip_victorgm(.fuente_letra, .oscuro), use_cursor_pos = TRUE)
    )
  )
}

#' Mostrar un ggplot de forma interactiva en HTML y estática en PDF o Word
#'
#' En una salida HTML de Quarto o R Markdown devuelve el gráfico como widget `ggiraph` (con
#' tooltips si el gráfico usa `geom_*_interactive`); en el resto de formatos devuelve el `ggplot`
#' tal cual, que se dibuja en vectorial.
#'
#' @param plt Un `ggplot`.
#' @param h Altura en pulgadas. Por defecto, `5`.
#' @param w Anchura en pulgadas. Por defecto, `10`.
#' @param .fuente_letra Fuente del tooltip. Por defecto, `"Source Sans 3"`.
#' @param .oscuro Valor lógico. Tooltip con los colores del tema oscuro. Por defecto, `FALSE`.
#' @return Un objeto `girafe` en HTML; el `ggplot` en los demás formatos.
#' @export
mostrar_plt <- function(plt, h = 5, w = 10, .fuente_letra = "Source Sans 3", .oscuro = FALSE) {
  if (inherits(plt, "girafe") || inherits(plt, "htmlwidget")) return(plt)
  if (knitr::is_html_output()) {
    return(.widget_victorgm(plt, h, w, .fuente_letra, .oscuro))
  }
  plt
}

# ---------- Reglas automáticas de los gráficos ----------

.es_serie_temporal <- function(p) {
  if (inherits(p$coordinates, "CoordFlip")) return(FALSE)
  tryCatch({
    sx <- ggplot2::ggplot_build(p)$layout$panel_scales_x[[1]]
    if (inherits(sx, c("ScaleContinuousDate", "ScaleContinuousDatetime"))) return(TRUE)
    vx <- tryCatch(rlang::as_label(p$mapping$x), error = function(e) "")
    inherits(sx, "ScaleContinuous") && tolower(vx) %in% c("anio", "año", "ano", "year", "anyo", "periodo", "fecha")
  }, error = function(e) FALSE)
}

#' Poner el eje de valores a la derecha en los gráficos de evolución temporal
#'
#' Regla de estilo: en una serie temporal el último dato es el que importa, así que el eje `y` va a
#' la derecha, junto a él. Si el gráfico no es una serie temporal (eje `x` discreto, `coord_flip`),
#' se devuelve sin cambios.
#'
#' @param p Un `ggplot`.
#' @return El `ggplot`, con la escala `y` a la derecha si procede.
#' @export
eje_y_derecha <- function(p) {
  if (!.es_serie_temporal(p)) return(p)
  tryCatch(suppressMessages({
    sc <- p$scales$get_scales("y")
    if (is.null(sc)) {
      p + ggplot2::scale_y_continuous(position = "right")
    } else if (inherits(sc, "ScaleContinuous")) {
      sc2 <- sc$clone()
      sc2$position <- "right"
      p + sc2
    } else {
      p
    }
  }), error = function(e) p)
}

.limites_escala <- function(p, eje = "y") {
  tryCatch({
    b <- ggplot2::ggplot_build(p)
    if (isTRUE(p$facet$params$free[[eje]])) return(NULL)
    sc <- if (eje == "y") b$layout$panel_scales_y[[1]] else b$layout$panel_scales_x[[1]]
    if (!inherits(sc, "ScaleContinuous")) return(NULL)
    lim <- sc$get_limits()
    tr <- tryCatch(sc$get_transformation(), error = function(e) NULL)
    if (!is.null(tr)) lim <- tr$inverse(lim)
    lim
  }, error = function(e) NULL)
}

#' Igualar las escalas de dos gráficos comparables
#'
#' Dos gráficos que se leen por comparación (mismas unidades, mismo periodo) deben compartir el
#' rango de la escala. Amplía los límites de cada uno a la unión de los rangos de datos de ambos.
#'
#' @param p1,p2 Dos `ggplot`.
#' @param ejes Cadena de caracteres: `"y"` (por defecto), `"x"` o `"xy"`.
#' @return Una lista con los dos `ggplot` ajustados.
#' @export
igualar_escalas <- function(p1, p2, ejes = "y") {
  for (eje in strsplit(ejes, "")[[1]]) {
    l1 <- .limites_escala(p1, eje)
    l2 <- .limites_escala(p2, eje)
    if (is.null(l1) || is.null(l2)) {
      message("igualar_escalas: no se iguala el eje ", eje, " (facetas con escala libre o eje no continuo).")
      next
    }
    u <- range(c(l1, l2))
    if (eje == "y") {
      p1 <- p1 + ggplot2::expand_limits(y = u)
      p2 <- p2 + ggplot2::expand_limits(y = u)
    } else {
      p1 <- p1 + ggplot2::expand_limits(x = u)
      p2 <- p2 + ggplot2::expand_limits(x = u)
    }
  }
  list(p1, p2)
}

# ---------- Tira de cifras ----------

#' Una cifra clave para la tira de [kpis()]
#'
#' @param etiqueta Qué se mide (`"Exportaciones"`, `"Tasa de paro"`).
#' @param valor La cifra ya formateada (`"324.500 M€"`, `"11,2 %"`).
#' @param variacion Texto de la variación (`"+5,3 % interanual"`). Opcional.
#' @param signo Número cuyo signo decide el color de la variación: positivo en verde, negativo en granate. Opcional.
#' @param medida Aclaración breve que sale junto a la etiqueta en gris (`"12 m"`, `"mediana"`). Opcional.
#' @return Una lista que entiende [kpis()].
#' @export
kpi <- function(etiqueta, valor, variacion = NULL, signo = NULL, medida = NULL) {
  list(etiqueta = etiqueta, valor = valor, variacion = variacion, signo = signo, medida = medida)
}

#' Tira de cifras clave
#'
#' Fila de 3 a 6 cifras destacadas al comienzo de un informe o artículo, cada una creada con [kpi()].
#' Necesita el CSS de [css_informe_victorgm()].
#'
#' @param ... Objetos de [kpi()].
#' @return Un `shiny.tag` de htmltools.
#' @examples
#' kpis(
#'   kpi("Exportaciones", "324.500 M€", "+5,3 % interanual", signo = 5.3, medida = "12 m"),
#'   kpi("Importaciones", "380.100 M€", "-1,2 % interanual", signo = -1.2, medida = "12 m")
#' )
#' @export
kpis <- function(...) {
  k <- list(...)
  clase_var <- function(x) if (x >= 0) "sube" else "baja"
  htmltools::div(
    class = "kpis", style = sprintf("--n:%d", length(k)),
    lapply(k, function(m) {
      htmltools::div(
        class = "kpi",
        htmltools::div(
          class = "etiqueta", m$etiqueta,
          if (!is.null(m$medida)) htmltools::span(class = "medida", m$medida)
        ),
        htmltools::div(class = "valor", m$valor),
        if (!is.null(m$variacion)) {
          htmltools::div(class = paste("variacion", if (!is.null(m$signo)) clase_var(m$signo)), m$variacion)
        }
      )
    })
  )
}

# ---------- Resumen ejecutivo ----------

#' Un bloque del [resumen()] ejecutivo
#'
#' @param titulo Título corto del bloque.
#' @param clave Mensaje clave: una frase que se lee sola. Admite `<b>` e `<i>`.
#' @param texto Detalle. Admite `<b>` e `<i>`.
#' @param seccion Id de la sección a la que enlaza el título (`"sec-exportaciones"`). Opcional.
#' @param afirmacion Texto citado que sale con un filete vertical oro bajo el párrafo. Opcional.
#' @return Una lista que entiende [resumen()].
#' @export
bloque <- function(titulo, clave, texto, seccion = NULL, afirmacion = NULL) {
  list(titulo = titulo, clave = clave, texto = texto, seccion = seccion, afirmacion = afirmacion)
}

#' Resumen ejecutivo en tarjetas numeradas
#'
#' Tarjetas con el número en oro que enlazan a su sección. Con un número impar de tarjetas van en
#' una sola columna, para que no quede una caja suelta. Necesita el CSS de [css_informe_victorgm()].
#'
#' @param ... Objetos de [bloque()].
#' @return Un `shiny.tag` de htmltools.
#' @export
resumen <- function(...) {
  bl <- list(...)
  htmltools::div(
    class = paste("resumen", if (length(bl) %% 2 == 1) "una-columna"),
    lapply(seq_along(bl), function(i) {
      b <- bl[[i]]
      tit <- if (!is.null(b$seccion)) htmltools::tags$a(href = paste0("#", b$seccion), b$titulo) else b$titulo
      htmltools::div(
        class = "bloque",
        htmltools::tags$h2(htmltools::span(class = "num", sprintf("%02d", i)), tit),
        htmltools::div(class = "clave", htmltools::HTML(b$clave)),
        htmltools::tags$p(htmltools::HTML(b$texto)),
        if (!is.null(b$afirmacion)) htmltools::tags$p(class = "afirmacion", htmltools::HTML(b$afirmacion))
      )
    })
  )
}

# ---------- Figuras y tablas ----------

.rotulo_fila <- function(rotulo, tipo, n) {
  htmltools::div(
    class = "rotulo-fila",
    htmltools::span(class = "num-fig", paste0(tipo, " ", .informe$prefijo, n, ".")),
    htmltools::span(class = "rotulo", rotulo)
  )
}

.subtitulo_fila <- function(subtitulo) {
  if (is.null(subtitulo) || !nzchar(subtitulo)) return(NULL)
  htmltools::div(class = "subtitulo", subtitulo)
}

.pie_fuente <- function(fuente, enlace = NULL) {
  if (is.null(fuente) || !nzchar(fuente)) return(NULL)
  f <- sub("\\.$", "", sub("^Fuente: ?", "", fuente))
  htmltools::div(
    class = "fuente-pie",
    paste0("Fuente: ", f, "."),
    if (!is.null(enlace) && nzchar(enlace)) {
      htmltools::HTML(sprintf(" <a href=\"%s\" target=\"_blank\" rel=\"noopener\">Ver datos</a>.", enlace))
    }
  )
}

.titulo_gg <- function(p) tryCatch(ggplot2::get_labs(p)$title, error = function(e) p$labels$title)
.subtitulo_gg <- function(p) tryCatch(ggplot2::get_labs(p)$subtitle, error = function(e) p$labels$subtitle)

.caja_grafico <- function(p, rotulo, fuente, alto, ancho, nota, derecha, enlace, subtitulo, .fuente_letra, .oscuro) {
  if (is.null(rotulo)) rotulo <- .titulo_gg(p)
  if (is.null(subtitulo)) subtitulo <- .subtitulo_gg(p)
  if (isTRUE(derecha)) p <- eje_y_derecha(p)
  p_limpio <- p + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL)
  n <- .siguiente_numero("Gráfico")
  list(
    p_limpio = p_limpio,
    html = htmltools::div(
      class = "figura",
      .rotulo_fila(rotulo, "Gráfico", n),
      .subtitulo_fila(subtitulo),
      .widget_victorgm(p_limpio, alto, ancho, .fuente_letra, .oscuro),
      .pie_fuente(fuente, enlace),
      if (!is.null(nota) && nzchar(nota)) htmltools::div(class = "nota", nota)
    )
  )
}

#' Gráfico numerado en una caja con rótulo, fuente y nota
#'
#' Quita título, subtítulo y pie del `ggplot` (pasan al rótulo y a la fuente de la caja) y lo numera
#' (`"Gráfico N."`). En HTML se dibuja con ggiraph (tooltips si usa `geom_*_interactive`); en PDF o
#' Word devuelve el `ggplot` limpio. Necesita el CSS de [css_informe_victorgm()].
#'
#' El rótulo dice el mensaje (una frase con verbo); el subtítulo, qué se mide y en qué unidades.
#'
#' @param p Un `ggplot` (sin pasar por [graficos_estilo_victorgm()], que ya pone título y pie).
#' @param rotulo Texto del rótulo. Si es `NULL`, el título del `ggplot`.
#' @param fuente Fuente de los datos (`"INE"`, `"Banco de España"`). Se antepone `"Fuente: "`.
#' @param enlace URL a la que enlaza la fuente. Opcional.
#' @param nota Aclaración breve bajo la fuente. Opcional.
#' @param subtitulo Unidad y tipo de dato bajo el rótulo. Si es `NULL`, el subtítulo del `ggplot`.
#' @param alto,ancho Tamaño de dibujo en pulgadas (el ancho se ajusta al de la página).
#' @param derecha Valor lógico. Regla: en los gráficos de evolución temporal el eje `y` va a la derecha. Por defecto, `TRUE`.
#' @param .fuente_letra Fuente del tooltip. Por defecto, `"Source Sans 3"`.
#' @param .oscuro Valor lógico. Tooltip con los colores del tema oscuro. Por defecto, `FALSE`.
#' @return Un `shiny.tag` en HTML; el `ggplot` en los demás formatos.
#' @seealso [figura_doble()], [tabla()]
#' @export
figura <- function(p, rotulo = NULL, fuente = NULL, enlace = NULL, nota = NULL, subtitulo = NULL,
                   alto = 4.5, ancho = 9, derecha = TRUE, .fuente_letra = "Source Sans 3", .oscuro = FALSE) {
  c1 <- .caja_grafico(p, rotulo, fuente, alto, ancho, nota, derecha, enlace, subtitulo, .fuente_letra, .oscuro)
  if (!knitr::is_html_output()) return(c1$p_limpio)
  c1$html
}

#' Dos gráficos comparables, uno al lado del otro y con la misma escala
#'
#' Cuando dos gráficos consecutivos se leen por comparación (mismas unidades, mismo periodo o
#' mismas categorías) van en una fila de dos cajas y comparten el rango de la escala de valores
#' (`escala = "y"`; `"xy"` también iguala el eje `x`; `"no"` no iguala). Cada gráfico lleva su
#' número. En PDF o Word se devuelven juntos con `patchwork` si está instalado.
#'
#' @inheritParams figura
#' @param p1,p2 Dos `ggplot`.
#' @param rotulo1,rotulo2 Rótulos de cada gráfico.
#' @param fuente2 Fuente del segundo gráfico. Por defecto, la misma que `fuente`.
#' @param escala `"y"` (por defecto), `"x"`, `"xy"` o `"no"`.
#' @param enlace2 Enlace del segundo gráfico. Por defecto, el mismo que `enlace`.
#' @param subtitulo1,subtitulo2 Subtítulos de cada gráfico.
#' @return Un `shiny.tag` en HTML; un `patchwork` (o una lista de dos `ggplot`) en los demás formatos.
#' @export
figura_doble <- function(p1, p2, rotulo1 = NULL, rotulo2 = NULL, fuente = NULL, fuente2 = fuente,
                         enlace = NULL, enlace2 = enlace, nota = NULL, subtitulo1 = NULL, subtitulo2 = subtitulo1,
                         alto = 4, ancho = 5, escala = "y", derecha = TRUE,
                         .fuente_letra = "Source Sans 3", .oscuro = FALSE) {
  if (!identical(escala, "no")) {
    pp <- igualar_escalas(p1, p2, escala)
    p1 <- pp[[1]]
    p2 <- pp[[2]]
  }
  hay_nota <- !is.null(nota) && nzchar(nota)
  c1 <- .caja_grafico(p1, rotulo1, fuente, alto, ancho, if (hay_nota) nota, derecha, enlace, subtitulo1, .fuente_letra, .oscuro)
  c2 <- .caja_grafico(p2, rotulo2, fuente2, alto, ancho, if (hay_nota) "Nota: ver el gráfico de la izquierda.", derecha, enlace2, subtitulo2, .fuente_letra, .oscuro)
  if (!knitr::is_html_output()) {
    if (requireNamespace("patchwork", quietly = TRUE)) {
      return(patchwork::wrap_plots(c1$p_limpio, c2$p_limpio, nrow = 1))
    }
    return(list(c1$p_limpio, c2$p_limpio))
  }
  htmltools::div(class = "figuras-par", c1$html, c2$html)
}

#' Tabla gt numerada con rótulo y fuente
#'
#' Aplica a una tabla `gt` el estilo común (cabecera morada, filete oro, fuente de la web) y la
#' numera (`"Tabla N."`). En HTML la envuelve en la misma caja que [figura()]; en PDF o Word
#' devuelve la tabla `gt`.
#'
#' @inheritParams figura
#' @param gt_tbl Una tabla `gt`.
#' @param .fuente_letra Fuente de la tabla. Por defecto, `"Source Sans 3"`.
#' @return Un `shiny.tag` en HTML; la tabla `gt` en los demás formatos.
#' @export
tabla <- function(gt_tbl, rotulo = NULL, fuente = NULL, enlace = NULL, nota = NULL, subtitulo = NULL,
                  .fuente_letra = "Source Sans 3") {
  tk <- tokens_victorgm()
  gt_tbl <- gt_tbl |>
    gt::tab_options(
      table.font.names = .fuente_letra,
      table.font.size = gt::px(13),
      table.width = gt::pct(100),
      table.background.color = "transparent",
      table.border.top.style = "solid",
      table.border.top.width = gt::px(3),
      table.border.top.color = tk[["oro"]],
      column_labels.border.top.style = "solid",
      column_labels.border.top.width = gt::px(3),
      column_labels.border.top.color = tk[["oro"]],
      table.border.bottom.color = tk[["primario"]],
      column_labels.background.color = tk[["primario"]],
      column_labels.font.weight = "bold",
      container.overflow.x = FALSE,
      container.overflow.y = FALSE,
      data_row.padding = gt::px(4),
      row.striping.include_table_body = FALSE
    ) |>
    gt::tab_style(style = gt::cell_text(color = "white"), locations = gt::cells_column_labels())
  if (!knitr::is_html_output()) return(gt_tbl)
  n <- .siguiente_numero("Tabla")
  htmltools::div(
    class = "figura tabla",
    if (!is.null(rotulo)) .rotulo_fila(rotulo, "Tabla", n),
    .subtitulo_fila(subtitulo),
    htmltools::HTML(gt::as_raw_html(gt_tbl, inline_css = TRUE)),
    .pie_fuente(fuente, enlace),
    if (!is.null(nota) && nzchar(nota)) htmltools::div(class = "nota", nota)
  )
}
