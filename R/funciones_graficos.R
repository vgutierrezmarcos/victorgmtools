#Colores SG estudios ----
#' @export
colores_victorgm <- function() {
  c(
    "#BC9BFD", "#FDBF6F", "#B2DF8A",
    "#FB9A99", "#9BE6FD", "#FFF5B3",
    "#FF7F00", "#6A3D9A", "#75A3FF",
    "#E31A1C", "#18BC9C", "#2C3E50",
    "#FF7F8F", "#46AF4B", "#DEBA2F",
    "#7BA1D8", "#C0C0C0", "#965B96",
    "#FFC000", "#843C0C", "#A5A5A5",
    "#F8CBAD", "#ED7D31", "#421E06",
    "#800000", "#d00000", "#cfd4fc",
    "#576af4", "#081482", "#777777"
  )
}

#Colores SG estudios paired ----
#' @export
colores_victorgm_paired <- function() {
  c(
    "#FF7F00", "#FFCC99",
    "#6A3D9A", "#B799D7",
    "#1F78B4", "#AED6F1",
    "#E31A1C", "#F4A2A3",
    "#18BC9C", "#51E9CB",
    "#2C3E50", "#8AA4BE"
  )
}

colores_alternativos <- function() {
  c(
    "#9BE6FD", "#FFF5B3", "#F48383",
    "#3DCCAD", "#75A3FF", "#C0C0C0",
    "#BC9BFD", "#FFD885", "#81F39C",
    "#C0F9CE", "#004FEE", "#CAB2D6"
  )
}

# Función para aplicar paleta ----

#' Fijar estilo de gráfico estándar para VictorGM, con algunos parámetros opcionales
#' @export
aplicar_paleta_victorgm <- function(.x, .n_colors = NULL) {
  colores_victorgm <- victorgmtools::colores_victorgm()

  palette(colores_victorgm)

  pal <- palette(colores_victorgm)


  .plot_to_return_plt <-
    .x +
    ggplot2::scale_color_manual(values = colores_victorgm, aesthetics = c("colour", "fill"))

  if(!is.null(.n_colors)){
    .plot_to_return_plt <-
      .x +
      ggplot2::scale_color_manual(values = grDevices::colorRampPalette(pal)(.n_colors), aesthetics = c("colour", "fill"))
  }

  return(.plot_to_return_plt)
}

#Tema SG estudios ----
#' @export

tema_victorgm <- function() {
    ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(hjust = 1, angle = 45),
    axis.ticks = ggplot2::element_blank(),
    legend.key = ggplot2::element_rect(fill = NA, colour = NA),
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    panel.background = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major = ggplot2::element_line(colour = "#D9D9D9", lineend = "round", linewidth = 0.75, linetype = "dotted", arrow = FALSE),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    strip.background = ggplot2::element_rect(fill = "white")
  )
}

# Función general para dar formato a gráficos ----

#' Fijar estilo de gráfico estándar para victorgm, con algunos parámetros opcionales
#' @param .tipo_grafico_x Cadena de caracteres. Indica si se trata de una serie temporal o no. Puede tomar valores `"fecha"` o `"nofecha"`. Por defecto, `"fecha"`.
#' @param .tipo_grafico_y Cadena de caracteres. Sirve para indicar en ciertos casos la escala del eje y. Puede tomar valores `"millones"`, `"suffixing"`, `"porcentaje"` o `NULL`. Si es distinto a `NULL`, el resto de argumentos no se tienen en cuenta en la medida en que colisionen con los definidos para el tipo de gráfico. Por defecto, `NULL`.
#' @param .scale Número. Factor por el que dividir los números del eje. Por defecto, `1`. Para millones, p.ej., habría que fijar a 1e+6 o 1000000.
#' @param .accuracy Número. Número de decimales a mostrar en eje. Por defecto, `1`. Para mostrar 0 decimales, fijar a 1; para 1 decimal, fijar a 0.1; para 2, a 0.01 y así...
#' @param .suffix Cadena de caracteres. Por defecto, cadena vacía.
#' @param .minimo_eje_y Número. Valor mínimo en el eje. Por defecto, `NULL`` y el mínimo se adapta al gráfico
#' @param .maximo_eje_y Número. Valor máximo en el eje. Por defecto, `NULL` y el mínimo se adapta al gráfico
#' @param .date_labels Cadena de caracteres. Formato de las fechas en eje de abscisas. Por defecto, `"%Y"`. Para mostrar el año y mes abreviado, fijar a `"%Y-%b"`; para fijar sólo el número del año `"%Y"`. Para otros casos, ver documentación de `scale_x_date`.
#' @param .angle_x_axis_labels Número. Grado de inclinación de los datos en el eje horizontal. Sólo funciona con `.tipo_grafico_x = "fecha"`. Por defecto, `45`.
#' @param .date_breaks Cadena de caracteres. Frecuencia de las fechas en eje de abscisas. Por defecto, `"1 year"`. Para mostrar una fecha cada seis meses, fijar a `"6 months"`; para mostrar fechas cada 2 años, fijar a `"2 years"`.
#' @param .fecha_inicial_grafico Fecha. Sólo funciona con `.tipo_grafico_x = "fecha"`. Por defecto, `NULL`.
#' @param .fecha_final_grafico Fecha. Sólo funciona con `.tipo_grafico_x = "fecha"`. Por defecto, `NULL`.
#' @param .axis_position Cadena de caracteres. Posición del eje de ordenadas. Por defecto, `"right"`.
#' @param .legend_position Cadena de caracteres. Posición de la leyenda. Incluye la opción `"auto"`, que permite buscar un hueco "libre" dentro del gráfico y ponerlo en una esquina dentro del área de trazado. Por defecto, `"bottom"`.
#' @param .title_axis_size Número. Tamaño de la fuente en los títulos de los ejes. Sólo aplicable para `.tipo_grafico = "nofecha"`. Por defecto, `12`.
#' @param .text_axis_size Número. Tamaño de la fuente del texto de los ejes. Por defecto, `12`.
#' @param .legend_text_size Número. Tamaño de la fuente en la leyenda. Por defecto, `16`.
#' @param .text_size Número. Tamaño de la fuente en el resto del texto. Por defecto, `14`.
#' @param .nrows_legend_color Número. Número de filas a incluir en la leyenda. Por defecto, `3`.
#' @param .n_breaks_y Número. Número de breaks en el eje vertical. Por defecto, `NULL`.
#' @param .title_x_axis Título del eje x. Por defecto, `NULL`.
#' @param .title_y_axis Título del eje y. Por defecto, `NULL`.
#' @param .title Título del gráfico. Por defecto, `NULL`.
#' @param .subtitle Subtitulo del gráfico. Habitualmente usado para definir las unidades de medida. Por defecto, `NULL`.
#' @param .caption Nota al pie del gráfico. Por defecto, `NULL`.
#' @param .fuente_letra Cadena de caracteres. Fuente tipográfica a utilizar en el gráfico. Por defecto, utiliza la fuente: `"Segoe UI"`. Otras opciones comunes: `"Arial"`, `"Times"`, `"Courier"`, `"Helvetica"`, etc.
#' @export
graficos_estilo_victorgm <- function(
    .x,
    .tipo_grafico_x = "fecha",
    .tipo_grafico_y = NULL,
    .scale = 1,
    .accuracy = 1,
    .suffix = "",
    .minimo_eje_y = NULL,
    .maximo_eje_y = NULL,
    .date_labels = "%Y",
    .angle_x_axis_labels = 45,
    .date_breaks = "1 year",
    .fecha_inicial_grafico = NULL,
    .fecha_final_grafico = NULL,
    .axis_position = "right",
    .legend_position = "bottom",
    .title_axis_size = 12,
    .text_axis_size = 12,
    .legend_text_size = 16,
    .text_size = 14,
    .nrows_legend_color = 3,
    .n_breaks_y = NULL,
    .paleta_utilizada = NULL,
    .n_colors = NULL,
    .title_x_axis = NULL,
    .title_y_axis = NULL,
    .title = NULL,
    .title_size = 18,
    .subtitle = NULL,
    .subtitle_size = 14,
    .caption = NULL,
    .caption_size = 10,
    .fuente_letra = "Segoe UI"
) {

  # Función auxiliar para encontrar la mejor posición de leyenda automáticamente
  encontrar_mejor_posicion_leyenda <- function(plot_object) {
    tryCatch({
      # Construir el gráfico para extraer datos
      plot_built <- ggplot2::ggplot_build(plot_object)

      if(length(plot_built$data) == 0) {
        return(list(pos = "bottom", just = NULL))
      }

      # Extraer datos de todas las capas
      all_data <- do.call(rbind, lapply(plot_built$data, function(layer) {
        if("x" %in% names(layer) && "y" %in% names(layer)) {
          data.frame(x = layer$x, y = layer$y, stringsAsFactors = FALSE)
        } else {
          NULL
        }
      }))

      if(is.null(all_data) || nrow(all_data) == 0) {
        return(list(pos = "bottom", just = NULL))
      }

      # Remover valores NA
      all_data <- all_data[complete.cases(all_data), ]

      if(nrow(all_data) < 4) {
        return(list(pos = "bottom", just = NULL))
      }

      # Calcular límites
      x_min <- min(all_data$x, na.rm = TRUE)
      x_max <- max(all_data$x, na.rm = TRUE)
      x_medio <- x_min + (x_max - x_min) / 2

      y_min <- min(all_data$y, na.rm = TRUE)
      y_max <- max(all_data$y, na.rm = TRUE)
      y_medio <- y_min + (y_max - y_min) / 2

      # Clasificar puntos por cuadrantes
      cuadrantes <- all_data %>%
        dplyr::mutate(
          cuadrante = dplyr::case_when(
            x <= x_medio & y >= y_medio ~ "sup_izq",
            x > x_medio & y >= y_medio ~ "sup_der",
            x <= x_medio & y < y_medio ~ "inf_izq",
            x > x_medio & y < y_medio ~ "inf_der"
          )
        )

      # Contar densidad por cuadrante
      densidad_cuadrantes <- cuadrantes %>%
        dplyr::count(cuadrante, name = "puntos") %>%
        dplyr::arrange(puntos)

      # Encontrar cuadrante menos denso
      cuadrante_menos_denso <- densidad_cuadrantes$cuadrante[1]

      # Asignar posición según cuadrante menos denso
      posicion_leyenda <- switch(
        cuadrante_menos_denso,
        "sup_izq" = list(pos = c(0.02, 0.98), just = c(0, 1)),
        "sup_der" = list(pos = c(0.98, 0.98), just = c(1, 1)),
        "inf_izq" = list(pos = c(0.02, 0.02), just = c(0, 0)),
        "inf_der" = list(pos = c(0.98, 0.02), just = c(1, 0)),
        list(pos = "bottom", just = NULL)  # fallback
      )

      return(posicion_leyenda)

    }, error = function(e) {
      # En caso de error, usar posición por defecto
      return(list(pos = "bottom", just = NULL))
    })
  }

  # if (length(.paleta_utilizada) < .n_colors) {
  #   stop("La paleta de colores proporcionada tiene menos colores que el número requerido.")
  # }

  # Determinar posición de leyenda
  if(.legend_position == "auto") {
    posicion_auto <- encontrar_mejor_posicion_leyenda(.x)
    legend_pos <- posicion_auto$pos
    legend_just <- posicion_auto$just
  } else {
    legend_pos <- .legend_position
    legend_just <- NULL
  }

  .plot_to_return_plt <-
    .x +
    tema_victorgm() +
    ggplot2::theme(
      text = ggplot2::element_text(size = .text_size, family = .fuente_letra),
      legend.position = legend_pos,
      legend.text = ggplot2::element_text(size = .legend_text_size, family = .fuente_letra),
      title = ggplot2::element_text(family = .fuente_letra),
      plot.margin = ggplot2::margin(t = 5, r = 10, b = 5, l = 10),
      axis.text = ggplot2::element_text(size = .text_axis_size, family = .fuente_letra)
    ) +
    # ggplot2::guides(color = ggplot2::guide_legend(nrow = .nrows_legend_color, byrow = TRUE)) +
    # ggplot2::scale_color_manual(values = grDevices::colorRampPalette(pal)(.n_colors))
    ggplot2::guides(color = ggplot2::guide_legend(nrow = .nrows_legend_color, byrow = TRUE),
                    fill = ggplot2::guide_legend(nrow = .nrows_legend_color, byrow = TRUE))
  # ggplot2::scale_color_manual(values = .paleta_utilizada[1:.n_colors]) +
  # ggplot2::scale_fill_manual(values = .paleta_utilizada[1:.n_colors])

  # Aplicar justificación y estilo especial para leyenda automática
  if(.legend_position == "auto" && !is.null(legend_just)) {
    .plot_to_return_plt <- .plot_to_return_plt +
      ggplot2::theme(
        legend.justification = legend_just,
        legend.background = ggplot2::element_rect(
          fill = ggplot2::alpha("white", 0.9),
          colour = "white",
          size = 0.3
        ),
        legend.key.size = ggplot2::unit(0.4, "cm"),
        legend.margin = ggplot2::margin(t = 2, r = 3, b = 2, l = 3)
      )
  }

  suffix_definido <- .suffix
  scale_definido <- .scale
  accuracy_definido <- .accuracy

  # Función auxiliar para determinar el valor máximo del gráfico
  get_max_value <- function(plot_object, default_max = 1e6) {
    # Obtener datos construidos del gráfico
    plot_built <- try(ggplot2::ggplot_build(plot_object), silent = TRUE)

    if (inherits(plot_built, "try-error")) {
      return(default_max)
    }

    # Extraer todos los valores numéricos del eje Y
    y_values <- numeric()

    # Revisar todas las capas de datos
    if (!is.null(plot_built$data)) {
      for (layer_data in plot_built$data) {
        # Buscar columnas y, ymax, ymin y yend que suelen contener los valores del eje Y
        y_cols <- intersect(names(layer_data), c("y", "ymax", "ymin", "yend"))
        if (length(y_cols) > 0) {
          for (col in y_cols) {
            if (is.numeric(layer_data[[col]])) {
              y_values <- c(y_values, layer_data[[col]])
            }
          }
        }
      }
    }

    # Si encontramos valores, usar el máximo
    if (length(y_values) > 0 && max(y_values, na.rm = TRUE) > 0) {
      return(max(abs(y_values), na.rm = TRUE))
    } else {
      return(default_max)
    }
  }

  # Determinar escala automáticamente para tipo_grafico_y = "suffixing"
  if (length(.tipo_grafico_y) > 0 && .tipo_grafico_y == "suffixing") {
    # Determinar el máximo valor para elegir la escala apropiada
    if (!is.null(.maximo_eje_y)) {
      max_value <- .maximo_eje_y
    } else {
      max_value <- get_max_value(.x)
    }

    # Aplicar la escala adecuada según la magnitud
    if (max_value < 1e3) {
      suffix_definido <- ""
      scale_definido <- 1
    } else if (max_value < 1e6) {
      suffix_definido <- "K"
      scale_definido <- 1e-3
    } else if (max_value < 1e9) {
      suffix_definido <- "M"
      scale_definido <- 1e-6
    } else if (max_value < 1e12) {
      suffix_definido <- "MM"
      scale_definido <- 1e-9
    } else if (max_value < 1e15) {
      suffix_definido <- "B"
      scale_definido <- 1e-12
    }else{
      suffix_definido <- "T"
      scale_definido <- 1e-15
    }
  } else if (length(.tipo_grafico_y) > 0) {
    if (.tipo_grafico_y == "millones") {
      suffix_definido <- "M"
      scale_definido <- 1e-6
    } else if (.tipo_grafico_y == "porcentaje") {
      suffix_definido <- "%"
      scale_definido <- 1e2
    }
  }

  if (.tipo_grafico_x == "fecha") {
    if(is.null(.fecha_inicial_grafico) && is.null(.fecha_final_grafico)){
      limits_x <- NULL
    } else{
      limits_x <- c(.fecha_inicial_grafico, .fecha_final_grafico)
    }

    if(!is.null(.minimo_eje_y) & !is.null(.maximo_eje_y)) {
      .plot_to_return_plt <-
        .plot_to_return_plt +
        ggplot2::scale_x_date(
          limits = limits_x,
          date_labels = .date_labels,
          date_breaks = .date_breaks,
          guide = guide_axis(angle = .angle_x_axis_labels)
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::number_format(
            accuracy = accuracy_definido,
            scale = scale_definido,
            suffix = suffix_definido,
            big.mark = ".",
            decimal.mark = ","
          ),
          limits = c(.minimo_eje_y, .maximo_eje_y),
          position = .axis_position,
          n.breaks = .n_breaks_y
        )
    } else {
      .plot_to_return_plt <-
        .plot_to_return_plt +
        ggplot2::scale_x_date(
          limits = limits_x,
          date_labels = .date_labels,
          date_breaks = .date_breaks,
          guide = guide_axis(angle = .angle_x_axis_labels)
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::number_format(
            accuracy = accuracy_definido,
            scale = scale_definido,
            suffix = suffix_definido,
            big.mark = ".",
            decimal.mark = ","
          ),
          position = .axis_position,
          n.breaks = .n_breaks_y
        )
    }
  } else if (.tipo_grafico_x == "nofecha") {
    if(!is.null(.minimo_eje_y) & !is.null(.maximo_eje_y)) {
      .plot_to_return_plt <-
        .plot_to_return_plt +
        ggplot2::scale_y_continuous(
          labels = scales::number_format(
            accuracy = accuracy_definido,
            scale = scale_definido,
            suffix = suffix_definido,
            big.mark = ".",
            decimal.mark = ","
          ),
          limits = c(.minimo_eje_y, .maximo_eje_y),
          position = .axis_position,
          n.breaks = .n_breaks_y
        ) +
        ggplot2::theme(
          panel.grid.major.x = ggplot2::element_line(colour = "#D9D9D9",
                                                     lineend = "round",
                                                     linewidth = 0.75, linetype = "dotted", arrow = FALSE)
        )
    } else {
      .plot_to_return_plt <-
        .plot_to_return_plt +
        ggplot2::scale_y_continuous(
          labels = scales::number_format(
            accuracy = accuracy_definido,
            scale = scale_definido,
            suffix = suffix_definido,
            big.mark = ".",
            decimal.mark = ","
          ),
          position = .axis_position,
          n.breaks = .n_breaks_y
        ) +
        ggplot2::theme(
          panel.grid.major.x = ggplot2::element_line(colour = "#D9D9D9",
                                                     lineend = "round",
                                                     linewidth = 0.75, linetype = "dotted", arrow = FALSE)
        )
    }
  }

  if (is.null(.paleta_utilizada)){
    .plot_to_return_plt <-
      .plot_to_return_plt |>
      aplicar_paleta_victorgm(.n_colors = .n_colors)
  } else if(is.null(.n_colors)){
    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::scale_color_manual(values = .paleta_utilizada, aesthetics = c("colour", "fill"))
  } else{
    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::scale_color_manual(values = grDevices::colorRampPalette(.paleta_utilizada)(.n_colors), aesthetics = c("colour", "fill"))
  }

  if(!is.null(.title)){
    titulo_formateado <- stringr::str_wrap(.title, width = 50)

    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::labs(title = titulo_formateado) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = .title_size, face = "bold", margin = ggplot2::margin(b = 5), family = .fuente_letra))
  }

  if(!is.null(.subtitle)){
    subtitulo_formateado <- stringr::str_wrap(.subtitle, width = 50)

    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::labs(subtitle = subtitulo_formateado) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(size = .subtitle_size, family = .fuente_letra, face = "italic", margin = ggplot2::margin(b = 10)))
  }

  if(!is.null(.caption)){
    caption_formateado <- stringr::str_wrap(.caption, width = 80)

    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::labs(caption = caption_formateado) +
      ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, family = .fuente_letra, size = .caption_size, color = "gray50"))
  }

  if(is.null(.title_x_axis)){
    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::theme(axis.title.x = ggplot2::element_blank())
  } else {
    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::labs(x = .title_x_axis) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(family = .fuente_letra, size = .title_axis_size))
  }

  if(is.null(.title_y_axis)){
    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::theme(axis.title.y = ggplot2::element_blank())
  } else {
    .plot_to_return_plt <-
      .plot_to_return_plt +
      ggplot2::labs(y = .title_y_axis) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(family = .fuente_letra, size = .title_axis_size))
  }

  # Configurar update_geom_defaults para que geom_text use la fuente por defecto
  ggplot2::update_geom_defaults("text", list(family = .fuente_letra))
  ggplot2::update_geom_defaults("label", list(family = .fuente_letra))

  return(.plot_to_return_plt)
}

# Funciones necesarias para mostrar gráficos interactivos y tablas con gt ----

#' @param .plot Objeto ggplot a mostrar
#' @param .width Número. Ancho del gráfico a mostrar en pulgadas. Por defecto, `6`.
#' @param .height Número. Alto del gráfico a mostrar en pulgadas. Por defecto, `5`.
#' @export
to_giraph <- function(.plot,
                      .width = 6,
                      .height = 5) {
  ggiraph::girafe(
    ggobj = .plot,
    width_svg = .width,
    height_svg = .height,
    options = list(
      # ggiraph::opts_tooltip(css = ''),
      ggiraph::opts_hover(css = ''), ## CSS code of line we're hovering over
      ggiraph::opts_hover_inv(
        css = "opacity:0.4;"
        ), ## CSS code of all other lines
      ggiraph::opts_sizing(rescale = FALSE), ## Fixes sizes to dimensions below
      position = "topright",
      saveaspng = TRUE
                  )
  )

}


# estatico ---
#' @param .plot Objeto ggplot a mostrar
#' @param .width Número. Ancho del gráfico a mostrar en pulgadas. Por defecto, `6`.
#' @param .height Número. Alto del gráfico a mostrar en pulgadas. Por defecto, `5`.
#' @export
estatico <- function(.plot,
                     .estatico,
                     .width = 6,
                     .height = 5) {

  .plot_return <- .plot

  if(!.estatico) {
    .plot_return <-
      .plot_return |>
      victorgmtools::to_giraph(.width = .width, .height = .height) |>
      ggiraph::girafe_options(
        ggiraph::opts_sizing(
          rescale = TRUE,
          width = 1
          )
      )

    return(.plot_return)
  }
  return(.plot_return)
}

#' Muestra un objeto ggplot o gt, si existe en memoria, y con contenido dinámico si disponible. Además, si se está renderizando un pdf, hace una captura de la tabla gt con webshot2 y la muestra.
#' @param .x Objeto ggplot o gt a mostrar
#' @param .estatico Lógico. Si el objeto es un objeto ggplot, permite que se muestre como un gráfico interactivo en caso de establecerse en `FALSE`. Por defecto, `FALSE`.
#' @param .width Número. Ancho del gráfico a mostrar en pulgadas. Por defecto, `6`.
#' @param .height Número. Alto del gráfico a mostrar en pulgadas. Por defecto, `5`.
#' @export
mostrar <- function(.x,
                    .estatico = FALSE,
                    .width = 6,
                    .height = 5) {
  object_name <- deparse(substitute(.x))
  x <- tryCatch({
    objeto <- eval(substitute(.x))
    if(inherits(objeto, "ggplot")) {
      return(objeto |>
               estatico(
                 .estatico = .estatico,
                 .width = .width,
                 .height = .height
                 )
             )
    } else if(inherits(objeto, "gt_tbl")) {
      if (knitr::is_html_output()){
        return(objeto)
      } else if (knitr::is_latex_output()) {
          objeto <-
            objeto |>
            gt::as_latex()

          return(objeto)
      } else {
        return(objeto)
      }
    }
  },
  error = function(e) {
    message(sprintf("Cannot find object passed as argument"))
    return(NULL)
  })

  return(x)
}

# Función general para dar formato a mapas coropléticos ----
#' Fijar estilo de mapas coropléticos para victorgm, con algunos parámetros opcionales. Habría que añadir el título al gráfico generado.
#' @param .x Data frame que contenga 2 columnas: `.col_nombres` y `.col_valores` con los valores a representar.
#' @param .region_geografica Cadena de caracteres. Puede tomar valores `"España"` o `"Mundo"`. Por defecto, `"España"`.
#' @param .unidades_territoriales Cadena de caracteres. Solo funciona en caso de que `.region_geografica == "España"`, ya que en caso contrario se representara por países. Puede tomar valores `"pais"`, `"ccaa"` o `"provincia"`. Por defecto, `"provincia"`.
#' @param .col_nombres Cadena de caracteres. Sirve para indicar el nombre de la columna que contiene las áreas geográficas. En caso de `.unidades_territoriales == "provincias"` y `.unidades_territoriales == "ccaa"` se espera que introduzca un df con los códigos por provincias o de las comunidades de datacomex. En caso de `.unidades_territoriales == "pais"` se utilizará el código iso3A (se puede cambiar en el parámetro `.codigo_pais`). Por defecto, `"nombres"`.
#' @param .col_valores Cadena de caracteres. Sirve para indicar el nombre de la columna que contiene los valores a representar. Por defecto, `"valores"`.
#' @param .codigo_pais Cadena de caracteres. Solo funciona en caso de que las unidades geográficas sean países. Puede tomar valores `"iso3A"` (si se usa el código iso a 3 letras), `"iso_code"` (si se usa el código iso a 3 dígitos) o `"datacomex"` (si se usa el código de datacomex). Por defecto, `"iso3A"`.
#' @param .color_negativo Cadena de caracteres. Color que representará los valores negativos. Por defecto, `"#E31A1CFF"`, rojo de la paleta de victorgm.
#' @param .color_positivo Cadena de caracteres. Color que representará los valores positivos. Por defecto, `"#18BC9CFF"`, verde de la paleta de victorgm.
#' @param .color_neutro Cadena de caracteres. Color que representará el nivel neutro. Por defecto, `"white"`.
#' @param .nivel_neutro Número. Cuantía que se representará con el color neutro en la escala de 3 colores. Por defecto, `0`.
#' @param .color_na Cadena de caracteres. Color que representará a las unidades territoriales con `NA`. Por defecto, `"grey50"`.
#' @param .resaltar Cadena de caracteres o vector. Permite establecer una o varias unidades territoriales a destacar en el mapa mediante sombras. Se establecerán con el código que aparezca en el dataframe `.x`. Por defecto, `NULL`.
#' @param .scale Número. Factor por el que multiplicar los números del dataframe Por defecto, `1`. Para millones, p.ej., habría que fijar a 1e+6 o 1000000.
#' @param .accuracy Número. Número de decimales a mostrar en el interactivo (en caso de que se represente la versión interactiva). Para mostrar 0 decimales, fijar a `1`; para 1 decimal, fijar a `0.1`; para 2, a `0.01` y así sucesivamente. Por defecto, `0.01` (i.e. 2 decimales).
#' @param .suffix Cadena de caracteres. Por ejemplo, en caso de porcentaje, establecer `" %"` para que aparezca es sufijo tanto en la leyenda como en el interactivo (en su caso). Por defecto, cadena vacía.
#' @param .title Título del mapa Por defecto, `NULL`.
#' @param .subtitle Subtitulo del mapa. Habitualmente usado para definir las unidades de medida. Por defecto, `NULL`.
#' @param .caption Nota al pie del mapa. Por defecto, `NULL`.
#' @param .fuente_letra Cadena de caracteres. Fuente tipográfica a utilizar en el gráfico. Por defecto, utiliza la fuente: `"Segoe UI"`. Otras opciones comunes: `"Arial"`, `"Times"`, `"Courier"`, `"Helvetica"`, etc.
#' @export
mapa_estilo_victorgm <- function(
    .x,
    .region_geografica = "España",
    .unidades_territoriales = "provincia",
    .col_nombres = "nombres",
    .col_valores = "valores",
    .codigo_pais = "iso3A",
    .color_negativo = "#E31A1C",
    .color_positivo = "#18BC9C",
    .color_neutro = "white",
    .nivel_neutro = 0,
    .color_na = "grey50",
    .resaltar = NULL,
    .scale = 1,
    .accuracy = 0.01,
    .suffix = "",
    .title = NULL,
    .title_size = 18,
    .subtitle = NULL,
    .subtitle_size = 14,
    .caption = NULL,
    .caption_size = 10,
    .fuente_letra = "Segoe UI"
) {
  if(.region_geografica != "España"){
    .unidades_territoriales  <- "pais"
  }

  if(.region_geografica == "España" && .unidades_territoriales == "provincia"){
    .x <-
      .x |>
      dplyr::rename(provincia = !!rlang::sym(.col_nombres)) |>
      dplyr::left_join(datacomexr::get_provincias_metadata(), by = "provincia") |>
      dplyr::rename(nombres = nombre_provincia) |>
      dplyr::mutate(
        nombres = dplyr::case_when(
          nombres == "Araba/Alava" ~ "Álava",
          nombres == "Balears, Illes" ~ "Baleares",
          nombres == "Coruña, A" ~ "La Coruña",
          nombres == "Girona" ~ "Gerona",
          nombres == "Lleida" ~ "Lérida",
          nombres == "Rioja, La" ~ "La Rioja",
          nombres == "Ourense" ~ "Orense",
          nombres == "Palmas, Las" ~ "Las Palmas",
          TRUE ~ nombres
        ))

    espana_sf <-
      rnaturalearth::ne_states(country = "spain", returnclass = "sf")

    espana_datos <-
      merge(espana_sf, .x,
            by.x = "name",
            by.y = "nombres")

    espana_datos <-
      espana_datos |>
      dplyr::mutate(
        name = dplyr::case_when(
          name == "Álava" ~ "Araba/Alava",
          name == "Baleares" ~ "Balears, Illes",
          name == "La Coruña" ~ "A Coruña",
          name == "Gerona" ~ "Girona",
          name == "Lérida" ~ "Lleida",
          name == "La Rioja" ~ "Rioja, La",
          name == "Orense" ~ "Ourense",
          name == "Las Palmas" ~ "Palmas, Las",
          TRUE ~ name
        )
      )

    canarias <-
      espana_datos[grepl("Canarias", espana_datos$nombre_comunidad), ] |>
      sf::st_transform(4326)
    peninsula <-
      espana_datos[!grepl("Canarias", espana_datos$nombre_comunidad), ] |>
      sf::st_transform(4326)

    shift_geometry <- function(sf_obj, x_shift = 7, y_shift = 6) {
      geom <- sf::st_geometry(sf_obj)
      new_geom <- geom + c(x_shift, y_shift)
      sf::st_set_crs(new_geom, sf::st_crs(geom))
    }

    canarias_shifted <- canarias
    sf::st_geometry(canarias_shifted) <- shift_geometry(canarias, 7, 6)

    sf::st_crs(canarias_shifted) <- sf::st_crs(peninsula)

    .x <- rbind(peninsula, canarias_shifted)
  } else if(.region_geografica == "España" && .unidades_territoriales == "ccaa"){
    .x <-
      .x |>
      dplyr::rename(comunidad = !!rlang::sym(.col_nombres)) |>
      dplyr::left_join(datacomexr::get_provincias_metadata(), by = "comunidad") |>
      dplyr::rename(nombres = nombre_comunidad) |>
      dplyr::mutate(
        nombres = dplyr::case_when(
          nombres == "Asturias, Principado de" ~ "Asturias",
          nombres == "Canarias" ~ "Canary Is.",
          nombres == "Navarra, Comunidad Foral de" ~ "Foral de Navarra",
          nombres == "Balears, Illes" ~ "Islas Baleares",
          nombres == "Rioja, La" ~ "La Rioja",
          nombres == "Madrid, Comunidad de" ~ "Madrid",
          nombres == "Murcia, Región de" ~ "Murcia",
          nombres == "Comunitat Valenciana" ~ "Valenciana",
          TRUE ~ nombres
        ))

    espana_sf <-
      rnaturalearth::ne_states(country = "spain", returnclass = "sf") |>
      dplyr::group_by(region) |>
      dplyr::summarise(geometry = sf::st_union(geometry)) |>
      dplyr::ungroup() |>
      dplyr::rename(name = region)

    espana_datos <-
      merge(espana_sf, .x,
            by.x = "name",
            by.y = "nombres")

    espana_datos <-
      espana_datos |>
      dplyr::mutate(
        name = dplyr::case_when(
          name == "Asturias" ~ "Asturias, Principado de",
          name == "Canary Is." ~ "Canarias",
          name == "Foral de Navarra" ~ "Navarra, Comunidad Foral de",
          name == "Islas Baleares" ~ "Balears, Illes",
          name == "La Rioja" ~ "Rioja, La",
          name == "Madrid" ~ "Madrid, Comunidad de",
          name == "Murcia" ~ "Murcia, Región de",
          name == "Valenciana" ~ "Comunitat Valenciana",
          TRUE ~ name
        )
      )

    canarias <-
      espana_datos[grepl("Canarias", espana_datos$nombre_comunidad), ] |>
      sf::st_transform(4326)
    peninsula <-
      espana_datos[!grepl("Canarias", espana_datos$nombre_comunidad), ] |>
      sf::st_transform(4326)

    shift_geometry <- function(sf_obj, x_shift = 7, y_shift = 6) {
      geom <- sf::st_geometry(sf_obj)
      new_geom <- geom + c(x_shift, y_shift)
      sf::st_set_crs(new_geom, sf::st_crs(geom))
    }

    canarias_shifted <- canarias
    sf::st_geometry(canarias_shifted) <- shift_geometry(canarias, 7, 6)

    sf::st_crs(canarias_shifted) <- sf::st_crs(peninsula)

    .x <- rbind(peninsula, canarias_shifted)
  } else if(.region_geografica == "Mundo"){
    if(.codigo_pais == "iso3A"){
      .x <-
        .x |>
        dplyr::rename(iso3A = !!rlang::sym(.col_nombres)) |>
        dplyr::left_join(wtor::get_partner_economies(), by = "iso3A") |>
        dplyr::rename(nombres = iso3A)
    }
    else if(.codigo_pais == "iso_code"){
      .x <-
        .x |>
        dplyr::rename(iso_code = !!rlang::sym(.col_nombres)) |>
        dplyr::left_join(datacomexr::get_iso_datacomex(), by = "iso_code") |>
        dplyr::select(-nombre, -iso_code, -datacomex) |>
        dplyr::left_join(wtor::get_partner_economies(), by = "iso3A") |>
        dplyr::rename(nombres = iso3A)
    } else if (.codigo_pais == "datacomex"){
      .x <-
        .x |>
        dplyr::rename(datacomex = !!rlang::sym(.col_nombres)) |>
        dplyr::left_join(datacomexr::get_iso_datacomex(), by = "datacomex") |>
        dplyr::select(-nombre, -iso_code, -datacomex) |>
        dplyr::left_join(wtor::get_partner_economies(), by = "iso3A") |>
        dplyr::rename(nombres = iso3A)
    }

    mundo_sf <-
      rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
      dplyr::mutate(iso_a3 = dplyr::case_when(iso_a3 != -99 ~ iso_a3,
                                              TRUE ~ adm0_a3)) |>
      dplyr::select(iso_a3) |>
      dplyr::rename(name = iso_a3)


    mundo_datos <-
      merge(mundo_sf, .x,
            by.x = "name",
            by.y = "nombres")

    .x <-
      mundo_datos |>
      dplyr::rename(iso3A = name) |>
      dplyr::left_join(wtor::get_partner_economies(lang = "3"), by = "iso3A")
  }

  .map_to_return <-
    ggplot2::ggplot(data = .x) +
    ggiraph::geom_sf_interactive(ggplot2::aes(fill = .data[[.col_valores]],
                                              tooltip = paste0(name, "\n",
                                                               scales::number_format(scale = .scale, accuracy = .accuracy, suffix = .suffix, big.mark = ".", decimal.mark = ",")(.data[[.col_valores]]))))

  if(!is.null(.resaltar)){
    if(.region_geografica == "Mundo"){
      if(.codigo_pais == "iso_3A"){
        .paises <-
          datacomexr::get_iso_datacomex() |>
          dplyr::select(
            codigo = iso3A,
            nombre = nombre
          )

        .resaltar_name <-
          .paises |>
          dplyr::filter(codigo %in% .resaltar) |>
          dplyr::pull(nombre)
      } else if(.codigo_pais == "iso_code"){
        .paises <-
          datacomexr::get_iso_datacomex() |>
          dplyr::select(
            codigo = iso_code,
            nombre = nombre
          )

        .resaltar_name <-
          .paises |>
          dplyr::filter(codigo %in% .resaltar) |>
          dplyr::pull(nombre)
      } else if(.codigo_pais == "datacomex"){
        .paises <-
          datacomexr::get_iso_datacomex() |>
          dplyr::select(
            codigo = datacomex,
            nombre = nombre
          )

        .resaltar_name <-
          .paises |>
          dplyr::filter(codigo %in% .resaltar) |>
          dplyr::pull(nombre)
      } else{
        .resaltar_name <- NULL
      }
    }else if(.region_geografica == "España" && .unidades_territoriales == "ccaa"){
      .ccaa <-
        datacomexr::get_provincias_metadata() |>
        dplyr::select(
          codigo = comunidad,
          nombre = nombre_comunidad
        )

      .resaltar_name <-
        .ccaa |>
        dplyr::filter(codigo %in% .resaltar) |>
        dplyr::pull(nombre)
    }else if(.region_geografica == "España" && .unidades_territoriales == "provincia"){
      .provincias <-
        datacomexr::get_provincias_metadata() |>
        dplyr::select(
          codigo = provincia,
          nombre = nombre_provincia
        )

      .resaltar_name <-
        .provincias |>
        dplyr::filter(codigo %in% .resaltar) |>
        dplyr::pull(nombre)
    }

    .map_to_return <-
      .map_to_return +
      ggfx::with_shadow(
        ggiraph::geom_sf_interactive(
          data = .x |> dplyr::filter(name == .resaltar_name),
          mapping = ggplot2::aes(fill = .data[[.col_valores]],
                                 tooltip = paste0(name, "\n",
                                                  scales::number_format(scale = .scale, accuracy = .accuracy, suffix = .suffix, big.mark = ".", decimal.mark = ",")(.data[[.col_valores]]))),
          color = "white",
          linewidth = 0.8),
        sigma = 0,
        x_offset = 4,
        y_offset = 4
      )
  }


  .map_to_return <-
    .map_to_return +
    ggplot2::scale_fill_gradient2(
      name = "",
      low = .color_negativo,
      mid = .color_neutro,
      high = .color_positivo,
      na.value = .color_na,
      midpoint = .nivel_neutro,
      labels = scales::label_number(
        scale = .scale,
        big.mark = ".",
        decimal.mark = ",",
        suffix = .suffix
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggtext::element_textbox_simple(family = .fuente_letra),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, family = .fuente_letra),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )

  if(.region_geografica == "España"){
    .map_to_return <-
      .map_to_return +
      ggplot2::geom_rect(
        ggplot2::aes(xmin = -11.3, xmax = -6,
                     ymin = 33.5, ymax = 35.5),
        fill = NA,
        color = "darkgrey",
        linewidth = 0.25)
  }

  if(!is.null(.title)){
    .map_to_return <-
      .map_to_return +
      ggplot2::labs(title = stringr::str_wrap(.title, width = 50)) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 18, family = .fuente_letra, face = "bold", margin = ggplot2::margin(b = 5)))
  }

  if(!is.null(.subtitle)){
    .map_to_return <-
      .map_to_return +
      ggplot2::labs(subtitle = .subtitle) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(size = .subtitle_size, family = .fuente_letra, face = "italic", margin = ggplot2::margin(b = 10)))
  }

  if(!is.null(.caption)){
    .map_to_return <-
      .map_to_return +
      ggplot2::labs(caption = .caption) +
      ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, family = .fuente_letra, size = .caption_size, color = "gray50"))
  }

  # Configurar update_geom_defaults para que geom_text use la fuente por defecto
  ggplot2::update_geom_defaults("text", list(family = .fuente_letra))
  ggplot2::update_geom_defaults("label", list(family = .fuente_letra))

  return(.map_to_return)
}

# Función general para dar formato a tablas ----

#' Función para aplicar estilo de tabla estándar para victorgm
#'
#' @param .data Data frame con los datos a mostrar en la tabla
#' @param .title Cadena de caracteres. Título principal de la tabla. Por defecto, `NULL`.
#' @param .subtitle Cadena de caracteres. Subtítulo de la tabla. Por defecto, `NULL`.
#' @param .caption Cadena de caracteres. Nota al pie de la tabla. Por defecto, `NULL`.
#' @param .columnas_numericas Vector de caracteres. Nombres de las columnas que contienen valores numéricos. Por defecto, detecta automáticamente.
#' @param .columnas_porcentaje Vector de caracteres. Nombres de las columnas que representan porcentajes. Por defecto, `NULL`.
#' @param .columnas_colores_positivo_negativo Vector de caracteres. Columnas donde aplicar colores según valores positivos/negativos. Por defecto, `NULL`.
#' @param .color_positivo Cadena de caracteres. Color para valores positivos. Por defecto, `"#38A169"` (verde).
#' @param .color_negativo Cadena de caracteres. Color para valores negativos. Por defecto, `"#E53E3E"` (rojo).
#' @param .color_fondo_positivo Cadena de caracteres. Color de fondo para valores positivos. Por defecto, `"#F0FFF4"`.
#' @param .color_fondo_negativo Cadena de caracteres. Color de fondo para valores negativos. Por defecto, `"#FFF5F5"`.
#' @param .columnas_separador_derecha Vector de caracteres. Columnas después de las cuales añadir un separador vertical. Por defecto, `NULL`.
#' @param .color_encabezado Cadena de caracteres. Color de fondo de los encabezados. Por defecto, `"#2B3A67"`.
#' @param .color_texto_encabezado Cadena de caracteres. Color del texto de los encabezados. Por defecto, `"#FFFFFF"`.
#' @param .color_borde_encabezado Cadena de caracteres. Color del borde inferior de los encabezados. Por defecto, `"#FCD757"`.
#' @param .color_filas_alternas Cadena de caracteres. Color de fondo para filas alternas. Por defecto, `"#F7F9FC"`.
#' @param .precision_decimales Número. Decimales para valores numéricos. Por defecto, `2`.
#' @param .formato_miles Cadena de caracteres. Separador de miles. Por defecto, `"."`.
#' @param .formato_decimal Cadena de caracteres. Separador decimal. Por defecto, `","`.
#' @param .font_size Cadena de caracteres. Tamaño de fuente para toda la tabla. Por defecto, `14`.
#' @param .fuente_letra Cadena de caracteres. Fuente tipográfica. Por defecto, `"Segoe UI"`.
#' @param .padding_celdas Número. Espaciado interno de las celdas en píxeles. Por defecto, `10`.
#' @param .agrupaciones Lista con agrupaciones de columnas. Por defecto, `NULL`.
#' @param .ancho_tabla Cadena de caracteres. Ancho de la tabla. Por defecto, `"100%"`.
#' @param .centrar_columnas Vector de caracteres. Columnas a centrar. Por defecto, columnas numéricas.
#' @param .primera_columna_fija Lógico. Si TRUE, mantiene la primera columna como etiqueta fija. Por defecto, `TRUE`.
#' @param .resaltar_filas Vector numérico. Índices de filas a resaltar. Por defecto, `NULL`.
#' @param .color_resaltado Cadena de caracteres. Color de fondo para filas resaltadas. Por defecto, `"#FFFACD"`.
#'
#' @examples
#' # Ejemplo básico con datos de comercio exterior
#' datos_comercio <- data.frame(
#'   Concepto = c("Exportaciones", "Importaciones", "Saldo"),
#'   Mensual_Jun_2025 = c(33.77, 37.36, -3.59),
#'   Var_Jun_2025 = c(2.42, 10.91, -2.88),
#'   Mensual_May_2025 = c(35.00, 37.54, -2.54),
#'   Var_May_2025 = c(0.82, 1.29, -194.09),
#'   Acumulado_12_meses = c(386.51, 436.08, -49.57),
#'   Var_Acumulado = c(2.02, 4.12, -9.60)
#' )
#'
#' # Uso básico
#' tabla_estilo_victorgm(
#'   .data = datos_comercio,
#'   .title = "España - Comercio Exterior",
#'   .subtitle = "Millones de euros y variaciones interanuales (%)",
#'   .caption = "Fuente: DataComex. Elaboración: victorgm"
#' )
#'
#' @export
tablas_estilo_victorgm <- function(
    .data,
    .title = NULL,
    .subtitle = NULL,
    .caption = NULL,
    .title_size = 18,
    .subtitle_size = 14,
    .caption_size = 12,
    .columnas_numericas = NULL,
    .columnas_variacion = NULL,
    .forzar_signo_variacion = TRUE,
    .columnas_cuota = NULL,
    .columnas_colores_positivo_negativo = NULL,
    .color_positivo = "#38A169",
    .color_negativo = "#E53E3E",
    .color_fondo_positivo = "#F0FFF4",
    .color_fondo_negativo = "#FFF5F5",
    .columnas_separador_derecha = NULL,
    .color_encabezado = "#2B3A67",
    .color_texto_encabezado = "#FFFFFF",
    .color_borde_encabezado = "#FCD757",
    .color_filas_alternas = "#F7F9FC",
    .precision_decimales = 2,
    .formato_miles = ".",
    .formato_decimal = ",",
    .font_size = 14,
    .fuente_letra = "Segoe UI",
    .padding_celdas = 10,
    .agrupaciones = NULL,
    .ancho_tabla = "100%",
    .centrar_columnas = NULL,
    .primera_columna_fija = TRUE,
    .resaltar_filas = NULL,
    .color_resaltado = "#FFFACD",
    .usar_google_fonts = FALSE  # NUEVO PARÁMETRO
) {

  # Cargar librerías necesarias
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("El paquete 'gt' es necesario. Por favor, instálalo con: install.packages('gt')")
  }

  # Validación de entrada
  if (!is.data.frame(.data)) {
    stop("El argumento .data debe ser un data frame")
  }

  if (nrow(.data) == 0) {
    warning("El data frame está vacío")
    return(gt::gt(.data))
  }

  # Detectar automáticamente columnas numéricas si no se especifican
  if (is.null(.columnas_numericas)) {
    .columnas_numericas <- names(.data)[sapply(.data, is.numeric)]
  }

  # Centrar columnas numéricas por defecto si no se especifica
  if (is.null(.centrar_columnas)) {
    .centrar_columnas <- .columnas_numericas
  }

  # Crear tabla base
  .tabla <- gt::gt(.data) |>
    # Configuración general
    gt::tab_options(
      table.border.top.style = "hidden",
      table.border.bottom.style = "hidden",
      table_body.border.bottom.style = "hidden",
      data_row.padding = gt::px(.padding_celdas),
      table.width = .ancho_tabla,
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(3),
      column_labels.border.bottom.color = .color_borde_encabezado
    )

  # CONFIGURAR FUENTES DE FORMA SEGURA
  if (.usar_google_fonts) {
    # Intentar Google Fonts con fallback
    tryCatch({
      .tabla <- .tabla |>
        gt::opt_table_font(
          font = list(
            gt::google_font(.fuente_letra),
            .fuente_letra,
            "system-ui",
            "sans-serif"
          ),
          size = paste0(.font_size, "px")
        )
    }, error = function(e) {
      warning("No se pudo cargar Google Fonts, usando fuentes del sistema")
      .tabla <<- configurar_fuente_tabla(.tabla, .fuente_letra, .font_size)
    })
  } else {
    # Usar solo fuentes del sistema (RECOMENDADO)
    .tabla <- configurar_fuente_tabla(.tabla, .fuente_letra, .font_size)
  }

  # Añadir título y subtítulo - CORREGIDO para compatibilidad HTML/LaTeX
  if (!is.null(.title)) {
    if (!is.null(.subtitle)) {
      # Detectar formato de salida
      if (exists("knitr") && requireNamespace("knitr", quietly = TRUE)) {
        if (knitr::is_latex_output()) {
          # Para LaTeX/PDF: usar texto plano sin HTML
          .tabla <- .tabla |>
            gt::tab_header(
              title = .title,
              subtitle = .subtitle
            ) |>
            gt::tab_style(
              style = list(
                gt::cell_text(size = gt::px(.title_size), weight = "bold")
              ),
              locations = gt::cells_title(groups = "title")
            ) |>
            gt::tab_style(
              style = list(
                gt::cell_text(size = gt::px(.subtitle_size))
              ),
              locations = gt::cells_title(groups = "subtitle")
            )
        } else {
          # Para HTML: usar HTML con estilos inline
          .tabla <- .tabla |>
            gt::tab_header(
              title = gt::html(paste0("<strong style='font-size:", .title_size, "px'>", .title, "</strong>")),
              subtitle = gt::html(paste0("<span style='font-size:", .subtitle_size, "px'>", .subtitle, "</span>"))
            )
        }
      } else {
        # Si knitr no está disponible, asumir HTML
        .tabla <- .tabla |>
          gt::tab_header(
            title = gt::html(paste0("<strong style='font-size:", .title_size, "px'>", .title, "</strong>")),
            subtitle = gt::html(paste0("<span style='font-size:", .subtitle_size, "px'>", .subtitle, "</span>"))
          )
      }
    } else {
      # Solo título
      if (exists("knitr") && requireNamespace("knitr", quietly = TRUE) && knitr::is_latex_output()) {
        .tabla <- .tabla |>
          gt::tab_header(title = .title) |>
          gt::tab_style(
            style = list(
              gt::cell_text(size = gt::px(.title_size), weight = "bold")
            ),
            locations = gt::cells_title(groups = "title")
          )
      } else {
        .tabla <- .tabla |>
          gt::tab_header(
            title = gt::html(paste0("<strong style='font-size:", .title_size, "px'>", .title, "</strong>"))
          )
      }
    }
  }

  # Añadir nota al pie si se proporciona - CORREGIDO
  if (!is.null(.caption)) {
    # if (exists("knitr") && requireNamespace("knitr", quietly = TRUE) && knitr::is_latex_output()) {
      # Para LaTeX: texto plano
      .tabla <- .tabla |>
        gt::tab_source_note(source_note = .caption) |>
        gt::tab_style(
          style = list(
            gt::cell_text(size = gt::px(.caption_size))
          ),
          locations = gt::cells_source_notes()
        )
    # } else {
    #   # Para HTML: con estilo
    #   .tabla <- .tabla |>
    #     gt::tab_source_note(
    #       source_note = gt::html(paste0("<span style='font-size:", .caption_size, "px'>", .caption, "</span>"))
    #     )
    # }
  }

  # Aplicar agrupaciones si se proporcionan
  if (!is.null(.agrupaciones)) {
    for (i in seq_along(.agrupaciones)) {
      grupo <- .agrupaciones[[i]]
      # Verificar que las columnas existan
      cols_grupo <- intersect(grupo$columns, names(.data))
      if (length(cols_grupo) > 0) {
        .tabla <- .tabla |>
          gt::tab_spanner(
            label = grupo$label,
            columns = gt::all_of(cols_grupo)
          )
      }
    }
  }

  # Estilo de encabezados
  .tabla <- .tabla |>
    gt::tab_style(
      style = list(
        gt::cell_fill(color = .color_encabezado),
        gt::cell_text(color = .color_texto_encabezado, weight = "bold", align = "center")
      ),
      locations = gt::cells_column_labels(columns = gt::everything())
    )

  # Estilo para agrupaciones de columnas (spanners)
  if (!is.null(.agrupaciones)) {
    .tabla <- .tabla |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = .color_encabezado),
          gt::cell_text(color = .color_texto_encabezado, weight = "bold", align = "center")
        ),
        locations = gt::cells_column_spanners(spanners = gt::everything())
      )
  }

  # Primera columna como etiqueta fija (si aplica)
  if (.primera_columna_fija && ncol(.data) > 1) {
    primera_col <- names(.data)[1]
    .tabla <- .tabla |>
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "bold", align = "left"),
          gt::cell_fill(color = .color_filas_alternas)
        ),
        locations = gt::cells_body(columns = gt::all_of(primera_col))
      )
  }

  # Filas alternas
  cols_alternas <- if (.primera_columna_fija && ncol(.data) > 1) {
    names(.data)[-1]
  } else {
    names(.data)
  }

  if (nrow(.data) > 1 && length(cols_alternas) > 0) {
    filas_pares <- seq(2, nrow(.data), by = 2)
    if (length(filas_pares) > 0) {
      .tabla <- .tabla |>
        gt::tab_style(
          style = gt::cell_fill(color = .color_filas_alternas),
          locations = gt::cells_body(
            columns = gt::all_of(cols_alternas),
            rows = filas_pares
          )
        )
    }
  }

  # Resaltar filas específicas
  if (!is.null(.resaltar_filas)) {
    filas_validas <- .resaltar_filas[.resaltar_filas <= nrow(.data) & .resaltar_filas > 0]
    if (length(filas_validas) > 0) {
      .tabla <- .tabla |>
        gt::tab_style(
          style = gt::cell_fill(color = .color_resaltado),
          locations = gt::cells_body(rows = filas_validas)
        )
    }
  }

  # Formateo de columnas numéricas
  if (length(.columnas_numericas) > 0) {
    cols_num_existentes <- intersect(.columnas_numericas, names(.data))

    for (col in cols_num_existentes) {
      es_variacion <- !is.null(.columnas_variacion) && col %in% .columnas_variacion
      es_cuota <- !is.null(.columnas_cuota) && col %in% .columnas_cuota

      if (es_variacion) {
        valores_col <- .data[[col]]
        valores_no_na <- valores_col[!is.na(valores_col)]

        if (length(valores_no_na) > 0) {
          .tabla <- .tabla |>
            gt::fmt_percent(
              columns = gt::all_of(col),
              decimals = .precision_decimales,
              dec_mark = .formato_decimal,
              sep_mark = .formato_miles,
              force_sign = .forzar_signo_variacion
            )
        }
      } else if (es_cuota) {
        valores_col <- .data[[col]]
        valores_no_na <- valores_col[!is.na(valores_col)]

        if (length(valores_no_na) > 0) {
          .tabla <- .tabla |>
            gt::fmt_percent(
              columns = gt::all_of(col),
              decimals = .precision_decimales,
              dec_mark = .formato_decimal,
              sep_mark = .formato_miles,
              force_sign = FALSE
            )
        }
      } else {
        .tabla <- .tabla |>
          gt::fmt_number(
            columns = gt::all_of(col),
            decimals = .precision_decimales,
            dec_mark = .formato_decimal,
            sep_mark = .formato_miles
          )
      }
    }
  }

  # Centrar columnas especificadas
  if (length(.centrar_columnas) > 0) {
    cols_existentes <- intersect(.centrar_columnas, names(.data))
    if (length(cols_existentes) > 0) {
      .tabla <- .tabla |>
        gt::cols_align(
          align = "center",
          columns = gt::all_of(cols_existentes)
        )
    }
  }

  # Aplicar colores para valores positivos y negativos
  if (!is.null(.columnas_colores_positivo_negativo)) {
    cols_colores <- intersect(.columnas_colores_positivo_negativo, names(.data))

    for (col in cols_colores) {
      if (col %in% names(.data)) {
        valores <- .data[[col]]

        if (any(!is.na(valores))) {
          filas_negativas <- which(!is.na(valores) & valores < 0)
          if (length(filas_negativas) > 0) {
            .tabla <- .tabla |>
              gt::tab_style(
                style = list(
                  gt::cell_fill(color = .color_fondo_negativo),
                  gt::cell_text(color = .color_negativo, weight = "bold")
                ),
                locations = gt::cells_body(
                  columns = gt::all_of(col),
                  rows = filas_negativas
                )
              )
          }

          filas_positivas <- which(!is.na(valores) & valores > 0)
          if (length(filas_positivas) > 0) {
            .tabla <- .tabla |>
              gt::tab_style(
                style = list(
                  gt::cell_fill(color = .color_fondo_positivo),
                  gt::cell_text(color = .color_positivo, weight = "bold")
                ),
                locations = gt::cells_body(
                  columns = gt::all_of(col),
                  rows = filas_positivas
                )
              )
          }
        }
      }
    }
  }

  # Añadir separadores verticales
  if (!is.null(.columnas_separador_derecha)) {
    cols_separador <- intersect(.columnas_separador_derecha, names(.data))

    for (col in cols_separador) {
      .tabla <- .tabla |>
        gt::tab_style(
          style = gt::cell_borders(
            sides = "right",
            color = .color_borde_encabezado,
            weight = gt::px(2),
            style = "solid"
          ),
          locations = gt::cells_body(columns = gt::all_of(col))
        ) |>
        gt::tab_style(
          style = gt::cell_borders(
            sides = "right",
            color = .color_borde_encabezado,
            weight = gt::px(2),
            style = "solid"
          ),
          locations = gt::cells_column_labels(columns = gt::all_of(col))
        )
    }
  }

  return(.tabla)
}

# Función de alias - mantener compatibilidad con el nombre sin 's'
#' @export
tabla_estilo_victorgm <- tablas_estilo_victorgm

#' @export
configurar_fuente_tabla <- function(.tabla, .fuente_letra, .font_size) {
  # Stack de fuentes fallback según el sistema
  fuentes_sistema <- list(
    "Segoe UI" = list("Segoe UI", "Tahoma", "Geneva", "Verdana", "sans-serif"),
    "Arial" = list("Arial", "Helvetica", "sans-serif"),
    "Times" = list("Times New Roman", "Times", "serif"),
    "Courier" = list("Courier New", "Courier", "monospace"),
    "Helvetica" = list("Helvetica", "Arial", "sans-serif")
  )

  # Obtener el stack de fuentes o usar genérico
  font_stack <- if (.fuente_letra %in% names(fuentes_sistema)) {
    fuentes_sistema[[.fuente_letra]]
  } else {
    list(.fuente_letra, "system-ui", "sans-serif")
  }

  # Configurar fuente sin Google Fonts
  .tabla |>
    gt::opt_table_font(
      font = font_stack,
      size = paste0(.font_size, "px")
    )
}

# Función auxiliar para crear agrupaciones de columnas fácilmente
#' Crear agrupación de columnas para tabla gt
#'
#' @param label Etiqueta de la agrupación
#' @param columns Vector de nombres de columnas a agrupar
#' @return Lista con label y columns
#' @export
crear_agrupacion <- function(label, columns) {
  if (!is.character(label) || length(label) != 1) {
    stop("El argumento 'label' debe ser una cadena de caracteres")
  }
  if (!is.character(columns) || length(columns) == 0) {
    stop("El argumento 'columns' debe ser un vector de caracteres no vacío")
  }
  return(list(label = label, columns = columns))
}
