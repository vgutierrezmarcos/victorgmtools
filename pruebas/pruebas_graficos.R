

bdeseries::get_catalog() |>
  dplyr::filter(frecuencia == "ANUAL") |>
  dplyr::filter(fuente != "Banco de España") |> 
  View()


codigos_descarga <- 	
  c(
    "D_AKR620PA",
    "D_AKR620PC",
    "D_AKR620PP",
    "D_AKB07000",
    "D_AKB07300",
    "D_AKB07100",
    "D_AKB07200"
  )

datos_df <- 
  bdeseries::get_series(codes = codigos_descarga)

datos_procesados_df <- 
  datos_df |> 
  dplyr::filter(
    unidades == "Porcentaje",
    as.Date(fecha) >= as.Date("2000-01-01")
  ) |> 
  dplyr::mutate(
    fecha = lubridate::floor_date(fecha, "years"), # Asegurar que fecha es Date
    valores = valores * 1e-2,
    nombres_cortos = dplyr::if_else(nchar(nombres) > 70, stringr::str_sub(nombres, 70, 300), nombres),
    nombres_muy_cortos = dplyr::if_else(nchar(nombres) > 116, stringr::str_sub(nombres, 116, 300), nombres)
  )


# Data Processing with Order for Interactive Area
datos_grafico <- 
  datos_procesados_df |> 
  dplyr::mutate(
    # Create ordering variable: Propiedad (1-Bottom), Alquiler (2), Cesión (3-Top)
    orden = dplyr::case_when(
      grepl("cesi", nombres, ignore.case = TRUE) ~ 1,
      grepl("alquiler", nombres, ignore.case = TRUE) ~ 2,
      grepl("propiedad", nombres, ignore.case = TRUE) ~ 3,
      TRUE ~ 0
    )
  ) |>
  # Arrange by order to fix factor levels
  dplyr::arrange(orden) |> 
  dplyr::mutate(
    nombres_muy_cortos = factor(nombres_muy_cortos, levels = unique(nombres_muy_cortos))
  )


paleta <- 
  c(
    "Propiedad" = victorgmtools::colores_victorgm()[1],
    "Alquiler" = victorgmtools::colores_victorgm()[3],
    "Cesión" = victorgmtools::colores_victorgm()[2]
  )
# Dates for labels and limits
fechas_unicas <- sort(unique(datos_grafico$fecha))
fecha_etiqueta <- fechas_unicas[round(length(fechas_unicas) / 2)]
fecha_max <- max(datos_grafico$fecha, na.rm = TRUE)
fecha_min <- min(datos_grafico$fecha, na.rm = TRUE)

viviendas_plt <- 
  ggplot2::ggplot(
    data = datos_grafico,
    mapping = ggplot2::aes(
      x = fecha,
      y = valores,
      fill = nombres_muy_cortos,
      group = nombres_muy_cortos,
      data_id = nombres_muy_cortos
    )
  ) +
  # Visual + Interaction Layer
  ggiraph::geom_area_interactive(
    alpha = 0.8,
    color = "white",
    linewidth = 0.2
  ) +
  ggiraph::geom_point_interactive(
    mapping = ggplot2::aes(
      tooltip = paste0(
        "<b>Año</b>: ", fecha |> format("%Y"), "\n",
        "<b>", nombres_cortos, "</b>: ", scales::number_format(big.mark = ".", decimal.mark = ",", accuracy = 0.1, scale = 1e2, suffix = "%")(valores)
      )
    ),
    position = ggplot2::position_stack(vjust = 0.5),
    alpha = 0,
    size = 5
  ) +
  # 3. Label Layer
  ggplot2::geom_text(
    data = datos_grafico |> dplyr::filter(fecha == fecha_etiqueta),
    mapping = ggplot2::aes(
      label = nombres_muy_cortos
    ),
    position = ggplot2::position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3,
    check_overlap = TRUE
  )


viviendas_plt |> 
  graficos_estilo_victorgm(
    .tipo_grafico_x = "fecha",
    .tipo_grafico_y = "porcentaje",
    .title = "Régimen de tenencia de viviendas en España.",
    .subtitle = "Porcentaje.",
    .caption = "Fuente: Censos de viviendas (Instituto Nacional de Estadística).",
    .fuente_letra = "Source Sans 3",
    .logo_path = "C:/Users/vgutierrez/Documents/src/victorgutierrezmarcos.github.io/blog/images/icon.png",
    .logo_hover_path = "C:/Users/vgutierrez/Documents/src/victorgutierrezmarcos.github.io/blog/images/icon_inverse.png",
    .legend_position = "none",
    .colores_linea = c("#5F2987", "#B8860B", "#E2EFD9"),
    .fecha_inicial_grafico = fecha_min,
    .fecha_final_grafico = fecha_max,
    .paleta_utilizada = paleta,
    .logo_ancho_pct = 10
  )


